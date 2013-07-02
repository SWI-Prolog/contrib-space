/*  $Id$

    Author:        Willem Robert van Hage
    E-mail:        wrvhage@few.vu.nl
    WWW:           http://www.few.vu.nl/~wrvhage
    Copyright (C): 2009, Vrije Universiteit Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

/*
 * TODO: improve error message handling
 * TODO: add uri_shape(U,S,Source) from index
 */

#include "Index.h"


		 /*******************************
		 *	       LOCKING		*
		 *******************************/

#define RDLOCK(lock)			rdlock(lock)
#define WRLOCK(lock, allowreaders)	wrlock(lock, allowreaders)
#define LOCKOUT_READERS(lock)		lockout_readers(lock)
#define REALLOW_READERS(lock)		reallow_readers(lock)
#define WRUNLOCK(lock)			unlock(lock, FALSE)
#define RDUNLOCK(lock)			unlock(lock, TRUE)
#define LOCK_MISC(lock)			lock_misc(lock)
#define UNLOCK_MISC(lock)		unlock_misc(lock)
#define INIT_LOCK(lock)			init_lock(lock)


/*
 * -- DataStream --------------------------------------------
 */

class RTreePrologStream : public IDataStream
{
public:
  qid_t q;
  predicate_t p;
  term_t cache0;
  bool cached;
  RTreeIndex *index;
public:
  RTreePrologStream(term_t goal,RTreeIndex* i) {
    cache0 = PL_new_term_refs(3);
    p = PL_predicate("call",3,"system");
    cached = false;
    PL_put_term(cache0+0,goal);
    q = PL_open_query(NULL, PL_Q_NORMAL, p, cache0);
    index = i;
    index->bulkload_tmp_id_cnt = 0;
  }
  ~RTreePrologStream() {
    PL_cut_query(q);
  }
  virtual IData* getNext() {
    RTree::Data* rv = NULL;
    if ( !WRLOCK(&index->lock, FALSE) ) {
      cerr << __FUNCTION__ << " could not acquire write lock" << endl;
      return NULL;
    }
    if (!cached && !PL_next_solution(q)) {
      rv = NULL;
    } else {
      cached = false;
      term_t shape_term = PL_new_term_ref();
      shape_term = (term_t)(cache0+2);
      IShape *s = index->interpret_shape(shape_term);
      Region r;
      s->getMBR(r);
      PlTerm uri_term((term_t)cache0+1);
      PlAtom uri_atom(uri_term);
      id_type id = index->get_new_id(uri_term);
      index->storeShape(id,s,PlTerm(shape_term));
#ifdef DEBUGGING
      cout << "uri " << (char*)uri_term << " atom " << uri_atom.handle << " shape " << r << " id " << id << endl;
#endif
       rv = new RTree::Data(sizeof(uri_atom.handle), (byte*)&uri_atom.handle, r, id);
    }
    WRUNLOCK(&index->lock);
    return rv;
  }
  virtual bool hasNext() throw (Tools::NotSupportedException)
  {
    if (cached) return true;
    if (PL_next_solution(q)) {
      cached = true;
      return true;
    }
    cached = false;
    return false;
  }

  virtual uint32_t size() throw (Tools::NotSupportedException)
  {
    throw Tools::NotSupportedException("Operation not supported.");
    return 0;
  }

  virtual void rewind() throw (Tools::NotSupportedException)
  {
    cerr << "rewinding a RTreePrologStream does nothing" << endl;
  }
};



/*
 * -- RTreeIndex -------------------------------------
 */


RTreeIndex::RTreeIndex(PlTerm indexname) :  storage(MEMORY), distance_function(PYTHAGOREAN), baseName(indexname), utilization(0.7), nodesize(4),  storage_manager(NULL), buffer(NULL), tree(NULL) {
  bulkload_tmp_id_cnt = -1;
  INIT_LOCK(&lock);
  PL_register_atom(PlAtom(indexname).handle);
}

RTreeIndex::RTreeIndex(PlTerm indexname, double util, int nodesz) : storage(MEMORY), distance_function(PYTHAGOREAN), baseName(indexname), storage_manager(NULL), buffer(NULL), tree(NULL) {
  utilization = util;
  nodesize = nodesz;
  bulkload_tmp_id_cnt = -1;
  PL_register_atom(PlAtom(indexname).handle);
  INIT_LOCK(&lock);
}

RTreeIndex::~RTreeIndex() {
  if ( !WRLOCK(&lock,FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire write lock" << endl;
    return;
  }
  this->clear_tree();
  PL_unregister_atom(PlAtom(baseName).handle);
  WRUNLOCK(&lock);
}

void RTreeIndex::clear_tree() {
  if ( !WRLOCK(&lock,FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire write lock" << endl;
    return;
  }
  if (tree != NULL) {
    delete tree;
    tree = NULL;
  }
  if (buffer != NULL) {
    delete buffer;
    buffer = NULL;
  }
  if(storage_manager != NULL) {
    delete storage_manager;
    storage_manager = NULL;
  }
  uri_id_multimap.clear();
  map<id_type,pair<IShape*,record_t> >::iterator id_shape_iter;
  for( id_shape_iter = id_shape_map.begin(); id_shape_iter != id_shape_map.end(); ++id_shape_iter ) {
    GEOSShape *s = dynamic_cast<GEOSShape*>(id_shape_iter->second.first);
    if (s != NULL) {
      global_factory->destroyGeometry(s->g);
      s->g = NULL;
    }
    delete id_shape_iter->second.first;
    PL_erase(id_shape_iter->second.second);
  }
  id_shape_map.clear();
  WRUNLOCK(&lock);
}

void RTreeIndex::storeShape(id_type id,IShape *s,PlTerm t) {
  if ( !WRLOCK(&lock,FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire write lock" << endl;
    return;
  }
  id_shape_map[id] = pair<IShape*,record_t>(s,PL_record(t));
  WRUNLOCK(&lock);
}

IShape* RTreeIndex::getShape(id_type id) {
  IShape *rv = NULL;
  if ( !RDLOCK(&lock) ) {
    cerr << __FUNCTION__ << " could not acquire read lock" << endl;
    return NULL;
  }
  map<id_type,pair<IShape*,record_t> >::iterator iter = id_shape_map.find(id);
  if (iter == id_shape_map.end()) {
    rv = NULL;
  } else {
    rv = iter->second.first;
  }
  RDUNLOCK(&lock);
  return rv;
}

// FIXME: obsolete function, replaced by code in delete_single_object
void RTreeIndex::deleteShape(id_type id) {
  if ( !WRLOCK(&lock,FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire read lock" << endl;
    return;
  }
  map<id_type,pair<IShape*,record_t> >::iterator shape_iter = id_shape_map.find(id);
  if (shape_iter != id_shape_map.end()) {
    GEOSShape *s = dynamic_cast<GEOSShape*>(shape_iter->second.first);
    if (s != NULL) {
      global_factory->destroyGeometry(s->g);
      s->g = NULL;
    }
    delete shape_iter->second.first;
    PL_erase(shape_iter->second.second);
  }
  id_shape_map.erase(id);
  WRUNLOCK(&lock);
}

bool RTreeIndex::getShapeTerm(id_type id, term_t t) {
  bool rv = false;
  if ( !RDLOCK(&lock) ) {
    cerr << __FUNCTION__ << " could not acquire read lock" << endl;
    rv = false;
  }
  map<id_type,pair<IShape*,record_t> >::iterator iter = id_shape_map.find(id);
  if (iter == id_shape_map.end()) {
    rv = false;
  } else {
    rv = PL_recorded(iter->second.second,t);
  }
  RDUNLOCK(&lock);
  return rv;
}

id_type  RTreeIndex::get_new_id(PlTerm uri) {
  id_type id = -1;
  if ( !WRLOCK(&lock, FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire write lock" << endl;
    return (id_type)0;
  }
  PlAtom uri_atom(uri);
  PL_register_atom(uri_atom.handle); // FIXME: unregister somewhere...
  if (bulkload_tmp_id_cnt != -1) { // we're bulkloading
    id = bulkload_tmp_id_cnt++;
    uri_id_multimap.insert(pair<atom_t,id_type>(uri_atom.handle,id));
  } else {
    if (tree == NULL) {
      id = -1;
    } else {
      IStatistics* stats;
      tree->getStatistics(&stats);
      id = stats->getNumberOfData();
      uri_id_multimap.insert(pair<atom_t,id_type>(uri_atom.handle,id));
      delete stats;
    }
  }
  WRUNLOCK(&lock);
  return id;
}

void RTreeIndex::create_tree(uint32_t dimensionality) {
  if ( !WRLOCK(&lock,FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire write lock" << endl;
    return;
  }
  RTreeIndex::create_tree(dimensionality,utilization,nodesize);
  WRUNLOCK(&lock);
}
void RTreeIndex::create_tree(uint32_t dimensionality, double util, int nodesz) {
  if ( !WRLOCK(&lock,FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire write lock" << endl;
    return;
  }
  if (tree != NULL) clear_tree();
  utilization = util;
  nodesize = nodesz;
  PlTerm bnt(baseName);
  if (storage == MEMORY) {
    storage_manager = StorageManager::createNewMemoryStorageManager();
  } else if (storage == DISK) {
    string *bns = new string((char*)bnt);
    storage_manager = StorageManager::createNewDiskStorageManager(*bns, 32);
    delete bns;
  }
  buffer = StorageManager::createNewRandomEvictionsBuffer(*storage_manager, 4096, false);
  tree = RTree::createNewRTree(*buffer, utilization,
                               nodesize, nodesize, dimensionality,
                               SpatialIndex::RTree::RV_RSTAR, indexIdentifier);
  WRUNLOCK(&lock);
}


bool
RTreeIndex::bulk_load(PlTerm goal,uint32_t dimensionality) {
  if (dimensionality > 3 || dimensionality < 1) {
    cerr << "only dimensionality from 1 to 3 supported, not " << dimensionality << endl;
    return false;
  }
  RTreePrologStream stream(goal,this); // assuming goal of the form 'somepred(URI,Shape)'

  if ( !WRLOCK(&lock,FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire write lock" << endl;
    return false;
  }
  if (tree != NULL) clear_tree();
  if (storage == MEMORY) {
    storage_manager = StorageManager::createNewMemoryStorageManager();
  } else if (storage == DISK) {
    string *bns = new string((const char*)baseName);
    storage_manager = StorageManager::createNewDiskStorageManager(*bns, 32);
    delete bns;
  }
  buffer = StorageManager::createNewRandomEvictionsBuffer(*storage_manager, 4096, false);
  id_type indexIdentifier;
  tree = RTree::createAndBulkLoadNewRTree(RTree::BLM_STR,
                                          stream, *buffer, utilization,
                                          nodesize, nodesize, dimensionality,
                                          SpatialIndex::RTree::RV_RSTAR, indexIdentifier);
  WRUNLOCK(&lock);
  if ( !RDLOCK(&lock) ) {
    return FALSE;
  }
  bool rv = tree->isIndexValid();
  RDUNLOCK(&lock);
  return rv;
}

IShape* RTreeIndex::interpret_shape(PlTerm shape_term) {
  if (shape_term.name() == ATOM_point) {

    double *point = new double[shape_term.arity()];
    for (int i = 1; i <= shape_term.arity(); i++) {
      point[i-1] = (double)shape_term[i];
    }
    GEOSPoint *p = new GEOSPoint(point,shape_term.arity()); // testing GEOS points
    #ifdef DEBUGGING
    cout << "made point " << p << endl;
    #endif
    return p;


  } else if (shape_term.name() == ATOM_linestring) {

    if (shape_term.arity() != 1) {
      cout << "arity not 1: linestring must have one argument, a list containing a list of points" << endl;
      return NULL;
    } else if (!PL_is_list(shape_term[1])) {
      cout << "first argument not a list: linestring must have one argument, a list containing a list of points" << endl;
      return NULL;
    }
    geos::geom::CoordinateSequence *cl = new geos::geom::CoordinateArraySequence();
    PlTail list(shape_term[1]);
    PlTerm pt;
    while (list.next(pt)) {
      if (pt.name() != ATOM_point) {
        cerr << "linestring contains non-point" << endl;
        return NULL;
      }
      uint32_t dim = pt.arity();
      if (dim == 2) {
        cl->add(geos::geom::Coordinate((double)pt[1],(double)pt[2]));
      } else if (dim == 1) {
        cl->add(geos::geom::Coordinate((double)pt[1]));
      } else if (dim == 3) {
        cl->add(geos::geom::Coordinate((double)pt[1],(double)pt[2],(double)pt[3]));
      } else {
        cerr << dim << " dimensional points not supported" << endl;
      }
    }
    geos::geom::LineString *ls = global_factory->createLineString(*cl);
    GEOSLineString *linestring = new GEOSLineString(*ls);
    delete ls;
    delete cl;
    return linestring;

  } else if (shape_term.name() == ATOM_polygon) {

    if (shape_term.arity() != 1) {
      cout << "arity not 1: polygon must have one argument, a list containing a list of points representing the shell, and an second argument, a list containing lists of points representing the holes" << endl;
      return NULL;
    } else if (!PL_is_list(shape_term[1])) {
      cout << "first argument not a list: polygon must have one argument, a list containing a list of points representing the shell, and an second argument, a list containing lists of points representing the holes" << endl;
      return NULL;
    }
    geos::geom::CoordinateSequence *cl = new geos::geom::CoordinateArraySequence();
    PlTail linearrings(shape_term[1]);
    PlTerm ring;
    linearrings.next(ring);
    PlTail shell(ring);
    PlTerm pt;
    while (shell.next(pt)) {
      if (pt.name() != ATOM_point) {
        cerr << "polygon shell contains non-point" << endl;
        return NULL;
      }
      uint32_t dim = pt.arity();
      if (dim == 2) {
        cl->add(geos::geom::Coordinate((double)pt[1],(double)pt[2]));
      } else if (dim == 1) {
        cl->add(geos::geom::Coordinate((double)pt[1]));
      } else if (dim == 3) {
        cl->add(geos::geom::Coordinate((double)pt[1],(double)pt[2],(double)pt[3]));
      } else {
        cerr << dim << " dimensional points not supported" << endl;
      }
    }
    // assuming linear ring is already closed
    //    cl->add(cl->getAt(0));
    geos::geom::LinearRing *lr = global_factory->createLinearRing(*cl);
    vector<geos::geom::Geometry*> *holes = new vector<geos::geom::Geometry*>;
    while (linearrings.next(ring)) {
      geos::geom::CoordinateSequence *hcl = new geos::geom::CoordinateArraySequence();
      PlTail hole(ring);
      PlTerm hpt;
      while (hole.next(hpt)) {
        if (hpt.name() != ATOM_point) {
          cerr << "polygon hole contains non-point" << endl;
          return NULL;
        }
        uint32_t dim = hpt.arity();
        if (dim == 2) {
          hcl->add(geos::geom::Coordinate((double)hpt[1],(double)hpt[2]));
        } else if (dim == 1) {
          hcl->add(geos::geom::Coordinate((double)hpt[1]));
        } else if (dim == 3) {
          hcl->add(geos::geom::Coordinate((double)hpt[1],(double)hpt[2],(double)hpt[3]));
        } else {
          cerr << dim << " dimensional points not supported" << endl;
        }
      }
      // assuming linear ring is already closed
      //      hcl->add(cl->getAt(0));
      geos::geom::LinearRing *hlr = global_factory->createLinearRing(hcl);
      holes->push_back(hlr);
    }
    geos::geom::Polygon *p = global_factory->createPolygon(lr,holes);
    GEOSPolygon *poly = new GEOSPolygon(*p);
    delete p;
    delete cl;
    return poly;

  } else if (shape_term.name() == ATOM_box) {

    #ifdef DEBUGGING
    cout << "reading box" << endl;
    #endif
    if (shape_term.arity() != 2) {
      cerr << "MBR dimensionality must be 2 (low and high), but is " << shape_term.arity() << endl;
      return NULL;
    }
    int dim = shape_term.arity();
    double *low_point = new double[dim];
    double *high_point = new double[dim];
    if (shape_term[1].name() != ATOM_point) return NULL;
    for (int i = 1; i <= dim; i++) {
      low_point[i-1] = (double)shape_term[1][i];
    }
    if (shape_term[2].name() != ATOM_point) return NULL;
    for (int i = 1; i <= dim; i++) {
      high_point[i-1] = (double)shape_term[2][i];
    }

    geos::geom::Polygon *box;
    if (dim == 2) {
      geos::geom::CoordinateSequence *cl = new geos::geom::CoordinateArraySequence();
      cl->add(geos::geom::Coordinate(low_point[0], low_point[1]));
      cl->add(geos::geom::Coordinate(low_point[0], high_point[1]));
      cl->add(geos::geom::Coordinate(high_point[0], high_point[1]));
      cl->add(geos::geom::Coordinate(high_point[0], low_point[1]));
      cl->add(geos::geom::Coordinate(low_point[0], low_point[1]));
      geos::geom::LinearRing *lr = global_factory->createLinearRing(*cl);
      box = global_factory->createPolygon(lr, NULL);
      box->normalize();
      delete cl;
    } else if (dim == 1) {
      geos::geom::CoordinateSequence *cl = new geos::geom::CoordinateArraySequence();
      cl->add(geos::geom::Coordinate(low_point[0]));
      cl->add(geos::geom::Coordinate(high_point[0]));
      geos::geom::LinearRing *lr = global_factory->createLinearRing(*cl);
      box = global_factory->createPolygon(lr, NULL);
      box->normalize();
      delete cl;
    } else if (dim == 3) {
      cerr << "3d box regions not implemented yet" << endl;
      return NULL;
    } else {
      cerr << dim << " dimensional regions not supported" << endl;
      return NULL;
    }
    GEOSPolygon *poly = new GEOSPolygon(*box);
    global_factory->destroyGeometry(box);
    return poly;
  } else {
    cerr << "shape type \"" << (char*)shape_term.name() << "\" unsupported" << endl;
    return NULL;
  }
  return NULL;
}

bool RTreeIndex::insert_single_object(PlTerm uri,PlTerm shape_term) {
  IShape *shape;
  id_type id;
  if ( !WRLOCK(&lock,FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire write lock" << endl;
    return FALSE;
  }
  if ((shape = RTreeIndex::interpret_shape(shape_term)) == NULL) {
    cerr << "could not interpret shape" << endl;
    WRUNLOCK(&lock);
    return FALSE;
  }
  if (tree == NULL) {
    uint32_t dimensionality = shape->getDimension();
    clear_tree();
    RTreeIndex::create_tree(dimensionality,utilization,nodesize);
  }
  if ((id = get_new_id(uri)) == -1) {
    cerr << "could not generate new ID" << endl;
    WRUNLOCK(&lock);
    return FALSE;
  }
  try {
    PlAtom uri_atom(uri);
    tree->insertData(sizeof(uri_atom.handle), (byte*)&uri_atom.handle, *shape, id);
  } catch (...) {
    WRUNLOCK(&lock);
    return FALSE;
  }
  storeShape(id,shape,shape_term);
  WRUNLOCK(&lock);
  return TRUE;
}

bool RTreeIndex::delete_single_object(PlTerm uri,PlTerm shape_term) {
  bool rv = TRUE;
  if (tree == NULL) {
    return TRUE;
  }
  if ( !WRLOCK(&lock,FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire write lock" << endl;
    return FALSE;
  }
  IteratorState *state = new IteratorState();
  state->uri_id_range = uri_id_multimap.equal_range(PlAtom(uri).handle);
  for ( state->uri_id_iter = state->uri_id_range.first;
        state->uri_id_iter != state->uri_id_range.second;
        ++(state->uri_id_iter)) {
    id_type id = state->uri_id_iter->second;
    map<id_type,pair<IShape*,record_t> >::iterator id_shape_iter = id_shape_map.find(id);
    if (id_shape_iter != id_shape_map.end()) {
      term_t t = PL_new_term_ref();
      if (!PL_recorded(id_shape_iter->second.second,t)) {
        cerr << __FUNCTION__ << " couldn't resolve shape term record" << endl;
        return FALSE;
      }
      PlTerm st = PlTerm(t);
      if (st == shape_term) {
        // FIXME: improve error handling. rv should be dependent on all frees
        uri_id_multimap.erase(state->uri_id_iter);
	IShape *shape = (*id_shape_iter).second.first;
	rv = tree->deleteData(*shape, id);
        //deleteShape(id);
        GEOSShape *s = dynamic_cast<GEOSShape*>(shape);
        if (s != NULL) {
          global_factory->destroyGeometry(s->g);
          s->g = NULL;
        }
        delete shape;
        PL_erase(id_shape_iter->second.second);
        id_shape_map.erase(id);
        break;
      }
    }
  }
  delete state;
  WRUNLOCK(&lock);
  return rv;
}

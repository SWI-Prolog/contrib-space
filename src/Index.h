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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

#ifndef __INDEX_H
#define __INDEX_H

#include <SWI-cpp.h>
#include "globals.h"
#include "Shapes.h"
#include "lock.h"


typedef enum {
  PYTHAGOREAN,
  HAVERSINE
} distance_function_t;

typedef enum {
  MEMORY,
  DISK
} storage_t;

class IteratorState {
  public:
    pair<multimap<atom_t,id_type>::iterator,
         multimap<atom_t,id_type>::iterator> uri_id_range;
    multimap<atom_t,id_type>::iterator uri_id_iter;
};

using namespace std;
using namespace SpatialIndex;


class Index
{
 public:
  storage_t storage;
  distance_function_t distance_function;

  virtual ~Index() {};

  virtual id_type get_new_id(PlTerm uri) = 0;
  virtual void storeShape(id_type id,IShape *s,PlTerm shape_term) = 0;
  virtual IShape* getShape(id_type id) = 0;
  virtual bool getShapeTerm(id_type id,term_t t) = 0;
  virtual void deleteShape(id_type id) = 0;

  virtual IShape* interpret_shape(PlTerm shape_term) = 0;
  virtual void clear_tree() = 0;
  virtual void create_tree(uint32_t dimensionality) = 0;
  virtual void create_tree(uint32_t dimensionality, double util, int nodesz) = 0;
  virtual bool insert_single_object(PlTerm uri,PlTerm shape_term) = 0;
  virtual bool delete_single_object(PlTerm uri,PlTerm shape_term) = 0;

 private:
  rwlock lock;

};


class RTreeIndex : public Index
{
 public:
  storage_t storage;
  distance_function_t distance_function;

  PlAtom baseName;
  double utilization;
  int nodesize;
  IStorageManager* storage_manager;
  StorageManager::IBuffer* buffer;
  ISpatialIndex* tree;
  id_type indexIdentifier;
  multimap<atom_t,id_type> uri_id_multimap;
  map<id_type,pair<IShape*,record_t> > id_shape_map;

  RTreeIndex(PlTerm indexname);
  RTreeIndex(PlTerm indexname, double util, int nodesz);
  virtual ~RTreeIndex();

  virtual id_type get_new_id(PlTerm uri);
  virtual void storeShape(id_type id,IShape *s,PlTerm t);
  virtual IShape* getShape(id_type id);
  virtual bool getShapeTerm(id_type id,term_t t);
  virtual void deleteShape(id_type id);

  virtual IShape* interpret_shape(PlTerm shape_term);
  virtual bool bulk_load(PlTerm goal,uint32_t dimensionality);
  virtual void clear_tree();
  virtual void create_tree(uint32_t dimensionality);
  virtual void create_tree(uint32_t dimensionality, double util, int nodesz);
  virtual bool insert_single_object(PlTerm uri,PlTerm shape_term);
  virtual bool delete_single_object(PlTerm uri,PlTerm shape_term);
  
 public:
  id_type bulkload_tmp_id_cnt;

 public:
  rwlock lock;


};

#endif // __INDEX_H

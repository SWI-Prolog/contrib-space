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


#include "Search.h"

//class PrintVisitor : public IVisitor

void PrintVisitor::visitNode(const INode& n) {
  printf("visiting node %d (level %d)\n",(int)n.getIdentifier(),(int)n.getLevel());
}

void PrintVisitor::visitData(const IData& d) {
  byte* pData = 0;
  size_t cLen = 0;
  d.getData(cLen, &pData);
  printf("visiting data at %d ",(int)d.getIdentifier());
  printf("length %d ",(int)cLen);
  printf("[%s]\n",(char*)pData);
}

void PrintVisitor::visitData(std::vector<const IData*>& v) {};



// class PrintGnuplotVisitor : public IVisitor
void PrintGnuplotVisitor::visitNode(const INode& n) {
  IShape* ps;
  n.getShape(&ps);
  Region r;
  ps->getMBR(r);
  
  // print node MBRs gnuplot style!
  cout << r.m_pLow[0] << " " << r.m_pLow[1] << endl;
  cout << r.m_pHigh[0] << " " << r.m_pLow[1] << endl;
  cout << r.m_pHigh[0] << " " << r.m_pHigh[1] << endl;
  cout << r.m_pLow[0] << " " << r.m_pHigh[1] << endl;
  cout << r.m_pLow[0] << " " << r.m_pLow[1] << endl << endl << endl;
  
  delete ps;
}

// ignore data, only show index MBRs
void PrintGnuplotVisitor::visitData(const IData& d) {};
void PrintGnuplotVisitor::visitData(std::vector<const IData*>& v) {};
    

// class TraverseBreadthFirst : public SpatialIndex::IQueryStrategy
TraverseBreadthFirst::TraverseBreadthFirst(IVisitor *vis) {
  v = vis;
}

void TraverseBreadthFirst::getNextEntry(const IEntry& entry, id_type& nextEntry, bool& hasNext) {    
  const INode* n = dynamic_cast<const INode*>(&entry);
  v->visitNode(*n);
  
  if (n != 0 && n->getLevel() > 0) {
    for (size_t cChild = 0; cChild < n->getChildrenCount(); cChild++) {
      ids.push(n->getChildIdentifier(cChild));
    }
  } else if (n != 0 && n->getLevel() == 0) {      
    for (size_t cChild = 0; cChild < n->getChildrenCount(); cChild++) {
      IShape* childShape;
      n->getChildShape(cChild,&childShape);
      size_t length;
      byte* data;
      n->getChildData(cChild,length,&data);
      Region childMBR;
      childShape->getMBR(childMBR);
      id_type childIdentifier = n->getChildIdentifier(cChild);
      RTree::Data* e = new RTree::Data(length,data,childMBR,childIdentifier);
      v->visitData(*e);
      delete e;
    }
  }
  
  if (! ids.empty()) {
    nextEntry = ids.front(); ids.pop();
    hasNext = true;
  } else {
    hasNext = false;
  }
}


//class TraverseDepthFirst : public SpatialIndex::IQueryStrategy

TraverseDepthFirst::TraverseDepthFirst(IVisitor *vis) {
  v = vis;
}

void TraverseDepthFirst::getNextEntry(const IEntry& entry, id_type& nextEntry, bool& hasNext) {    
  const INode* n = dynamic_cast<const INode*>(&entry);
  v->visitNode(*n);
  
  if (n != 0 && n->getLevel() > 0) {
    for (size_t cChild = 0; cChild < n->getChildrenCount(); cChild++) {
      ids.push(n->getChildIdentifier(cChild));
    }
  } else if (n != 0 && n->getLevel() == 0) {      
    for (size_t cChild = 0; cChild < n->getChildrenCount(); cChild++) {
      IShape* childShape;
      n->getChildShape(cChild,&childShape);
      size_t length;
      byte* data;
      n->getChildData(cChild,length,&data);
      Region childMBR;
      childShape->getMBR(childMBR);
      id_type childIdentifier = n->getChildIdentifier(cChild);
      RTree::Data* e = new RTree::Data(length,data,childMBR,childIdentifier);
      v->visitData(*e);
      delete e;
    }
  }
  
  if (! ids.empty()) {
    nextEntry = ids.top(); ids.pop();
    hasNext = true;
  } else {
    hasNext = false;
  }
}


// class IncrementalRangeStrategy : public SpatialIndex::IQueryStrategy

IncrementalRangeStrategy::IncrementalRangeStrategy(RangeQueryType type,const IShape* queryp,IVisitor* vp,Index* idx) 
  : result(NULL), result_length(0), continuation(false), child_idx(0) {
  query = queryp;
  v = vp;
  t = type;
  index = idx;
}
IncrementalRangeStrategy::~IncrementalRangeStrategy() {
  if (result != NULL) {
    delete result;
    result = NULL;
  }
}

void IncrementalRangeStrategy::getNextEntry(const IEntry& entry, id_type& nextEntry, bool& hasNext) {
  // This run we have not fetched a result yet, so the value is NULL.
  if (result != NULL) {
    delete result;
    result = NULL;
    result_length = 0;
  }
#ifdef DEBUG
  cout << "visiting node " << entry.getIdentifier() << endl;
#endif
  
  /*
   * IQueryStrategy.queryStrategy always starts at the root node, for every call,
   * even when it is a successive call, a continuation.
   * If we deal with such a continuation, we skip the root node by doing nothing
   * and setting the nextEntry to the proper next node. Otherwise, we proceed as usual.
   */
  if(!continuation) {
    const INode* n = dynamic_cast<const INode*>(&entry);
    if (n->isLeaf()) {
      /* We're dealing with a Leaf here, that means we might have to return something.
       * There might be more than one matching child of the leaf, so we might have
       * to continue with a successive child, memorized as "child_idx".
       */
      for (size_t cChild = child_idx; cChild < n->getChildrenCount() ; cChild++) {
        id_type childId = n->getChildIdentifier(cChild); // for fetching the ACTUAL shape, not MBR from the ID-Shape map
        IShape* childShape = index->getShape(childId);
        /* If the query intersects or contains the child shape we have a match and should return it,
         * proceeding with the next child at the next call of IQueryStrategy.queryStrategy.
         */
        bool b = false;
        const GEOSShape *qs = dynamic_cast<const GEOSShape*>(query);
        if (t == IntersectionQuery) {
          if (qs != NULL) {
            b = qs->intersectsShape(*childShape);
#ifdef DEBUG
            cout << "Query GEOSShape intersects Shape? " << b << endl;           
#endif
          } else {
            b = query->intersectsShape(*childShape);
#ifdef DEBUG
            cout << "Query IShape intersects Shape? " << b << endl;           
#endif
          }
        } else { // ContainmentQuery
          if (qs != NULL) {
            b = qs->containsShape(*childShape);
#ifdef DEBUG
            cout << "Query GEOSShape contains Shape? " << b << endl;     
#endif      
          } else {
            b = query->containsShape(*childShape);
#ifdef DEBUG
            cout << "Query GEOSShape contains Shape? " << b << endl;          
#endif
          }
        }
        if (b) {
          size_t length;
          byte* data;
          n->getChildData(cChild,length,&data);
          // If we got an IVisitor, use it to report the result.
          if (v != NULL) {
            Region childMBR;
            childShape->getMBR(childMBR);
            id_type childIdentifier = n->getChildIdentifier(cChild);
            RTree::Data* e = new RTree::Data(length,data,childMBR,childIdentifier);
            v->visitData(*e);
            delete e;
          }
          char* result_str = new char[length]; // gets freed at the next call or by the destructor
          strcpy(result_str,(char*)data);
          result = result_str; // We store the result.
          result_length = length - 1; // string length, not memory size.
          hasNext = false; // We stop looking for other results (incremental behavior).
          continuation = true; // If we want to find more results we should not start from the root node,
          child_idx = cChild + 1; // but from the next child.
          nextEntry = entry.getIdentifier(); // The next child of the SAME entry.
          ids.push(nextEntry); // After every call, the next entry is popped of the queue, so we push on this node.
          return; // Stop looking for more matches.
        }
      }
      child_idx = 0; // Done checking the children, reset the continuation index.
    } else {
      // We're dealing with a Node here, that means we're just navigating, not looking for matches.
      for (size_t cChild = 0; cChild < n->getChildrenCount() ; cChild++) {
        IShape* childShape;
        n->getChildShape(cChild,&childShape); // using MBRs, not the actual Shape, for speed.
        // Continue searching in every intersecting (and thus possibly overlapping) child node.
        if (query->intersectsShape(*childShape)) {
          ids.push(n->getChildIdentifier(cChild));                     
        }
      }
    }
  } else { // end if !continuation
    continuation = false; // Reset the continuation flag.
  }
  
  if (ids.empty()) {
    // When the queue is empty the options are exhausted.
    // Either we just found the last result or we found no results at all.
    hasNext = false; // No next node.
    result = NULL; // No result.
    result_length = 0;
  } else {
    // There are still candidates to check in the queue.
    if (hasNext) {
      // We have not reached a matching leaf child yet.
      nextEntry = ids.top(); // continue with the next option.
      ids.pop();
    } else if (result != NULL) {
      // We have reached a matching leaf child.
      nextEntry = ids.top(); // continue with the next option.
      // Don't pop the queue, because we continue with the next child of the same leaf node.
    }
  } 
}



// class IncrementalNearestNeighborStrategy : public SpatialIndex::IQueryStrategy

IncrementalNearestNeighborStrategy::IncrementalNearestNeighborStrategy(const IShape* queryp,IVisitor* vp,Index *idx) 
  : result(NULL), result_length(0), continuation(false), child_idx(0), first_call(true) {
  query = queryp;
  v = vp;
  index = idx;
}

void IncrementalNearestNeighborStrategy::getNextEntry(const IEntry& entry, id_type& nextEntry, bool& hasNext) {
  if (result != NULL) { // clean up previous results
    delete result;
    result = NULL;
    result_length = 0;
  }
#ifdef DEBUG
  cout << "visiting node " << entry.getIdentifier() << endl;
#endif
  
  if (first_call && !continuation) { 
    // if this is the first call, put the root node on the stack
    queue.push(new NNEntry(entry.getIdentifier(),NULL,0));
    first_call = false;
  }
  
  if (continuation) { // continuation after returning a result
    continuation = false;
    if (!queue.empty()) {
      hasNext = true;
      nextEntry = queue.top()->m_id; // continue with the top of the stack instead of the root node (entry)
    } else {
      hasNext = false;
    }
  } else { // not a continuation of the algorithm
    if (!queue.empty()) {
      const INode* n = dynamic_cast<const INode*>(&entry);
      const NNEntry* e = queue.top();
#ifdef DEBUG        
      cout << "taking " << e->m_id << " from the queue" << endl;
#endif
      if (e->m_pEntry == NULL) {
        // only pop the queue when we're searching for data
        // when data has been reached, don't pop.
#ifdef DEBUG
        cout << "pop " << queue.top()->m_id << ", new top is ";
#endif
        queue.pop();
#ifdef DEBUG
        cout << queue.top()->m_id << endl;
#endif
      }
      
      if (n->isLeaf() || e->m_pEntry != NULL) { // leaf node or data 
#ifdef DEBUG
        cout << "leaf\n";
#endif
        if (e->m_pEntry != NULL) { // data
#ifdef DEBUG
          cout << "leaf data\n";
#endif
          // If we got an IVisitor, use it to report the result.
          if (v != NULL) {
            v->visitData(dynamic_cast<const IData&>(*(queue.top()->m_pEntry)));
          }
          byte* data;
          size_t length;
          (dynamic_cast<const IData&>(*(queue.top()->m_pEntry))).getData(length,&data);
          char* result_str = new char[length]; // gets freed at the next call or by the destructor
          strcpy(result_str,(char*)data);
          result = result_str; // We store the result.
          result_length = length - 1; // string length, not memory size.
          hasNext = false; // We stop looking for other results (incremental behavior).
          continuation = true; // If we want to find more results we should not start from the root node
          queue.pop();
          nextEntry = queue.top()->m_id;
          return; // Stop looking for more matches.
        } else { // leaf node
#ifdef DEBUG
          cout << "leaf node\n";
#endif
          for (size_t cChild = 0; cChild < n->getChildrenCount(); cChild++) {
            id_type childId = n->getChildIdentifier(cChild); // for fetching the ACTUAL shape, not MBR from the ID-Shape map
            IShape* childShape = index->getShape(childId);
            double dist = nnc.getMinimumDistance(*query,*childShape);

            Region childMBR;
            childShape->getMBR(childMBR);
            size_t length;
            byte* data;
            n->getChildData(cChild,length,&data);
            id_type childIdentifier = n->getChildIdentifier(cChild);
            RTree::Data* e = new RTree::Data(length, data, childMBR ,childIdentifier);

            if (dist > queue.top()->m_minDist) {
#ifdef DEBUG
              cout << "dist > mindist (Shape)\n";
#endif
              // we push the actual shape of the object on the queue, 
              // because there are already objects in the queue that are closer than the MBR.
              double shapeDist = nnc.getMinimumDistance(*query,*childShape);
              queue.push(new NNEntry(childId, e, shapeDist));
#ifdef DEBUG
              cout << "push " << childIdentifier << " with shape" << endl;
#endif
            } else {
#ifdef DEBUG
              cout << "dist <= mindist (MBR)\n";
#endif
              // we push the MBR on the queue
              queue.push(new NNEntry(childId, e, dist));
#ifdef DEBUG
              cout << "push " << childId << " with MBR" << endl;
#endif
            }
          }
        }
        hasNext = true; // continue searching until we hit data
      } else { // index node
#ifdef DEBUG
        cout << "index node\n";
#endif
        for (size_t cChild = 0; cChild < n->getChildrenCount() ; cChild++) {
          IShape* childShape;
          n->getChildShape(cChild,&childShape);
          Region childMBR;
          childShape->getMBR(childMBR);
          double dist = nnc.getMinimumDistance(*query,childMBR);
          queue.push(new NNEntry(n->getChildIdentifier(cChild),NULL,dist));                     
#ifdef DEBUG
          cout << "push " << n->getChildIdentifier(cChild) << endl;
#endif
        }
        hasNext = true;
      }
    } else { // queue.empty()
      hasNext = false;
      continuation = false;
    }
    
  }
  
  // This takes care that the nextEntry is never data,
  // to circumvent the readNode call in the queryStrategy method.
  if (queue.top()->m_pEntry != NULL) {
#ifdef DEBUG
    cout << "next is data, setting entry to NULL" << endl;
#endif
    nextEntry = NULL;          
  } else {
#ifdef DEBUG
    cout << "next is " << queue.top()->m_id << endl;
#endif
    nextEntry = queue.top()->m_id;
  }
}

IncrementalNearestNeighborStrategy::~IncrementalNearestNeighborStrategy() {
  if (result != NULL) {
    delete result;
    result = NULL;
  }
}



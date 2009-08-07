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

#ifndef __SEARCH_H
#define __SEARCH_H

#include <SWI-cpp.h>
#include "globals.h"
#include <spatialindex/SpatialIndex.h>
#include "Index.h"

using namespace std;
using namespace SpatialIndex;


enum RangeQueryType
{
  ContainmentQuery = 0x1,
  IntersectionQuery = 0x2
};


class PrintVisitor : public IVisitor
{
 public:
  void visitNode(const INode& n);
  void visitData(const IData& d);
  void visitData(std::vector<const IData*>& v);
};


class PrintGnuplotVisitor : public IVisitor
{
 public:
  void visitNode(const INode& n);
  void visitData(const IData& d);
  void visitData(std::vector<const IData*>& v);
};


class TraverseBreadthFirst : public SpatialIndex::IQueryStrategy
{
 private:
  queue<id_type> ids;
  IVisitor *v;

 public:
  TraverseBreadthFirst(IVisitor *vis);
  void getNextEntry(const IEntry& entry, id_type& nextEntry, bool& hasNext);
};


class TraverseDepthFirst : public SpatialIndex::IQueryStrategy
{
 private:
  stack<id_type> ids;
  IVisitor *v;

 public:
  TraverseDepthFirst(IVisitor *vis);
  void getNextEntry(const IEntry& entry, id_type& nextEntry, bool& hasNext);
};


class IncrementalRangeStrategy : public SpatialIndex::IQueryStrategy
{
 public:
  atom_t result;
  bool result_found;
  bool continuation;
  size_t child_idx;
  const IShape* query;
  IVisitor* v;
  RangeQueryType t;
  stack<id_type> ids;
  Index *index;

  IncrementalRangeStrategy(RangeQueryType type, IShape* queryp,IVisitor* vp,Index* index);
  ~IncrementalRangeStrategy();

  void getNextEntry(const IEntry& entry, id_type& nextEntry, bool& hasNext);
};



class IncrementalNearestNeighborStrategy : public SpatialIndex::IQueryStrategy
{
private:
  class NNEntry
  {
  public:
    id_type m_id;
    IEntry* m_pEntry;
    double m_minDist;

    NNEntry(id_type id, IEntry* e, double f) : m_id(id), m_pEntry(e), m_minDist(f) {}
    ~NNEntry() { 
      if (m_pEntry != NULL)
        delete m_pEntry;
    }

    struct ascending : public std::binary_function<NNEntry*, NNEntry*, bool>
    {
      bool operator()(const NNEntry* __x, const NNEntry* __y) const { return __x->m_minDist > __y->m_minDist; }
    };
  }; // NNEntry

  class NNComparator : public INearestNeighborComparator
  {
  public:
    double getMinimumDistance(const IShape& query, const IShape& entry)
    {
      return query.getMinimumDistance(entry);
    }

    double getMinimumDistance(const IShape& query, const IData& data)
    {
      IShape* pS;
      data.getShape(&pS);
      double ret = query.getMinimumDistance(*pS);
      delete pS;
      return ret;
    }
  }; // NNComparator

 public:
  atom_t result;
  bool result_found;
  bool continuation;
  size_t child_idx;
  bool first_call;
  const IShape* query;
  IVisitor* v;
  std::priority_queue<NNEntry*, std::vector<NNEntry*>, NNEntry::ascending> queue;
  NNComparator nnc;
  Index *index;

  IncrementalNearestNeighborStrategy( IShape* queryp,IVisitor* vp, Index* index);
  void getNextEntry(const IEntry& entry, id_type& nextEntry, bool& hasNext);
  ~IncrementalNearestNeighborStrategy();
};

#endif // __SEARCH_H

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

//#include <spatialindex/SpatialIndex.h>
#include <SWI-cpp.h>
#include "globals.h"
#include "Shapes.h"

using namespace std;
using namespace SpatialIndex;


class Index
{
 public:
  virtual ~Index() {};

  virtual id_type get_new_id(const char* uri) = 0;
  virtual id_type get_uri_id(const char* uri) = 0;
  virtual void storeShape(id_type id,IShape *s) = 0;
  virtual IShape* getShape(id_type id) = 0;

  virtual IShape* interpret_shape(term_t shape_term) = 0;
  virtual void clear_tree() = 0;
  virtual void create_tree(size_t dimensionality) = 0;
  virtual void create_tree(size_t dimensionality, double util, int nodesz) = 0;
  virtual bool insert_single_object(const char* uri,term_t shape_term) = 0;
  virtual bool delete_single_object(const char* uri,term_t shape_term) = 0;
  virtual bool load_from_file(const char* filename) = 0;

};


class RTreeIndex : public Index
{
 public:
  string *baseName;
  double utilization;
  int nodesize;
  IStorageManager* diskfile;
  StorageManager::IBuffer* file;
  ISpatialIndex* tree;
  id_type indexIdentifier;
  map<string,id_type> uri_id_map;
  map<id_type,IShape*> id_shape_map;

  RTreeIndex(const char* indexname);
  RTreeIndex(const char* indexname, double util, int nodesz);
  virtual ~RTreeIndex();

  virtual id_type get_new_id(const char* uri);
  virtual id_type get_uri_id(const char* uri);
  virtual void storeShape(id_type id,IShape *s);
  virtual IShape* getShape(id_type id);

  virtual IShape* interpret_shape(term_t shape_term);
  virtual bool bulk_load(const char* module,const char* goal,size_t dimensionality);
  virtual void clear_tree();
  virtual void create_tree(size_t dimensionality);
  virtual void create_tree(size_t dimensionality, double util, int nodesz);
  virtual bool insert_single_object(const char* uri,term_t shape_term);
  virtual bool delete_single_object(const char* uri,term_t shape_term);
  virtual bool load_from_file(const char* filename);
  
 public:
  id_type bulkload_tmp_id_cnt;

};

#endif // __INDEX_H

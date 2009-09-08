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

#ifndef __SHAPES_H
#define __SHAPES_H

#include "globals.h"
#include <spatialindex/SpatialIndex.h>
#include <geos/geom/PrecisionModel.h>
#include <geos/geom/Geometry.h>
#include <geos/geom/GeometryFactory.h>
#include <geos/geom/Coordinate.h>
#include <geos/geom/CoordinateSequence.h>
#include <geos/geom/CoordinateArraySequence.h>
#include <geos/geom/LinearRing.h>
#include <geos/geom/Point.h>
#include <geos/geom/Polygon.h>
#include <geos/util/GEOSException.h>
#include <geos/io/WKBReader.h>
#include <geos/io/WKBWriter.h>
#include <geos/io/WKTWriter.h>
#include <cstdlib>
#include <vector>
#include <sstream>
#include <typeinfo> // to check if we have a GEOS shape or a SpatialIndex shape

using namespace std;
using namespace SpatialIndex;

void init_geos(); // must be called before starting to use the shapes
void cleanup_geos(); // must be called after finishing to use the shapes

namespace SpatialIndex
{
  interface GEOSShape : public IShape {
  public:
    virtual ~GEOSShape() {};
    geos::geom::Geometry *g; // pointer to the corresponding geos::geom::Geometry
  };


  /*
   * GEOSPoint
   */

  class GEOSPoint : public GEOSShape {
  public:
    GEOSPoint();
    GEOSPoint(const double* pCoords, size_t dimension);
    GEOSPoint(const GEOSPoint& p);
    GEOSPoint(const geos::geom::Coordinate &coordinate);
    virtual ~GEOSPoint();

     GEOSPoint& operator=(const GEOSPoint& p);
     bool operator==(const GEOSPoint& p) const;

    //
    // IObject interface
    //
     GEOSPoint* clone();

    //
    // ISerializable interface
    //
     size_t getByteArraySize();
     void loadFromByteArray(const byte* data);
     void storeToByteArray(byte** data, size_t& length);

    //
    // IShape interface
    //
     bool intersectsShape(const GEOSShape& in) const;
     bool containsShape(const GEOSShape& in) const;
     bool touchesShape(const GEOSShape& in) const;
     bool intersectsShape(const IShape& in) const;
     bool containsShape(const IShape& in) const;
     bool touchesShape(const IShape& in) const;
     void getCenter(Point& out) const;
     size_t getDimension() const;
     void getMBR(Region& out) const;
     double getArea() const;
     double getMinimumDistance(const GEOSShape& in) const;
     double getMinimumDistance(const IShape& in) const;

     double getCoordinate(size_t index) const;

     void makeInfinite(size_t dimension);
     void makeDimension(size_t dimension);

  private:
    Point* toPoint() const;

  public:
    size_t m_dimension;

    friend class Region;
    friend class GEOSPolygon;
    friend std::ostream& operator<<(std::ostream& os, const GEOSPoint& pt);

  };



  /*
   * GEOSPolygon
   */


  class GEOSPolygon : public GEOSShape {
  public:

    GEOSPolygon();
    GEOSPolygon(const double** verts, size_t nverts, size_t dimension); // verts[nverts][dimension]
    GEOSPolygon(const GEOSPoint*& points, size_t nverts); // [GEOSPoint][nverts]
    GEOSPolygon(const GEOSPolygon& poly);
    GEOSPolygon(const geos::geom::Polygon& poly);

    virtual ~GEOSPolygon();

     GEOSPolygon& operator=(const GEOSPolygon& p);
     bool operator==(const GEOSPolygon&) const;

    //
    // IObject interface
    //
     GEOSPolygon* clone();

    //
    // ISerializable interface
    //
     size_t getByteArraySize();
     void loadFromByteArray(const byte* data);
     void storeToByteArray(byte** data, size_t& length);

    //
    // IShape interface
    //
     bool intersectsShape(const GEOSShape& in) const;
     bool intersectsShape(const IShape& in) const;
     bool intersectsRegion(const Region& r) const;
     bool containsShape(const GEOSShape& in) const;
     bool containsShape(const IShape& in) const;
     bool containsRegion(const Region& r) const;
     bool containsPoint(const Point& r) const;
     bool touchesShape(const GEOSShape& in) const;
     bool touchesShape(const IShape& in) const;
     bool touchesRegion(const Region& r) const;
     bool touchesPoint(const Point& r) const;

     void getCenter(Point& out) const;
     size_t getDimension() const;
     void getMBR(Region& out) const;
     double getArea() const;
     double getMinimumDistance(const GEOSShape& in) const;
     double getMinimumDistance(const IShape& in) const;

     GEOSPolygon* getIntersectingGEOSPolygon(const GEOSPolygon& r) const;
     double getIntersectingArea(const GEOSPolygon& in) const;
     double getMargin() const;

     void combineRegion(const Region& in);
     void combineGEOSPoint(const GEOSPoint& in);
     void getCombinedGEOSPolygon(GEOSPolygon& out, const GEOSPolygon& in) const;

     GEOSPoint* getVertex(size_t vert) const;
     double getCoordinate(size_t vert, size_t index) const;

     void makeInfinite(size_t dimension);
     void makeDimension(size_t dimension);

  private:
    void initialize(const double* verts, size_t nverts, size_t dimension);

    friend class GEOSPoint;
    friend class Region;
    friend std::ostream& operator<<(std::ostream& os, const GEOSPolygon& r);
  }; // GEOSPolygon

}

geos::geom::Geometry* regionToBox(const Region& r);

#endif /*__SHAPES_H*/

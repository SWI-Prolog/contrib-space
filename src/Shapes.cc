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

#include "Shapes.h"

template< typename T >inline 
const std::type_info& get_type( T& object )
{
  return typeid(object) ;
}
template< typename T >inline 
const std::type_info& get_type( const T& object )
{
  return typeid(object) ;
}

using namespace std;
using namespace geos;
using namespace geos::geom;
using namespace SpatialIndex;

void init_geos() { 
  if (global_factory != NULL) return;

  // define a precision model that just uses double precision floating points.
  PrecisionModel *pm = new PrecisionModel(geos::geom::PrecisionModel::FLOATING);
  
  // Initialize global factory with defined PrecisionModel
  // and a SRID of -1 (undefined).
  global_factory = new GeometryFactory(pm, -1);
  
  // We do not need PrecisionMode object anymore, it has
  // been copied to global_factory private storage
  delete pm;
  pm = NULL;

}

void cleanup_geos() {
  if (global_factory != NULL) {
    delete global_factory;
    global_factory = NULL;
  }
}


GEOSPoint::GEOSPoint() {
#ifdef DEBUG
  cout << "entering GEOSPoint::GEOSPoint() " << endl;
#endif 
  this->g = global_factory->createPoint();
}
GEOSPoint::GEOSPoint(const double* pCoords, uint32_t dimension) {
#ifdef DEBUG
cout << "entering GEOSPoint::GEOSPoint(const double* pCoords, uint32_t dimension) " << endl;
#endif
  if (dimension == 2) {
    geos::geom::Coordinate *c = new geos::geom::Coordinate(pCoords[0],pCoords[1]);
    this->g = global_factory->createPoint(*c);
    delete c;
  } else if (dimension == 3) {
    geos::geom::Coordinate *c = new geos::geom::Coordinate(pCoords[0],pCoords[1],pCoords[2]);
    this->g = global_factory->createPoint(*c);
    delete c;
  } else if (dimension == 1) {
    geos::geom::Coordinate *c = new geos::geom::Coordinate(pCoords[0]);
    this->g = global_factory->createPoint(*c);
    delete c;
  } else {
    cerr << dimension << " dimensional points not supported" << endl;
    exit(1);
  }
#ifdef DEBUG
  cout << "created " << dimension << " dimensional GEOSPoint" << endl;
#endif
  this->m_dimension = dimension;
}

GEOSPoint::GEOSPoint(const GEOSPoint& p) {
#ifdef DEBUG
cout << "entering GEOSPoint::GEOSPoint(const GEOSPoint& p) " << endl;
#endif
  const Coordinate *c = p.g->getCoordinate();
  this->g = global_factory->createPoint(*c);
  uint32_t dim;
  if (ISNAN(c->y)) dim = 1;
  else if (ISNAN(c->z)) dim = 2;
  else dim = 3;
  this->m_dimension = dim;
#ifdef DEBUG
  cout << "created " << this->m_dimension << " dimensional GEOSPoint" << endl;
#endif
}
GEOSPoint::GEOSPoint(const Coordinate& c) {
#ifdef DEBUG
  cout << "entering GEOSPoint::GEOSPoint(const Coordinate& c) " << endl;
#endif
  this->g = global_factory->createPoint(c);
#ifdef DEBUG
  cout << "testing this->g " << this->g->getCoordinate()->x << endl;
#endif
  uint32_t dim;
  if (ISNAN(c.y)) dim = 1;
  else if (ISNAN(c.z)) dim = 2;
  else dim = 3;
  this->m_dimension = dim;
#ifdef DEBUG
  cout << "created " << this->m_dimension << " dimensional GEOSPoint from coordinate" << endl;
#endif
}
GEOSPoint::~GEOSPoint() {
#ifdef DEBUG
cout << "entering GEOSPoint::~GEOSPoint() " << endl;
#endif
 global_factory->destroyGeometry(g);
 g = NULL;
}
GEOSPoint* 
GEOSPoint::clone() {
#ifdef DEBUG
cout << "entering GEOSPoint::clone() " << endl;
#endif
  return NULL;
}
uint32_t 
GEOSPoint::getByteArraySize() {
#ifdef DEBUG
cout << "entering GEOSPoint::getByteArraySize() " << endl;
#endif
  byte** data;
  uint32_t length;
  this->storeToByteArray(data, length);
  delete data;
  return length;
}
void
GEOSPoint::loadFromByteArray(const byte* data) {
#ifdef DEBUG
cout << "entering GEOSPoint::loadFromByteArray(const byte* data) " << endl;
#endif
  stringstream sl(ios_base::binary|ios_base::in|ios_base::out);
  uint32_t length = 0;
  memcpy(&length,data,sizeof(uint32_t));
  stringstream s(ios_base::binary|ios_base::in|ios_base::out);
  s.write((const char*)(&data[sizeof(uint32_t)]),length-sizeof(uint32_t));
  s.seekg(0);
  geos::io::WKBReader wkbReader(*global_factory);
  this->g = wkbReader.read(s);
  const geos::geom::Coordinate *c = this->g->getCoordinate();
  uint32_t dim;
  if (ISNAN(c->y)) dim = 1;
  else if (ISNAN(c->z)) dim = 2;
  else dim = 3;
  this->m_dimension = dim;
}
void
GEOSPoint::storeToByteArray(byte** data, uint32_t& length) {
#ifdef DEBUG
cout << "entering GEOSPoint::storeToByteArray(byte** data, uint32_t& length) " << endl;
#endif
  if ( this->m_dimension < 2 || this->m_dimension > 3 ) {
    cerr << "WKB output dimension must be 2 or 3" << endl;
    return;
  }
  stringstream s(ios_base::binary|ios_base::in|ios_base::out);
  geos::io::WKBWriter *wkbWriter = new geos::io::WKBWriter(this->m_dimension,BYTE_ORDER==LITTLE_ENDIAN,true);
  wkbWriter->write(*(this->g), s);
  length = ((uint32_t)(s.tellp()))+sizeof(uint32_t);
  s.seekp(0, ios::beg); // rewind writer pointer
  *data = new byte[length];
  memcpy(*data,&length,sizeof(uint32_t));
  s.read((char*)&((*data)[sizeof(uint32_t)]),length-sizeof(uint32_t));
  delete wkbWriter;
}

bool
GEOSPoint::intersectsShape(const GEOSShape& in) const {
#ifdef DEBUG
cout << "entering GEOSPoint::intersectsShape(const GEOSShape& in) const " << endl;
#endif
  return this->g->intersects(in.g);
}
bool 
GEOSPoint::intersectsShape(const IShape& in) const {
#ifdef DEBUG
cout << "entering GEOSPoint::intersectsShape(const IShape& in) const " << endl;
#endif
 bool rv = false;
  try {
    const GEOSShape &s = dynamic_cast<const GEOSShape&>(in);
    return this->g->intersects(s.g);
  } catch (std::bad_cast &e) {
    Point *p = toPoint();
    try {
      const Region& pr = dynamic_cast<const Region&>(in);
      rv = pr.intersectsShape(*p);
    } catch (std::bad_cast &e) {
      try {
        const Point& ppt = dynamic_cast<const Point&>(in);
        rv = p->intersectsShape(ppt);
      } catch (std::bad_cast &e) {
        cerr << "unsupported type in GEOSPoint::intersectsShape, " << e.what() << endl;
      }
    }
    delete p;
  }
  return rv;
}
bool
GEOSPoint::containsShape(const GEOSShape& in) const {
#ifdef DEBUG
cout << "entering GEOSPoint::containsShape(const GEOSShape& in) const " << endl;
#endif
  return this->g->contains(in.g);
}
bool 
GEOSPoint::containsShape(const IShape& in) const {
#ifdef DEBUG
cout << "entering GEOSPoint::containsShape(const IShape& in) const " << endl;
#endif
 bool rv = false;
  try {
    const GEOSShape &s = dynamic_cast<const GEOSShape&>(in);
    return this->g->contains(s.g);
  } catch (std::bad_cast &e) {
    Point *p = toPoint();
    try {
      const Region& pr = dynamic_cast<const Region&>(in);
      rv = pr.containsShape(*p);
    } catch (std::bad_cast &e) {
      try {
        const Point& ppt = dynamic_cast<const Point&>(in);
        rv = p->containsShape(ppt);
      } catch (std::bad_cast &e) {
        cerr << "unsupported type in GEOSPoint::containsShape, " << e.what() << endl;
      }
    }
    delete p;
  }
  return rv;
}
bool
GEOSPoint::touchesShape(const GEOSShape& in) const {
#ifdef DEBUG
cout << "entering GEOSPoint::touchesShape(const GEOSShape& in) const " << endl;
#endif
  return this->g->touches(in.g);
}
bool 
GEOSPoint::touchesShape(const IShape& in) const {
#ifdef DEBUG
cout << "entering GEOSPoint::touchesShape(const IShape& in) const " << endl;
#endif
 bool rv = false;
 try {
   const GEOSShape &s = dynamic_cast<const GEOSShape&>(in);
   return this->g->touches(s.g);
 } catch (std::bad_cast &e) {
   Point *p = toPoint();
   try {
     const Region& pr = dynamic_cast<const Region&>(in);
     rv = pr.touchesShape(*p);
   } catch (std::bad_cast &e) {
      try {
        const Point& ppt = dynamic_cast<const Point&>(in);
        rv = p->touchesShape(ppt);
      } catch (std::bad_cast &e) {
        cerr << "unsupported type in GEOSPoint::touchesShape, " << e.what() << endl;
      }
    }
   delete p;
  }
  return rv;
}

SpatialIndex::Point*
GEOSPoint::toPoint() const {
#ifdef DEBUG
cout << "entering GEOSPoint::toPoint() const " << endl;
#endif
  const Coordinate *c = g->getCoordinate();
  Point *p;
  if (m_dimension == 2) {
    double a[2];
    a[0] = c->x;
    a[1] = c->y;
    p = new Point(a,2);
#ifdef DEBUG
    cout << "created a 2 dimensional Point" << endl;
#endif
  } else if (m_dimension == 3) {
    double a[3];
    a[0] = c->x;
    a[1] = c->y;
    a[2] = c->z;
    p = new Point(a,3);
#ifdef DEBUG
    cout << "created a 3 dimensional Point" << endl;
#endif
  } else if (m_dimension == 1) {
    double a[1];
    a[0] = c->x;
    p = new Point(a,1);
#ifdef DEBUG
    cout << "created a 1 dimensional Point" << endl;
#endif
  } else {
    cerr << "toPoint for " << m_dimension << " dimensional points not supported" << endl;
    return NULL;
  }
  return p;
}

void
GEOSPoint::getCenter(Point& out) const {
#ifdef DEBUG
cout << "entering GEOSPoint::getCenter(Point& out) const " << endl;
#endif
  out = *(toPoint());
}
uint32_t 
GEOSPoint::getDimension() const {
#ifdef DEBUG
cout << "entering GEOSPoint::getDimension() const " << endl;
#endif
  return m_dimension;
}
void
GEOSPoint::getMBR(Region& out) const {
#ifdef DEBUG
cout << "entering GEOSPoint::getMBR(Region& out) const " << endl;
#endif
  //Envelope e = this->g->getEnvelope();
  double p[m_dimension];
  const Coordinate *c = this->g->getCoordinate();
  if (m_dimension >= 1) p[0] = c->x;
  if (m_dimension >= 2) p[1] = c->y;
  if (m_dimension == 3) p[2] = 0; // no true 3d, just 2d plus z
  Region *r = new Region(p,p,m_dimension);
  out = *r;
  delete r;
}
double 
GEOSPoint::getArea() const {
#ifdef DEBUG
cout << "entering GEOSPoint::getArea() const " << endl;
#endif
  return 0.0;
}
double 
GEOSPoint::getMinimumDistance(const GEOSShape& in) const {
#ifdef DEBUG
cout << "entering GEOSPoint::getMinimumDistance(const GEOSShape& in) const " << endl;
#endif
  return this->g->distance(in.g);
}
double 
GEOSPoint::getMinimumDistance(const IShape& in) const {
#ifdef DEBUG
cout << "entering GEOSPoint::getMinimumDistance(const IShape& in) const " << endl;
#endif
  try {
    const GEOSShape &s = dynamic_cast<const GEOSShape&>(in);
    return this->g->distance(s.g);
  } catch (std::bad_cast &e) {
    double rv = 0.0;
    Point *p = toPoint();
    try {
      const Region& pr = dynamic_cast<const Region&>(in);
      rv = pr.getMinimumDistance(*p);
    } catch (std::bad_cast &e) {
      try {
        const Point& ppt = dynamic_cast<const Point&>(in);
        rv = p->getMinimumDistance(ppt);
      } catch (std::bad_cast &e) {
        cerr << "unsupported type in GEOSPoint::getMinimumDistance, " << e.what() << endl;
      }
    }
    delete p;
    return rv;
  }
}
double 
GEOSPoint::getCoordinate(uint32_t index) const {
#ifdef DEBUG
cout << "entering GEOSPoint::getCoordinate(uint32_t index) const " << endl;
#endif
  const Coordinate *c = this->g->getCoordinate();
  switch(index)  {
  case 0: return c->x;
  case 1: return c->y;
  case 2: return c->z;
  default: cerr << index << " dimensional points not supported" << endl;
  }
  return 0.0;
}
void
GEOSPoint::makeInfinite(uint32_t dimension) {
#ifdef DEBUG
cout << "entering GEOSPoint::makeInfinite(uint32_t dimension) " << endl;
#endif
  cerr << "make infinite not implemented yet" << endl;
}
void
GEOSPoint::makeDimension(uint32_t dimension) {
#ifdef DEBUG
cout << "entering GEOSPoint::makeDimension(uint32_t dimension) " << endl;
#endif
  cerr << "make dimension not implemented yet" << endl;
}

GEOSPoint&
GEOSPoint::operator=(const GEOSPoint& p) {
#ifdef DEBUG
cout << "entering GEOSPoint::operator=(const GEOSPoint& p) " << endl;
#endif
 cerr << "= operator on GEOSPoints not defined yet" << endl;
  return *this; // FIXME
}
bool
GEOSPoint::operator==(const GEOSPoint& p) const {
#ifdef DEBUG
cout << "entering GEOSPoint::operator==(const GEOSPoint& p) const " << endl;
#endif
 cerr << "== operator on GEOSPoints not defined yet" << endl;
  return false; // FIXME
}


std::ostream& SpatialIndex::operator<<(std::ostream& os, const GEOSPoint& r)
{
  os << r.g->getCoordinate()->x << " " << r.g->getCoordinate()->y;
  return os;
}



GEOSPolygon::GEOSPolygon() {
#ifdef DEBUG
cout << "entering GEOSPolygon::GEOSPolygon() " << endl;
#endif
  //  cerr << "GEOSPolygon() constructor not implemented yet" << endl;
}
GEOSPolygon::GEOSPolygon(const double** verts, uint32_t nverts, uint32_t dimension) {
#ifdef DEBUG
cout << "entering GEOSPolygon::GEOSPolygon(const double** verts, uint32_t nverts, uint32_t dimension) " << endl;
#endif
  cerr << "GEOSPolygon(verts,nverts,dimension) constructor not implemented yet" << endl;
}
GEOSPolygon::GEOSPolygon(const GEOSPoint*& points, uint32_t nverts) {
#ifdef DEBUG
cout << "entering GEOSPolygon::GEOSPolygon(const GEOSPoint*& points, uint32_t nverts) " << endl;
#endif
  cerr << "GEOSPolygon(points,nverts) constructor not implemented yet" << endl;
}
GEOSPolygon::GEOSPolygon(const GEOSPolygon& poly) {
#ifdef DEBUG
cout << "entering GEOSPolygon::GEOSPolygon(const GEOSPolygon& poly) " << endl;
#endif
  g = poly.g->clone();
}

GEOSPolygon::GEOSPolygon(const geos::geom::Polygon& poly) {
#ifdef DEBUG
cout << "entering GEOSPolygon::GEOSPolygon(const geos::geom::Polygon& poly) " << endl;
#endif
  g = poly.clone();
}
GEOSPolygon::~GEOSPolygon() {
#ifdef DEBUG
cout << "entering GEOSPolygon::~GEOSPolygon() " << endl;
#endif
 global_factory->destroyGeometry(g);
}
GEOSPolygon* 
GEOSPolygon::clone() {
#ifdef DEBUG
cout << "entering GEOSPolygon::clone() " << endl;
#endif
  Polygon *poly = dynamic_cast<Polygon*>(this->g->clone());
  GEOSPolygon *p = new GEOSPolygon(*poly);
  delete poly;
  return p;
}
uint32_t 
GEOSPolygon::getByteArraySize() {
#ifdef DEBUG
cout << "entering GEOSPolygon::getByteArraySize() " << endl;
#endif
  cerr << "getByteArraySize function not efficient (polygon)" << endl;
  byte** data;
  uint32_t length;
  this->storeToByteArray(data, length);
  delete data;
  return length;
}
void
GEOSPolygon::loadFromByteArray(const byte* data) {
#ifdef DEBUG
cout << "entering GEOSPolygon::loadFromByteArray(const byte* data) " << endl;
#endif
  stringstream sl(ios_base::binary|ios_base::in|ios_base::out);
  uint32_t length = 0;
  memcpy(&length,data,sizeof(uint32_t));
  stringstream s(ios_base::binary|ios_base::in|ios_base::out);
  s.write((const char*)(&data[sizeof(uint32_t)]),length-sizeof(uint32_t));
  s.seekg(0);
  geos::io::WKBReader wkbReader(*global_factory);
  this->g = wkbReader.read(s);
}
void
GEOSPolygon::storeToByteArray(byte** data, uint32_t& length) {
#ifdef DEBUG
cout << "entering GEOSPolygon::storeToByteArray(byte** data, uint32_t& length) " << endl;
#endif
  uint32_t dim = this->getDimension();
  if ( dim < 2 || dim > 3 ) {
    cerr << "WKB output dimension must be 2 or 3" << endl;
    return;
  }
  stringstream s(ios_base::binary|ios_base::in|ios_base::out);
  geos::io::WKBWriter *wkbWriter = new geos::io::WKBWriter(dim,BYTE_ORDER==LITTLE_ENDIAN,true);
  wkbWriter->write(*(this->g), s);
  length = ((uint32_t)(s.tellp()))+sizeof(uint32_t);
  s.seekp(0, ios::beg); // rewind writer pointer
  *data = new byte[length];
  memcpy(*data,&length,sizeof(uint32_t));
  s.read((char*)&((*data)[sizeof(uint32_t)]),length-sizeof(uint32_t));
  delete wkbWriter;
}
geos::geom::Geometry*
regionToBox(const Region& r) {
  geos::geom::Geometry *box;
  if (r.m_dimension == 2) {
    if (r.m_pLow[0] == r.m_pHigh[0] &&
        r.m_pLow[1] == r.m_pHigh[1]) {
      box = global_factory->createPoint(Coordinate(r.m_pLow[0],r.m_pHigh[1]));
    } else if (r.m_pLow[0] == r.m_pHigh[0] ||
               r.m_pLow[1] == r.m_pHigh[1]) {
      geos::geom::CoordinateSequence *cl = new geos::geom::CoordinateArraySequence();
      cl->add(Coordinate(r.m_pLow[0],r.m_pLow[1]));
      cl->add(Coordinate(r.m_pHigh[0],r.m_pHigh[1]));
      box = global_factory->createLineString(cl);
      box->normalize();
    } else {
      geos::geom::CoordinateSequence *cl = new geos::geom::CoordinateArraySequence();
      cl->add(Coordinate(r.m_pLow[0], r.m_pLow[1]));
      cl->add(Coordinate(r.m_pLow[0], r.m_pHigh[1]));
      cl->add(Coordinate(r.m_pHigh[0], r.m_pHigh[1]));
      cl->add(Coordinate(r.m_pHigh[0], r.m_pLow[1]));
      cl->add(Coordinate(r.m_pLow[0], r.m_pLow[1]));
      geos::geom::LinearRing *lr = global_factory->createLinearRing(cl);
      box = global_factory->createPolygon(lr, NULL);
      box->normalize();
    } 
  } else if (r.m_dimension == 1) {
    geos::geom::CoordinateSequence *cl = new geos::geom::CoordinateArraySequence();
    cl->add(Coordinate(r.m_pLow[0]));
    cl->add(Coordinate(r.m_pHigh[0]));
    box = global_factory->createLineString(cl);
    box->normalize();
  } else if (r.m_dimension == 3) {
    cerr << "polygon - 3d region containment not implemented yet" << endl;
    return false;
  } else {
    cerr << "polygon - " << r.m_dimension << " dimensional region containment not implemented" << endl;
    return false;
  }
  return box;
}
bool 
GEOSPolygon::intersectsShape(const GEOSShape& in) const {
#ifdef DEBUG
cout << "entering GEOSPolygon::intersectsShape(const GEOSShape& in) const " << endl;
#endif
  return this->g->intersects(in.g);
}
bool 
GEOSPolygon::intersectsShape(const IShape& in) const {
#ifdef DEBUG
cout << "entering GEOSPolygon::intersectsShape(const IShape& in) const " << endl;
#endif
  try {
    const GEOSShape &s = dynamic_cast<const GEOSShape&>(in);
    return this->g->intersects(s.g);
  } catch (std::bad_cast &e) {
    try {
      const Region& pr = dynamic_cast<const Region&>(in);
      return intersectsRegion(pr);
    } catch (std::bad_cast &e) {
      try {
        const Point& ppt = dynamic_cast<const Point&>(in);
        return containsPoint(ppt); // happens to be the same for points
      } catch (std::bad_cast &e) {
        cerr << "unsupported type in GEOSPolygon::intersectsShape, " << e.what() << endl;
      }
    }
  }
  return false;
}
bool
GEOSPolygon::intersectsRegion(const Region& r) const {
#ifdef DEBUG
  cout << "entering GEOSPolygon::intersectsRegion(const Region& r) const " << endl;
  cout << r.m_dimension << " dimensional intersection query" << endl;
#endif
  geos::geom::Geometry *box = regionToBox(r);
#ifdef DEBUG
  cout << "this->g defined? " << (this->g != NULL) << endl;
#endif
  bool rv = this->g->intersects(box);
#ifdef DEBUG
  cout << "outcome of intersection " << rv << endl;
#endif
  global_factory->destroyGeometry(box);
  return rv;
}
bool 
GEOSPolygon::containsShape(const GEOSShape& in) const {
#ifdef DEBUG
cout << "entering GEOSPolygon::containsShape(const GEOSShape& in) const " << endl;
#endif
  return this->g->contains(in.g);
}
bool 
GEOSPolygon::containsShape(const IShape& in) const {
#ifdef DEBUG
cout << "entering GEOSPolygon::containsShape(const IShape& in) const " << endl;
#endif
  try {
    const GEOSShape &s = dynamic_cast<const GEOSShape&>(in);
    return this->g->contains(s.g);
  } catch (std::bad_cast &e) {
    try {
      const Region& pr = dynamic_cast<const Region&>(in);
      return containsRegion(pr);
    } catch (std::bad_cast &e) {
      try {
        const Point& ppt = dynamic_cast<const Point&>(in);
        return containsPoint(ppt);
      } catch (std::bad_cast &e) {
        cerr << "unsupported type in GEOSPolygon::containsShape, " << e.what() << endl;
      }
    }
  }
  return false;
}
bool 
GEOSPolygon::containsPoint(const Point& in) const {
#ifdef DEBUG
cout << "entering GEOSPolygon::containsPoint(const Point& in) const " << endl;
#endif
 const GEOSPoint *p = new GEOSPoint(in.m_pCoords,in.m_dimension);
 bool rv = this->g->contains(p->g);
 delete p;
 return rv;
}
bool
GEOSPolygon::containsRegion(const Region& r) const {
#ifdef DEBUG
  cout << "entering GEOSPolygon::containsRegion(const Region& r) const " << endl;
  cout << r.m_dimension << " dimensional containment query" << endl;
#endif
  geos::geom::Geometry *box = regionToBox(r);
#ifdef DEBUG
  cout << "this->g defined? " << (this->g != NULL) << endl;
#endif
  geos::io::WKTWriter w;
#ifdef DEBUG
  cout << "shapes: " << w.write(box) << " in " << w.write(this->g) << endl;
#endif
  bool rv = this->g->contains(box);
#ifdef DEBUG
  cout << "outcome of containment " << rv << endl;
#endif
  global_factory->destroyGeometry(box);
  return rv;
}
bool 
GEOSPolygon::touchesShape(const GEOSShape& in) const {
#ifdef DEBUG
  cout << "entering GEOSPolygon::touchesShape(const GEOSShape& in) const " << endl;
#endif
  return this->g->touches(in.g);
}
bool 
GEOSPolygon::touchesShape(const IShape& in) const {
#ifdef DEBUG
cout << "entering GEOSPolygon::touchesShape(const IShape& in) const " << endl;
#endif
  try {
    const GEOSShape &s = dynamic_cast<const GEOSShape&>(in);
    return this->g->touches(s.g);
  } catch (std::bad_cast &e) {
    try {
      const Region& pr = dynamic_cast<const Region&>(in);
      return touchesRegion(pr);
    } catch (std::bad_cast &e) {
      try {
        const Point& ppt = dynamic_cast<const Point&>(in);
        return touchesPoint(ppt);
      } catch (std::bad_cast &e) {
        cerr << "unsupported type in GEOSPolygon::touchesShape, " << e.what() << endl;
      }
    }
  }
  return false;
}
bool 
GEOSPolygon::touchesPoint(const Point& in) const {
#ifdef DEBUG
cout << "entering GEOSPolygon::touchesPoint(const Point& in) const " << endl;
#endif
  const GEOSPoint *p = new GEOSPoint(in.m_pCoords,in.m_dimension);
  bool rv = this->g->touches(p->g);
  delete p;
  return rv;
}
bool
GEOSPolygon::touchesRegion(const Region& r) const {
#ifdef DEBUG
  cout << "entering GEOSPolygon::touchesRegion(const Region& r) const " << endl;
  cout << r.m_dimension << " dimensional touch query" << endl;
#endif
  geos::geom::Geometry *box = regionToBox(r);
#ifdef DEBUG
  cout << "this->g defined? " << (this->g != NULL) << endl;
#endif
  geos::io::WKTWriter w;
#ifdef DEBUG
  cout << "shapes: " << w.write(box) << " touch " << w.write(this->g) << endl;
#endif
  bool rv = this->g->touches(box);
#ifdef DEBUG
  cout << "outcome of touch " << rv << endl;
#endif
  global_factory->destroyGeometry(box);
  return rv;
}
void
GEOSPolygon::getCenter(Point& out) const {
#ifdef DEBUG
cout << "entering GEOSPolygon::getCenter(Point& out) const " << endl;
#endif
  geos::geom::Point *p = this->g->getCentroid();
  GEOSPoint *gp = new GEOSPoint(*(p->getCoordinate()));
  out = *(gp->toPoint());
  delete p;
  delete gp;
}
uint32_t 
GEOSPolygon::getDimension() const {
#ifdef DEBUG
cout << "entering GEOSPolygon::getDimension() const " << endl;
#endif

  const Coordinate *c = this->g->getCoordinate();
  if (ISNAN(c->y)) return 1;
  if (ISNAN(c->z)) return 2;
  else return 3;
}
void
GEOSPolygon::getMBR(Region& out) const {
#ifdef DEBUG
cout << "entering GEOSPolygon::getMBR(Region& out) const " << endl;
#endif
  const Envelope *e = this->g->getEnvelopeInternal();
  const Coordinate *c = this->g->getCoordinate();
  Region *r = NULL;
  if (ISNAN(c->y)) {
    double pLow[1];
    double pHigh[1];
    pLow[0] = e->getMinX();
    pHigh[0] = e->getMaxX();
    r = new Region(pLow,pHigh,1);
  } else if (ISNAN(c->z)) {
    double pLow[2];
    double pHigh[2];
    pLow[0] = e->getMinX();
    pHigh[0] = e->getMaxX();
    pLow[1] = e->getMinY();
    pHigh[1] = e->getMaxY();
    r = new Region(pLow,pHigh,2);
  } else {
    double pLow[3];
    double pHigh[3];
    pLow[0] = e->getMinX();
    pHigh[0] = e->getMaxX();
    pLow[1] = e->getMinY();
    pHigh[1] = e->getMaxY();
    pLow[2] = 0;
    pHigh[2] = 0;
    r = new Region(pLow,pHigh,3);
  }
  out = *r;
  delete r;
}
double 
GEOSPolygon::getArea() const {
#ifdef DEBUG
cout << "entering GEOSPolygon::getArea() const " << endl;
#endif
  return 0.0;
}
double 
GEOSPolygon::getMinimumDistance(const GEOSShape& in) const {
#ifdef DEBUG
cout << "entering GEOSPolygon::getMinimumDistance(const GEOSShape& in) const " << endl;
#endif
  return this->g->distance(in.g);
}
double 
GEOSPolygon::getMinimumDistance(const IShape& in) const {
#ifdef DEBUG
cout << "entering GEOSPolygon::getMinimumDistance(const IShape& in) const " << endl;
#endif
  try {
    const GEOSShape &s = dynamic_cast<const GEOSShape&>(in);
    return this->g->distance(s.g);
  } catch (std::bad_cast &e) {
    try {
      float dist = 0.0;
      const Region& pr = dynamic_cast<const Region&>(in);
      geos::geom::Polygon *box;
      if (pr.m_dimension == 2) {
        geos::geom::CoordinateSequence *cl = new geos::geom::CoordinateArraySequence();
        cl->add(geos::geom::Coordinate(pr.m_pLow[0], pr.m_pLow[1]));
        cl->add(geos::geom::Coordinate(pr.m_pLow[0], pr.m_pHigh[1]));
        cl->add(geos::geom::Coordinate(pr.m_pHigh[0], pr.m_pHigh[1]));
        cl->add(geos::geom::Coordinate(pr.m_pHigh[0], pr.m_pLow[1]));
        cl->add(geos::geom::Coordinate(pr.m_pLow[0], pr.m_pLow[1]));
        geos::geom::LinearRing *lr = global_factory->createLinearRing(cl);
        box = global_factory->createPolygon(lr, NULL); 
        box->normalize();
      } else if (pr.m_dimension == 1) {
        geos::geom::CoordinateSequence *cl = new geos::geom::CoordinateArraySequence();
        cl->add(geos::geom::Coordinate(pr.m_pLow[0]));
        cl->add(geos::geom::Coordinate(pr.m_pHigh[0]));
        geos::geom::LinearRing *lr = global_factory->createLinearRing(cl);
        box = global_factory->createPolygon(lr, NULL);
        box->normalize();
      } else if (pr.m_dimension == 3) {
        cerr << "3d box regions not implemented yet" << endl;
        return 0.0;
      } else {
        cerr << pr.m_dimension << " dimensional regions not supported" << endl;
        return 0.0;
      }
      dist = this->g->distance(box);
      global_factory->destroyGeometry(box);
      return dist;
    } catch (std::bad_cast &e) {
      try {
        float dist = 0.0;
        const Point& ppt = dynamic_cast<const Point&>(in);
        geos::geom::Point *p;
        if (ppt.m_dimension == 2) {
          p = global_factory->createPoint(Coordinate(ppt.m_pCoords[0],ppt.m_pCoords[1]));
        } else if (ppt.m_dimension == 3) {
          p = global_factory->createPoint(Coordinate(ppt.m_pCoords[0],ppt.m_pCoords[1],ppt.m_pCoords[2]));
        } else if (ppt.m_dimension == 1) {
          p = global_factory->createPoint(Coordinate(ppt.m_pCoords[0]));
        } else {
          cerr << ppt.m_dimension << " dimensional points not supported" << endl;
          exit(1);
        }
        dist = this->g->distance(p);
        return dist;
      } catch (std::bad_cast &e) {
        cerr << "unsupported type in GEOSPolygon::getMinimumDistance, " << e.what() << endl;
      }
    }
  }
  return 0.0;
}
GEOSPolygon* 
GEOSPolygon::getIntersectingGEOSPolygon(const GEOSPolygon& r) const {
#ifdef DEBUG
cout << "entering GEOSPolygon::getIntersectingGEOSPolygon(const GEOSPolygon& r) const " << endl;
#endif
  return NULL;
}
double 
GEOSPolygon::getIntersectingArea(const GEOSPolygon& in) const {
#ifdef DEBUG
cout << "entering GEOSPolygon::getIntersectingArea(const GEOSPolygon& in) const " << endl;
#endif
  return 0.0;
}
double 
GEOSPolygon::getMargin() const {
#ifdef DEBUG
cout << "entering GEOSPolygon::getMargin() const " << endl;
#endif
  return 0.0;
}
void
GEOSPolygon::combineRegion(const Region& in) {
#ifdef DEBUG
cout << "entering GEOSPolygon::combineRegion(const Region& in) " << endl;
#endif
}
void
GEOSPolygon::combineGEOSPoint(const GEOSPoint& in) {
#ifdef DEBUG
cout << "entering GEOSPolygon::combineGEOSPoint(const GEOSPoint& in) " << endl;
#endif
}
void
GEOSPolygon::getCombinedGEOSPolygon(GEOSPolygon& out, const GEOSPolygon& in) const {
#ifdef DEBUG
cout << "entering GEOSPolygon::getCombinedGEOSPolygon(GEOSPolygon& out, const GEOSPolygon& in) const " << endl;
#endif
}
GEOSPoint* 
GEOSPolygon::getVertex(uint32_t vert) const {
#ifdef DEBUG
cout << "entering GEOSPolygon::getVertex(uint32_t vert) const " << endl;
#endif
  return NULL;
}
double 
GEOSPolygon::getCoordinate(uint32_t vert, uint32_t index) const {
#ifdef DEBUG
cout << "entering GEOSPolygon::getCoordinate(uint32_t vert, uint32_t index) const " << endl;
#endif
  return 0.0;
}
void
GEOSPolygon::makeInfinite(uint32_t dimension) {
#ifdef DEBUG
cout << "entering GEOSPolygon::makeInfinite(uint32_t dimension) " << endl;
#endif
}
void
GEOSPolygon::makeDimension(uint32_t dimension) {
#ifdef DEBUG
cout << "entering GEOSPolygon::makeDimension(uint32_t dimension) " << endl;
#endif
}
void
GEOSPolygon::initialize(const double* verts, uint32_t nverts, uint32_t dimension) {
#ifdef DEBUG
cout << "entering GEOSPolygon::initialize(const double* verts, uint32_t nverts, uint32_t dimension) " << endl;
#endif
}

GEOSPolygon&
GEOSPolygon::operator=(const GEOSPolygon& p) {
#ifdef DEBUG
cout << "entering GEOSPolygon::operator=(const GEOSPolygon& p) " << endl;
#endif
  return *this; // FIXME
}

bool
GEOSPolygon::operator==(const GEOSPolygon&) const {
#ifdef DEBUG
cout << "entering GEOSPolygon::operator==(const GEOSPolygon&) const " << endl;
#endif
  return false; // FIXME
}

std::ostream& SpatialIndex::operator<<(std::ostream& os, const GEOSPolygon& r)
{
  uint32_t i;
  geos::geom::CoordinateSequence *c = r.g->getCoordinates();
  for (i = 0; i < c->getSize(); i++)
    {
      os << c->getAt(i).x << " " << c->getAt(i).y << " | ";
    }
  return os;
}

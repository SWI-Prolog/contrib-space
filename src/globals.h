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

#ifndef __GLOBALS_H
#define __GLOBALS_H

#ifdef HAVE_CONFIG_H
#include "../config.h"
#endif

#include <SWI-cpp.h>
#include <geos/geom/GeometryFactory.h>

// Geometry
static PlAtom ATOM_point("point");
static PlAtom ATOM_box("box");
static PlAtom ATOM_linestring("linestring");
static PlAtom ATOM_linearring("linearring");
static PlAtom ATOM_polygon("polygon");

// Configuration parameters
static PlAtom ATOM_rtree_utilization("rtree_utilization");
static PlAtom ATOM_rtree_nodesize("rtree_nodesize");
static PlAtom ATOM_rtree_storage("rtree_storage");
static PlAtom ATOM_rtree_distance_function("rtree_distance_function");
// Configuration values
static PlAtom ATOM_pythagorean("pythagorean");
static PlAtom ATOM_haversine("haversine");
static PlAtom ATOM_memory("memory");
static PlAtom ATOM_disk("disk");
    
extern geos::geom::GeometryFactory *global_factory;

#endif // __GLOBALS_H

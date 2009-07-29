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

:- module(wgs84,
	  [  wgs84_candidate/2,
	     coordinates/3,
	     coordinates/4,
	     lat/2,
	     long/2,
	     alt/2
	  ]).

:- use_module(library(semweb/rdf_db)).
:- rdf_register_ns(wgs84, 'http://www.w3.org/2003/01/geo/wgs84_pos#').

%%	wgs84_candidate(?URI,?Point) is nondet.
%
%	Finds URI-Shape pairs of RDF resources that are place-tagged
%	with W3C WGS84 properties (i.e. lat, long, alt).
%	Point = point(?Lat,?Long) ; Point = point(?Lat,?Long,?Alt).

wgs84_candidate(URI,point(Lat,Long)) :-
	\+alt(URI,_),
	lat(URI,Lat),
	long(URI,Long).

wgs84_candidate(URI,point(Lat,Long,Alt)) :-
	lat(URI,Lat),
	long(URI,Long),
	alt(URI,Alt).
	
%%	lat(?URI,?Lat) is nondet.
%
%	Finds the WGS84 latitude of resource URI (and vice versa)
%	using the rdf_db index. Lat is a number.

lat(URI,Lat) :-
	rdf(URI,wgs84:lat,literal(LatAtom)),
	(   LatAtom = type(_,LatVal)
	->  atom_number(LatVal,Lat)
	;   atom_number(LatAtom,Lat)
	).

%%	long(?URI,?Long) is nondet.
%
%	Finds the WGS84 longitude of resource URI (and vice versa)
%	using the rdf_db index. Long is a number.

long(URI,Long) :-
	rdf(URI,wgs84:long,literal(LongAtom)),
	(   LongAtom = type(_,LongVal)
	->  atom_number(LongVal,Long)
	;   atom_number(LongAtom,Long)
	).

%%	alt(?URI,?Alt) is nondet.
%
%	Finds the WGS84 altitude of resource URI (and vice versa)
%	using the rdf_db index. Alt is a number.

alt(URI,Alt) :-
	rdf(URI,wgs84:alt,literal(AltAtom)),
	(   AltAtom = type(_,AltVal)
	->  atom_number(AltVal,Alt)
	;   atom_number(AltAtom,Alt)
	).

%%	coordinates(?URI,?Lat,?Long) is nondet.
%%	coordinates(?URI,?Lat,?Long,?Alt) is nondet.
%
%	Finds the WGS84 latitude, longitude and possibly altitude
%       of resource URI (and vice versa) using the rdf_db index.
%       Lat, Long, and Alt are numbers.

coordinates(URI,Lat,Long) :-
	wgs84_candidate(URI,point(Lat,Long)).

coordinates(URI,Lat,Long,Alt) :-
	wgs84_candidate(URI,point(Lat,Long,Alt)).




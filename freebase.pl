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

:- module(freebase,
	  [  freebase_candidate/2,
	     freebase_candidate/3
	  ]).

:- use_module(library(semweb/rdf_db)).
:- rdf_register_ns(fb, 'http://rdf.freebase.com/ns/').

:- rdf_meta(freebase_candidate(r,?)).
:- rdf_meta(freebase_candidate(r,?,?)).

%%	freebase_candidate(?URI,?Point) is nondet.
%
%	Finds URI-Shape pairs of RDF resource that are place-tagged with
%	Freebase's location.location.geoposition notation that capture
%	WGS84 latitude/longitude positions.

freebase_candidate(URI,point(Lat,Long)) :-
	rdf(URI,fb:'location.location.geolocation',BN),
	rdf(BN,fb:'location.geocode.latitude',literal(type(_,LatAtom))),
	rdf(BN,fb:'location.geocode.longitude',literal(type(_,LongAtom))),
	atom_number(LatAtom,Lat),
	atom_number(LongAtom,Long).

%%	freebase_candidate(?URI,?Point,?Source) is nondet.
%
%	Finds URI-Shape pairs of RDF resource that are place-tagged with
%	Freebase's location.location.geoposition notation that capture
%	WGS84 latitude/longitude positions.
%	From RDF that was loaded from a certain Source.

freebase_candidate(URI,point(Lat,Long),Source) :-
	rdf(URI,fb:'location.location.geolocation',BN,Source),
	rdf(BN,fb:'location.geocode.latitude',literal(type(_,LatAtom))),
	rdf(BN,fb:'location.geocode.longitude',literal(type(_,LongAtom))),
	atom_number(LatAtom,Lat),
	atom_number(LongAtom,Long).





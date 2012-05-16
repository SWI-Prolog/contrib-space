/*  $Id$

    Author:        Willem Robert van Hage
    E-mail:        W.R.van.Hage@vu.nl
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

:- module(dbpedia,
	  [  dbpedia_candidate/2,
	     dbpedia_candidate/3
	  ]).

:- use_module(library(semweb/rdf_db)).
:- rdf_register_ns(dbp, 'http://dbpedia.org/property/').

:- rdf_assert(dbp:'co_percent_e3_percent_b6rdinatenProperty',rdfs:subPropertyOf,dbp:coordinateProperty).

%%	dbpedia_candidate(?URI,?Point) is nondet.
%
%	Finds URI-Shape pairs of RDF resource that are place-tagged with
%	DBpedia's coordinatenProperty notation that capture
%	WGS84 latitude/longitude positions.

dbpedia_candidate(URI,point(Lat,Long)) :-
	rdf_has(URI,dbp:coordinateProperty,literal(lang(nl,Atom))),
	atom_codes(Atom,Codes),
	append("_type:",_,End),
	append(Coords,End,Codes),
	phrase(split([LatDeg,LatMin,LatSec,NS,
		      LongDeg,LongMin,LongSec,EW]),Coords),
	Lat is NS * (LatDeg + (LatMin / 60) + (LatSec / 3600)),
	Long is EW * (LongDeg + (LongMin / 60) + (LongSec / 3600)).


%%	dbpedia_candidate(?URI,?Point,?Source) is nondet.
%
%	Finds URI-Shape pairs of RDF resource that are place-tagged with
%	DBpedia's coordinatenProperty notation that capture
%	WGS84 latitude/longitude positions.
%	From RDF that was loaded from a certain Source.

dbpedia_candidate(URI,point(Lat,Long),Source) :-
	rdf_has(URI,dbp:coordinateProperty,literal(lang(nl,Atom)),Source),
	atom_codes(Atom,Codes),
	append("_type:",_,End),
	append(Coords,End,Codes),
	phrase(split([LatDeg,LatMin,LatSec,NS,
		      LongDeg,LongMin,LongSec,EW]),Coords),
	Lat is NS * (LatDeg + (LatMin / 60) + (LatSec / 3600)),
	Long is EW * (LongDeg + (LongMin / 60) + (LongSec / 3600)).

split([LatDeg,LatMin,LatSec,NS,
       LongDeg,LongMin,LongSec,EW]) -->
	num(LatDeg), "_", num(LatMin), "_", num(LatSec), "_", ns(NS), "_",
	num(LongDeg), "_", num(LongMin), "_", num(LongSec), "_", ew(EW).

ns(-1.0) --> "S".
ns(1.0) --> "N".
ns(-1.0) --> "Z".
ew(-1.0) --> "W".
ew(1.0) --> "E".
ew(1.0) --> "O".

num(Num) -->
	digits(Ds),
	{ (   Ds = []
	  ->  fail
	  ;   atomic_list_concat(Ds,Atom),
	      atom_number(Atom,Num)
	  )
	}.

digit(D) -->
	[C],
	{ code_type(C, digit(D)) }.

digits([D|Ds]) --> digit(D), digits(Ds).
digits([]) --> [].







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

:- module(georss,
	  [ georss_candidate/2,
	    georss_candidate/3,
	    georss_simple_candidate/2,
	    georss_gml_candidate/2,
	    georss_uri_shape_triple/5
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(dcg/basics)).
:- use_module(gml).

:- rdf_meta(georss_simple_candidate(r,?,?)).

:- rdf_register_ns(georss,'http://www.georss.org/georss/').
:- rdf_register_ns(foaf,'http://xmlns.com/foaf/0.1/').

%%	georss_candidate(?URI,?Shape) is nondet.
%
%	Finds URI-Shape pairs by searching for RDF triples that
%	link URI to a Shape with GeoRSS RDF properties
%	(e.g. georss:where, georss:line, georss:polygon).
%	Both GeoRSS Simple and GML are supported.

georss_candidate(URI, Shape) :-
	georss_simple_candidate(URI, Shape).
georss_candidate(URI, Shape) :-
	georss_gml_candidate(URI, Shape).

%%	georss_candidate(?URI,?Shape,+Source) is nondet.
%
%	Finds URI-Shape pairs using georss_candidate/2 in RDF
%	that was loaded from a certain Source.

georss_candidate(URI, Shape, Source) :-
	var(Source),
	georss_simple_candidate(URI, Shape, Source).
georss_candidate(URI, Shape, Source) :-
	var(Source),
	georss_gml_candidate(URI, Shape, Source).

georss_candidate(URI, Shape, Source) :-
	nonvar(Source),
	georss_simple_candidate(URI, Shape, Source).
georss_candidate(URI, Shape, Source) :-
	nonvar(Source),
	georss_gml_candidate(URI, Shape, Source).

%
% GeoRSS Simple
%

%%	georss_simple_candidate(?URI,?Shape) is nondet.
%
%	Finds URI-Shape pairs by searching for GeoRSS Simple properties
%	(e.g. georss:point, georss:line, georss:polygon) in the RDF database.

georss_simple_candidate(URI, Shape) :-
	georss_simple_candidate(URI, Shape, _).

georss_simple_candidate(URI, Point, Source) :-
	rdf(URI, georss:point, PointStringLit, Source),
	parse_poslist_literal(PointStringLit,[Point]).
georss_simple_candidate(URI, linestring(Line), Source) :-
	rdf(URI, georss:line, LineStringLit, Source),
	parse_poslist_literal(LineStringLit,Line).
georss_simple_candidate(URI, polygon([Line]), Source) :-
	rdf(URI, georss:polygon, LineStringLit, Source),
	parse_poslist_literal(LineStringLit,Line).
georss_simple_candidate(URI, box(Line), Source) :-
	rdf(URI, georss:box, LineStringLit, Source),
	parse_poslist_literal(LineStringLit,Line).
georss_simple_candidate(URI, Circle, Source) :-
	rdf(URI, georss:circle, CenterRadiusLit, Source),
	parse_circle_literal(CenterRadiusLit,Circle).

parse_poslist_literal(literal(Lit), Shape) :-
	atom_codes(Lit,LSC),
	phrase(poslist(Shape),LSC).

parse_circle_literal(literal(Lit), Shape) :-
	atom_codes(Lit,CRC),
	phrase(circle(Shape),CRC).

circle(circle(Center,Radius)) -->
	blanks, pos(Center), blank, blanks, float(Radius), blanks.

%%	georss_uri_shape_triple(+URI,+Shape,-Subject,-Predicate,-Object) is det.
%%	georss_uri_shape_triple(-URI,-Shape,+Subject,+Predicate,+Object) is det.
%
%	Converts between a URI-Shape pair and its GeoRSS simple RDF triple form.

georss_uri_shape_triple(URI, Shape, URI, P, O) :-
	(   (   var(URI)
	    ;	var(Shape)
	    )
	->  georss_simple_candidate(URI,Shape)
	;   true
	),
	functor(Shape,GeomType,_),
	georss_simple_predicate(GeomType, P),
	georss_simple_literal(Shape, O).

georss_simple_predicate(point,P) :- rdf_equal(georss:point,P).
georss_simple_predicate(linestring,P) :- rdf_equal(georss:line,P).
georss_simple_predicate(linearring,P) :- rdf_equal(georss:line,P).
georss_simple_predicate(polygon,P) :- rdf_equal(georss:polygon,P).

georss_simple_literal(X, literal(L)) :-
	phrase(coordinates(X), Atomics),
	atomic_list_concat(Atomics, ' ', L).

coordinates(polygon([Coords|_])) --> !, point_list(Coords).
coordinates(linestring(Coords)) --> !, point_list(Coords).
coordinates(linearing(Coords)) --> !, point_list(Coords).
coordinates(Point) --> point(Point).

point_list([]) --> [].
point_list([H|T]) -->
	point(H), point_list(T).

point(Point) -->
	{ Point =.. [ point | List ] },
	List.

%
% GeoRSS GML
%

%%	georss_gml_candidate(?URI,?Shape) is nondet.
%
%	Finds URI-Shape pairs by searching for GeoRSS GML properties
%	(i.e. georss:where) in the RDF database.
%	Uses gml_shape/2 to parse the XMLLiteral representing the GML shape.

georss_gml_candidate(URI, Shape) :-
	georss_gml_candidate(URI, Shape, _).

georss_gml_candidate(URI, Shape, Source) :-
        (   rdf_equal(georss:where, P)
        ;   rdf_equal(foaf:based_near, P)
        ),
        georss_gml_triple(URI, P, GML, Source),
	gml_shape(GML, Shape).

georss_gml_triple(URI, Property, GML, Source) :-
	rdf(URI, Property, Lit, Source),
	gml_literal(Lit, GML).

gml_literal(literal(type(_,GML)),GML) :- !.
gml_literal(literal(GML),GML).

poslist(T) --> blank_star, poslist_plus(T), blank_star, !.
poslist_plus([H|T]) --> pos(H), poslist_star(T).
poslist_star(T) --> blank_plus, poslist(T).
poslist_star([]) --> [], !.

pos(point(X,Y)) --> c(X), blank_plus, c(Y).
pos(point(X,Y,Z)) --> c(X), blank_plus, c(Y), blank_plus, c(Z).
pos(point(X,Y,Z,M)) --> c(X), blank_plus, c(Y), blank_plus, c(Z), blank_plus, c(M).
c(X) --> float(X).

blank_plus --> blank, blank_star, !.
blank_plus --> " ", !.
blank_star --> blanks, !.
blank_star --> [], !.

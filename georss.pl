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

:- module(georss,
	  [ georss_candidate/2,
	    georss_candidate/3,
	    georss_simple_candidate/2,
	    georss_gml_candidate/2,
	    georss_uri_shape_triple/5
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/dcg_basics)).
:- use_module(gml).

:- rdf_register_ns(georss,'http://www.georss.org/georss/').
:- rdf_register_ns(foaf,'http://xmlns.com/foaf/0.1/').

%%	georss_candidate(?URI,?Shape) is nondet.
%
%	Finds URI-Shape pairs by searching for RDF triples that
%	link URI to a Shape with GeoRSS RDF properties
%	(e.g. georss:where, georss:line, georss:polygon).
%	Both GeoRSS Simple and GML are supported.

georss_candidate(URI, Shape) :-
	georss_simple_candidate(URI, Shape) ;
	georss_gml_candidate(URI, Shape).

%%	georss_candidate(?URI,?Shape,+Source) is nondet.
%
%	Finds URI-Shape pairs using georss_candidate/2 in RDF
%	that was loaded from a certain Source.

georss_candidate(URI, Shape, Source) :-
	(   georss_simple_candidate(URI, Shape, Source)
	;   georss_gml_candidate(URI, Shape, Source)
	).

%
% GeoRSS Simple
%

%%	georss_simple_candidate(?URI,?Shape) is nondet.
%
%	Finds URI-Shape pairs by searching for GeoRSS Simple properties
%	(e.g. georss:point, georss:line, georss:polygon) in the RDF database.

georss_simple_candidate(URI, Shape) :-
	georss_simple_candidate(URI, Shape, _).

georss_simple_candidate(URI, Shape, Source) :-
	(   rdf(URI, georss:point, PointStringLit, Source),
	    parse_poslist_literal(PointStringLit,[Point]),
	    Shape = Point
	)
	;
	(   rdf(URI, georss:line, LineStringLit, Source),
	    parse_poslist_literal(LineStringLit,Line),
	    Shape = linestring(Line)
	)
	;
	(   rdf(URI, georss:polygon, LineStringLit, Source),
	    parse_poslist_literal(LineStringLit,Line),
	    Shape = polygon([Line])
	)
	;
	(   rdf(URI, georss:box, LineStringLit, Source),
	    parse_poslist_literal(LineStringLit,Line),
	    Shape = box(Line)
	)
	;
	(   rdf(URI, georss:circle, CenterRadiusLit, Source),
	    parse_circle_literal(CenterRadiusLit,Circle),
	    Shape = Circle
	).

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
	georss_simple_predicate(Shape, P),
	georss_simple_literal(Shape, O).

georss_simple_predicate(S,P) :-
	functor(S,point,1),
	rdf_global_id(georss:point,P).
georss_simple_predicate(S,P) :-
	(   functor(S,linestring,1)
	;   functor(S,linearring,1)
	), !,
	rdf_global_id(georss:line,P).
georss_simple_predicate(S,P) :-
	functor(S,polygon,1),
	rdf_global_id(georss:polygon,P).

number_atom(N,A) :- atom_number(A,N).

georss_simple_literal_aux(T,L) :-
	T =.. [point | Coords],
	maplist(number_atom, Coords, Atoms),
	atomic_list_concat(Atoms, ' ', L).

georss_simple_literal(T,literal(L)) :-
	georss_simple_literal_aux(T,L).

georss_simple_literal(linestring(Coords),literal(L)) :-
	maplist(number_atom, Coords, Atoms),
	atomic_list_concat(Atoms, ' ', L).

georss_simple_literal(linearring(Coords),literal(L)) :-
	maplist(georss_simple_literal_aux, Coords, Atoms),
	atomic_list_concat(Atoms, ' ', L).

georss_simple_literal(polygon([Coords|_]),L) :-
	georss_simple_literal(linearring(Coords),L).


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
        (   rdf_global_id(georss:where, P)
        ;   rdf_global_id(foaf:based_near, P)
        ),
        georss_gml_triple(URI, P, GML),
        atom(GML),
	gml_shape(GML, Shape).

georss_gml_triple(URI,Property,GML) :-
	(   rdf(URI, Property, literal(type(_,GML)), Source), !
	;   rdf(URI, Property, literal(GML), Source), !
	;   rdf(URI, Property, GML, Source)
	).

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

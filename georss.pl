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
	    georss_simple_candidate/2,
	    georss_gml_candidate/2
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/dcg_basics)).
:- use_module(gml).

:- rdf_register_ns(georss,'http://www.georss.org/georss/').

%%	georss_candidate(?URI,?Shape) is nondet.
%
%	Finds URI-Shape pairs by searching for RDF triples that
%	link URI to a Shape with GeoRSS RDF properties
%	(e.g. georss:where, georss:line, georss:polygon).
%	Both GeoRSS Simple and GML are supported.

georss_candidate(URI, Shape) :-
	georss_simple_candidate(URI, Shape) ;
	georss_gml_candidate(URI, Shape).

%
% GeoRSS Simple
%

%%	georss_simple_candidate(?URI,?Shape) is nondet.
%
%	Finds URI-Shape pairs by searching for GeoRSS Simple properties
%	(e.g. georss:point, georss:line, georss:polygon) in the RDF database.

georss_simple_candidate(URI, Shape) :-
	(   rdf(URI, georss:point, PointStringLit),
	    parse_poslist_literal(PointStringLit,Line),
	    Shape = point(Line)
	)
	;
	(   rdf(URI, georss:line, LineStringLit),
	    parse_poslist_literal(LineStringLit,Line),
	    Shape = linestring(Line)
	)
	;
	(   rdf(URI, georss:polygon, LineStringLit),
	    parse_poslist_literal(LineStringLit,Line),
	    Shape = polygon([Line])
	)
	;
	(   rdf(URI, georss:box, LineStringLit),
	    parse_poslist_literal(LineStringLit,Line),
	    Shape = box(Line)
	)
	;
	(   rdf(URI, georss:circle, CenterRadiusLit),
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


%
% GeoRSS GML
%

%%	georss_gml_candidate(?URI,?Shape) is nondet.
%
%	Finds URI-Shape pairs by searching for GeoRSS GML properties
%	(i.e. georss:where) in the RDF database.
%	Uses gml_shape/2 to parse the XMLLiteral representing the GML shape.

georss_gml_candidate(URI, Shape) :-
	(   rdf(URI, georss:where, literal(type(_,GML)))
	;   rdf(URI, georss:where, literal(GML))
	;   rdf(URI, georss:where, GML)
	),
	gml_shape(GML, Shape).


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

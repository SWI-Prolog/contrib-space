:- module(
  georss,
  [
    georss_candidate/2,        % ?Res, ?Shape
    georss_candidate/3,        % ?Res, ?Shape, ?G
    georss_simple_candidate/2, % ?Res, ?Shape
    georss_gml_candidate/2     % ?Res, ?Shape
  ]
).

/** <module> WGS84

@author Willem Robert van Hage
@version 2009-2012

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(semweb/rdf11)).

:- use_module(gml).

:- rdf_meta
   georss_simple_candidate(r,?,?).

:- rdf_register_prefix(georss, 'http://www.georss.org/georss/').





%! georss_candidate(?Res, ?Shape) is nondet.
%! georss_candidate(?Res, ?Shape, +G) is nondet.
%
% Finds Res-Shape pairs by searching for RDF triples that link
% resource Res to a Shape with GeoRSS RDF properties
% (e.g. georss:where, georss:line, georss:polygon).  Both GeoRSS
% Simple and GML are supported.

georss_candidate(Res, Shape) :-
  georss_candidate(Res, Shape, _).


georss_candidate(Res, Shape, G) :-
  georss_simple_candidate(Res, Shape, G).
georss_candidate(Res, Shape, G) :-
  georss_gml_candidate(Res, Shape, G).



%! georss_simple_candidate(?Res, ?Shape) is nondet.
%! georss_simple_candidate(?Res, ?Shape, ?G) is nondet.
%
% Finds Res-Shape pairs by searching for GeoRSS Simple properties
% (e.g. `georss:point`, `georss:line`, `georss:polygon`) in the RDF
% database.

georss_simple_candidate(Res, Shape) :-
  georss_simple_candidate(Res, Shape, _).


georss_simple_candidate(Res, box(Line), G) :-
  rdf_has(Res, georss:box, Lex^^_, _, G),
  string_phrase(pointlist(Line), Lex).
georss_simple_candidate(Res, Circle, G) :-
  rdf_has(Res, georss:circle, Lex^^_, _, G),
  string_phrase(circle(Circle), Lex).
georss_simple_candidate(Res, linestring(Line), G) :-
  rdf_has(Res, georss:linestring, Lex^^_, _, G),
  string_phrase(pointlist(Line), Lex).
georss_simple_candidate(Res, Point, G) :-
  rdf_has(Res, georss:point, Lex^^_, _, G),
  string_phrase(pointlist([Point]), Lex).
georss_simple_candidate(Res, polygon([Line]), G) :-
  rdf_has(Res, georss:polygon, Lex^^_, _, G),
  string_phrase(pointlist(Line), Lex).



%! georss_gml_candidate(?Res, ?Shape) is nondet.
%! georss_gml_candidate(?Res, ?Shape, ?G) is nondet.
%
% Finds Res-Shape pairs by searching for GeoRSS GML properties
% (i.e. georss:where) in the RDF database.  Uses gml_shape/2 to parse
% the XMLLiteral representing the GML shape.

georss_gml_candidate(Res, Shape) :-
  georss_gml_candidate(Res, Shape, _).


georss_gml_candidate(Res, Shape, G) :-
  (rdf_equal(georss:where, P) ; rdf_equal(foaf:based_near, P)),
  rdf_has(Res, P, Gml^^_, _, G),
  gml_shape(Gml, Shape).





% GRAMMAR %

coords(polygon([Coords|_])) --> !, coordlist(Coords).
coords(linestring(Coords)) --> !, coordlist(Coords).
coords(linearing(Coords)) --> !, coordlist(Coords).
coords(Point) --> point(Point).

coordlist(Coords) --> *(blank), seplist(float, +(blank), Coords), !, *(blank).

pointlist(Points) --> *(blank), seplist(point, +(blank), Points), !, *(blank).

point([X,Y]) --> float(X), +(blank), float(Y).
point([X,Y,Z]) --> point([X,Y]), +(blank), float(Z).
point([X,Y,Z,M]) --> point([X,Y,Z]), +(blank), float(M).

circle(circle(Center,Radius)) -->
  *(blank), point(Center), +(blank), float(Radius), *(blank).

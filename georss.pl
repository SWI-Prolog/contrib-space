:- module(
  georss,
  [
    georss_candidate/2, % ?Res, ?Shape
    georss_candidate/3  % ?Res, ?Shape, ?G
  ]
).

/** <module> GeoRSS

@author Willem Robert van Hage
@version 2009-2012

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(xpath)).
:- use_module(library(xml/xml_dom)).

:- dynamic
   gis:resource_shape_hook/3.

:- multifile
   gis:resource_shape_hook/3.

gis:resource_shape_hook(Res, Shape, G) :-
  georss_candidate(Res, Shape, G).

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



%! gml_shape(?Gml, ?Shape) is semidet.
%
% Converts between the GML serialization of a shape and its internal
% Prolog representation.

gml_shape(Gml, Geom) :-
  var(Geom), !,
  Opts = [
    dialect(xmlns),
    xmlns('http://www.opengis.net/gml'),
    xmlns(gml, 'http://www.opengis.net/gml')
  ],
  atom_to_xml_dom(Gml, Dom, Opts),
  transform_gml(Dom, Geom).
gml_shape(Gml, Geom) :-
  construct_gml(Gml, Geom).



linearring('gml:LinearRing'('gml:posList'(LSC)),LR) :-
  atom_phrase(pointlist(LR),LSC).



interior([],[]).
interior(['gml:interior'(LR1)|T1],[LR2|T2]) :-
  linearring(LR1,LR2),
  interior(T1,T2).



construct_gml(GML,point(X,Y)) :-
  atomic_list_concat([X,Y],' ',PosList),
  phrase(html('gml:Point'('gml:pos'(PosList))),Atoms),
  atomic_list_concat(Atoms,GML).
construct_gml(GML,point(X,Y,Z)) :-
  atomic_list_concat([X,Y,Z],' ',PosList),
  phrase(html('gml:Point'('gml:pos'(PosList))),Atoms),
  atomic_list_concat(Atoms,GML).
construct_gml(GML,point(X,Y,Z,M)) :-
  atomic_list_concat([X,Y,Z,M],' ',PosList),
  phrase(html('gml:Point'('gml:pos'(PosList))),Atoms),
  atomic_list_concat(Atoms,GML).
construct_gml(GML,linestring(LS)) :-
  atom_phrase(pointlist(LS), LSC),
  phrase(html('gml:LineString'('gml:posList'(LSC))),Atoms),
  atomic_list_concat(Atoms,GML).
construct_gml(GML,polygon([Ext|Int])) :-
  linearring(ExtT,Ext),
  interior(InteriorTerms,Int),
  phrase(html('gml:Polygon'(['gml:exterior'(ExtT)|InteriorTerms])),Atoms),
  atomic_list_concat(Atoms,GML).
construct_gml(GML,box(point(X1,Y1),point(X2,Y2))) :-
  atomic_list_concat([X1,Y1],' ',PosList1),
  atomic_list_concat([X2,Y2],' ',PosList2),
  phrase(html('gml:Envelope'(['gml:lowerCorner'(PosList1),
            'gml:upperCorner'(PosList2)])),Atoms),
  atomic_list_concat(Atoms,GML).



transform_gml(Elts,P) :-
  member(element(_:'Point',_,PointElts),Elts),
  get_point(PointElts,P).
transform_gml(Elts,linestring(LS)) :-
  member(element(_:'LineString',_,LineStringElts),Elts),
  get_linestring(LineStringElts,LS).
transform_gml(Elts,polygon([Ext|Int])) :-
  member(element(_:'Polygon',_,PolygonElts),Elts),
  get_polygon_exterior(PolygonElts,Ext),
  get_polygon_interiors(PolygonElts,Int).
transform_gml(Elts,box(Lower,Upper)) :-
  member(element(_:'Envelope',_,BoxElts),Elts),
  get_box(BoxElts,Lower,Upper).



get_point(Elts,P) :-
  xpath(Elts, //(_:'pos'), element(_,_,[X])),
  atom_phrase(point(P), X).



get_linestring(Elts, LS) :-
  xpath(Elts, //(_:'posList'), element(_,_,[X])),
  atom_phrase(pointlist(LS), X).



get_polygon_exterior(Polygon, Ext) :-
  xpath(
    Polygon,
    //(_:'exterior')/(_:'LinearRing')/(_:'posList'),
    element(_,_,[X])
  ),
  atom_phrase(pointlist(Ext), X).
get_polygon_interiors(Polygon, Int) :-
  findall(I, get_polygon_interior(Polygon, I), Int).
get_polygon_interior(Polygon,Int) :-
  xpath(
    Polygon,
    //(_:'interior')/(_:'LinearRing')/(_:'posList'),
    element(_,_,[X])
  ),
  atom_phrase(pointlist(Int), X).



get_box(Elts, LBC, UBC) :-
  xpath(Elts, //(_:'lowerCorner'), element(_,_,[L])),
  atom_phrase(point(LBC), L),
  xpath(Elts, //(_:'lowerCorner'), element(_,_,[U])),
  atom_phrase(point(UBC), U).





% GRAMMAR %

pointlist(Points) --> *(bs), seplist(point, +(bs), Points), !, *(bs).

point(point(X,Y)) --> float(X), +(bs), float(Y).
point(point(X,Y,Z)) --> point([X,Y]), +(bs), float(Z).
point(point(X,Y,Z,M)) --> point([X,Y,Z]), +(bs), float(M).

circle(circle(Center,Radius)) -->
  *(bs), point(Center), +(bs), float(Radius), *(bs).

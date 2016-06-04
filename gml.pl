:- module(
  gml,
  [
    gml_shape/2
  ]
).

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(xpath)).
:- use_module(library(xml/xml_dom)).





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

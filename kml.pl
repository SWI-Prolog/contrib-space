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

:- module(kml,
	  [
	    kml_shape/2,
	    kml_shape/4,
	    kml_uri_shape/3,
	    kml_file_shape/2,
	    kml_file_shape/4,
	    kml_file_uri_shape/3,
	    kml_save_header/2,
	    kml_save_shape/3,
	    kml_save_footer/1
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/dcg_basics)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(xpath)).


%%	kml_shape(?Stream,?Shape) is semidet.
%%	kml_shape(?Stream,?Shape,?Attributes,?Content) is semidet.
%
%	Converts between the KML serialization of a shape and its
%	internal Prolog term representation.
%	Attributes and Content can hold additional
%	attributes and XML content elements of the KML, like ID, name,
%	or styleUrl.

kml_shape(KML, Geom) :- kml_shape(KML, Geom, _A, _C).
kml_shape(KML, Geom, Attributes, Content) :-
	(   nonvar(KML)
	->  atom_to_memory_file(KML, Memfile),
	    open_memory_file(Memfile, read, Stream),
	    call_cleanup(load_structure(Stream, XML,
					[ dialect(xmlns),
					  xmlns('http://www.opengis.net/kml/2.2'),
					  xmlns(kml, 'http://www.opengis.net/kml/2.2')
					]),
			 free_data(Stream, Memfile)),
	    transform_kml(XML, Geom, Attributes, Content)
	;   construct_kml(KML, Geom, Attributes, Content)
	).

%%	kml_uri_shape(?KML,?URI,?Shape) is semidet.
%
%	Converts between the KML serialization of a URI-shape pair and its
%	internal Prolog term representation.
%	It is assumed the KML Geometry element has a ID attribute
%       specifying the URI of the shape.
%	e.g. <Point ID="http://example.org/point1"><coordinates>52.37,4.89</coordinates></Point>

kml_uri_shape(KML, URI, Shape) :-
	kml_shape(KML, placemark(Shape,Attributes,Content)),
	get_uri(placemark(Shape,Attributes,Content), URI).

%%	kml_file_shape(+File,?Shape) is semidet.
%%	kml_file_shape(+File,?Shape,?Attributes,?Content) is semidet.
%
%	Reads shapes from a KML file using kml_shape/2.
%	kml_file_shape/4 also reads extra attributes and elements of
%	the KML Geometry.
%	e.g. <Point targetId="NCName"><extrude>0</extrude>...</Point>
%	will, besides parsing the Point, also instantiate Content with
%       [extrude(0)] and Attributes with [targetId('NCName')].

kml_file_shape(File, Geom) :- kml_file_shape(File, Geom, _A, _C).
kml_file_shape(File, Geom, Attributes, Content) :-
	open(File, read, Stream, []),
	read_stream_to_codes(Stream, Codes),
	atom_codes(KML, Codes), !,
	kml_shape(KML, Geom, Attributes, Content).

%%	kml_file_uri_shape(+File,?URI,?Shape) is semidet.
%
%	Reads URI-shape pairs from File using kml_uri_shape/2.

kml_file_uri_shape(File, URI, Shape) :-
	kml_file_shape(File, placemark(Shape,Attributes,Content)),
	get_uri(placemark(Shape,Attributes,Content), URI).

get_uri(placemark(_Shape,Attributes,_Content),URI) :-
	(   memberchk('ID'(URI), Attributes), !
	;   memberchk('id'(URI), Attributes)
	).


kml_ns(kml, 'http://www.opengis.net/kml/2.2/', _).

free_data(Stream, Memfile) :-
	close(Stream),
	free_memory_file(Memfile).

coordinates(point(X,Y),PosList) :-
	atomic_list_concat([Y,X], ',', PosList).
coordinates(point(X,Y,Z),PosList) :-
	atomic_list_concat([Y,X,Z], ',', PosList).

coordinates_list(Points, PosList) :-
	coordinates_list_aux(Points, PosLists),
	atomic_list_concat(PosLists, ' ', PosList).
coordinates_list_aux([],[]).
coordinates_list_aux([H|T],[Coord|Coords]) :-
	coordinates(H, Coord),
	coordinates_list_aux(T,Coords).

linearring(Points, Ring) :-
	(   nth0(0, Points, First1),
	    last(Points, First1)
	->  Ring = Points
	;   nth0(0, Points, First2),
	    append(Points, [First2], Ring)
	).

interior_tags([],[]).
interior_tags([PosList|PosLists],
	      ['LinearRing'('coordinates'(PosList))|ILRS]) :-
	interior_tags(PosLists, ILRS).

construct_kml(KML, Geom) :- construct_kml(KML, Geom, [], []).

construct_kml(KML, Geom, Attributes, Content) :-
	construct_term(Geom, Attributes, Content, T),
	phrase(html(T), Atoms),
	atomic_list_concat(Atoms, KML).

construct_term(Geom, Attributes, Content, T) :-
	(   placemark_term(Geom, Attributes, Content, T)
	;   point_term(Geom, Attributes, Content, T)
	;   linestring_term(Geom, Attributes, Content, T)
	;   linearring_term(Geom, Attributes, Content, T)
	;   polygon_term(Geom, Attributes, Content, T)
	;   multigeometry_term(Geom, Attributes, Content, T)
	).

point_term(Point, Attributes, Content, P) :-
	coordinates(Point, PosList),
	P = 'Point'(Attributes,[ 'coordinates'(PosList)|Content]).

linestring_term(linestring(Points), Attributes, Content, LR) :-
	coordinates_list(Points, PosList),
	LR = 'LineString'(Attributes, [ 'coordinates'(PosList) | Content ]).

linearring_term(linestring(Points), Attributes, Content, LR) :-
	linearring(Points, Ring),
	coordinates_list(Ring, PosList),
	LR = 'LinearRing'(Attributes, [ 'coordinates'(PosList) | Content ]).

polygon_term(polygon([ExternalRing|InternalRings]),
	     Attributes, Content, PT) :-
	linearring(ExternalRing, ER),
	coordinates_list(ER, ExtPosList),
	maplist(coordinates_list, InternalRings, IntPosLists),
	interior_tags(IntPosLists, ILRS),
	(   ILRS = []
	->  Rest = Content
	;   append(['innerBoundaryIs'(ILRS)], Content, Rest)
	),
	PT = 'Polygon'(Attributes, [ 'outerBoundaryIs'('LinearRing'('coordinates'(ExtPosList))) | Rest ]).

multigeometry_term(geometrycollection(GCS),
		   Attributes, Content, GCT) :-
	multigeometry_term_aux(GCS, GCTS),
	append(GCTS, Content, GCTS_and_Content),
	GCT = 'MultiGeometry'(Attributes, GCTS_and_Content).

multigeometry_term_aux([], []).
multigeometry_term_aux([Geom|Geoms], [T|Ts]) :-
	construct_term(Geom, [], [], T),
	multigeometry_term_aux(Geoms, Ts).

placemark_term(placemark(Geom), Attributes, Content, T) :-
	placemark_term(placemark(Geom, [], []), Attributes, Content, T).

placemark_term(placemark(Geom, GeomAttr, GeomCont), Attributes, Content, T) :-
	construct_term(Geom, GeomAttr, GeomCont, GT),
	T = 'Placemark'(Attributes, [ GT | Content ]), !.

% -----

transform_kml(Elts, P, Attributes, Content) :-
	member(element(_:'Point',_,PointElts), Elts),
	get_point(PointElts, P),
	get_extras(Elts, Attributes, Content).

transform_kml(Elts, linestring(P), Attributes, Content) :-
	(   member(element(_:'LineString',_,LSE), Elts)
	;   member(element(_:'LinearRing',_,LSE), Elts)
	),
        get_linestring(LSE, P),
	get_extras(Elts, Attributes, Content).

transform_kml(Elts, polygon([Ext|Int]), Attributes, Content) :-
	member(element(_:'Polygon',_,PolygonElts), Elts),
	get_polygon_exterior(PolygonElts, Ext),
	get_polygon_interiors(PolygonElts, Int),
	get_extras(Elts, Attributes, Content).

transform_kml(Elts, geometrycollection(Geoms), Attributes, Content) :-
	member(element(_:'MultiGeometry',_,GeomElts), Elts),
	get_geometry(GeomElts, Geoms),
	get_extras(Elts, Attributes, Content).

transform_kml(Elts, placemark(Geom, GeomAttr, GeomCont), Attributes, Content) :-
	member(element(_:'Placemark',_,GeomElts), Elts),
	transform_kml(GeomElts, Geom, GeomAttr, GeomCont),
	get_extras(Elts, Attributes, Content).

transform_kml(Elts, Geom, Attributes, Content) :-
	member(element(_:'Placemark',_,GeomElts), Elts),
	transform_kml(GeomElts, Geom, _, _),
	get_extras(Elts, Attributes, Content).

transform_kml(Elts, PMs, Attributes, Content) :-
	member(element(_:'Document',_,PMElts), Elts),
	transform_kml(PMElts, PMs, _, _),
	get_extras(Elts, Attributes, Content).

transform_kml(Elts, Doc, Attributes, Content) :-
	member(element(_:'kml',_,PMElts), Elts),
	transform_kml(PMElts, Doc, _, _),
	get_extras(Elts, Attributes, Content).

get_geometry([],[]).

get_geometry([Elt|Elts], [Geom|Geoms]) :-
	transform_kml([Elt], Geom, _A, _C),
	get_geometry(Elts, Geoms), !.
get_geometry([_|Elts], Geoms) :-
	get_geometry(Elts, Geoms).

get_point(Elts, P) :-
	xpath(Elts, //(_:'coordinates'), element(_,_,[A])),
	atom_codes(A, C),
	phrase(pos(P), C).

get_linestring(Elts, P) :-
	xpath(Elts, //(_:'coordinates'), element(_,_,[A])),
	atom_codes(A, C),
	phrase(poslist(P), C).

get_polygon_exterior(Elts, Ext) :-
	xpath(Elts, //(_:'outerBoundaryIs')//(_:'coordinates'), element(_,_,[A])),
	atom_codes(A, C),
	phrase(poslist(Ext), C).

get_polygon_interiors(Elts, Int) :-
	findall(I,
		(   xpath(Elts, //(_:'innerBoundaryIs')//(_:'coordinates'), element(_,_,[A])),
		    atom_codes(A, C),
		    phrase(poslist(I), C)
		),
		Int).

get_extras(Elts, Attributes, Content) :-
	(   member(element(_,_,Cont), Elts),
	    member(element(_:Tag,_A1,[C]), Cont),
	    \+member(Tag,['Point','LineString','LinearRing','Polygon','innerBoundaryIs','outerBoundaryIs','MultiGeometry','coordinates','Placemark']),
	    ContTag =.. [Tag,C]
	->  Content = [ContTag]
	;   Content = []
	),
	(   member(element(_,Attr,_), Elts),
	    memberchk('ID'=I, Attr)
	->  Attributes = ['ID'(I)]
	;   Attributes = []
	).



poslist(T) --> blank_star, poslist_aux(T), blank_star, !.
poslist_aux(L) --> poslist_plus(L).
poslist_plus([H|T]) --> pos(H), poslist_star(T).
poslist_star(T) --> blank_plus, poslist_aux(T).
poslist_star([]) --> [], !.

pos(point(X,Y)) --> c(Y), ",", blank_star, c(X).
pos(point(X,Y,Z)) --> c(Y), ",", blank_star, c(X), ",", blank_star, c(Z).
pos(point(X,Y,Z,M)) --> c(Y), ",", blank_star, c(X), ",", blank_star, c(Z), ",", blank_star, c(M).
pos(point(X,Y)) --> c(Y), ",", blank_star, c(X), ",", blank_star, "0".
c(X) --> float(X).

blank_plus --> blank, blank_star, !.
blank_plus --> " ", !.
blank_star --> blanks, !.
blank_star --> [], !.



%%	kml_save_header(+Stream,+Options) is semidet.
%
%	Outputs a KML header to Stream.
%	This can be followed by calls to kml_save_shape/3 and
%	kml_save_footer/1.
%
%	Options is an option list that can contain the option name(Name)
%	specifying the Name of the document.
%
%	@tbd options to configure optional entities, like styles

kml_save_header(Stream,Options) :-
	format(Stream,"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",[]),
	format(Stream,"<kml xmlns=\"http://www.opengis.net/kml/2.2\">\n",[]),
	format(Stream,"<Document>\n",[]),
	(   stream_property(Stream,file_name(FName)),
	    option(name(Name),Options,FName)
	->  format(Stream,"<name>~w</name>\n",[Name])
	;   option(name(Name),Options),
	    atom(Name)
	->  format(Stream,"<name>~w</name>\n",[Name])
	;   true
	).


%%	kml_save_shape(+Stream,+Shape,+Options) is semidet.
%
%	Outputs a KML serialization of Shape to Stream.
%	This can be preceded by a call to kml_save_header/2 and
%	followed by more calls to kml_save_shape/3 and a call to
%	kml_save_footer/1.
%
%	Options is an option list that can contain the option
%	attr(+List) or content(+List) that can be used to add
%	additional attributes or xml element content to the placemark.
%	This can be used to specify things like the ID, name, or
%	styleUrl of the placemark element.

kml_save_shape(Stream,Shape,Options) :-
	option(attr(Attributes),Options,[]),
	option(content(Content),Options,[]),
	kml_shape(KML,Shape,Attributes,Content),
	format(Stream,"~w\n",[KML]).

%%	kml_save_footer(+Stream) is det.
%
%	Outputs a KML footer to stream Stream.
%	This can be preceded by calls to kml_save_header/2 and
%	kml_save_shape/3.

kml_save_footer(Stream) :-
	format(Stream,"\n</Document>\n</kml>\n\n",[]).


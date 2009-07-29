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


/*
    TODO: rewrite this code into a XPath based parser and a html_write type
    DCG generator that uses option lists, so that you can easily add styles.
*/


:- module(kml,
	  [ kml_shape/2,
	    kml_placemark/2,
	    kml_document/2,
	    kml_save_header/2,
	    kml_save_shape/3,
	    kml_save_footer/1
	  ]).

:- use_module(library(http/dcg_basics)).
:- use_module(library(option)).

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
	stream_property(Stream,file_name(FName)),
	option(name(Name),Options,FName),
	format(Stream,"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",[]),
	format(Stream,"<kml xmlns=\"http://www.opengis.net/kml/2.2\">\n\n",[]),
	format(Stream,"<Document>\n<name>~w</name>\n",[Name]).

%%	kml_save_shape(+Stream,+Shape,+Options) is semidet.
%
%	Outputs a KML serialization of Shape to Stream.
%	This can be preceded by a call to kml_save_header/2 and
%	followed by more calls to kml_save_shape/3 and a call to
%	kml_save_footer/1.
%	
%	Options is an option list that can contain the option name(Name)
%	specifying the name of the Shape (e.g. label or URI).
%	
%	@tbd options to configure optional entities of the shape, like styles.

kml_save_shape(Stream,Shape,Options) :-
	option(name(Name),Options,'unidentified feature'),
	kml_placemark(KML,placemark(Name,Shape)),
	format(Stream,"~s\n",[KML]).

%%	kml_save_footer(+Stream) is det.
%
%	Outputs a KML footer to stream Stream.
%	This can be preceded by calls to kml_save_header/2 and
%	kml_save_shape/3.

kml_save_footer(Stream) :-
	format(Stream,"\n</Document>\n</kml>\n\n",[]).

%%	kml_shape(?KML,?Shape) is nondet.
%
%	Converts between a KML serialization of a Shape and its
%	internal Prolog term representation.

kml_shape(KML, Shape) :- % Shape = point(2.0,-4.2) etc.
	(   var(KML)
	->  phrase(geometry_tag(Shape), KMLcodes),
	    atom_codes(KML,KMLcodes)
	;   atom_codes(KML,KMLcodes),
	    phrase(geometry_tag(Shape), KMLcodes)
	).

%%	kml_placemark(+KML,-Placemark) is det.
%
%	Constructs a KML placemark tag containing the KML serialization
%	of a Shape. Placemark = placemark(+Name,+Shape).

kml_placemark(KML,P) :- % P = placemark(Name,Shape)
	once(phrase(placemark_tag(P),KML)).

%%	kml_document(+KML,-Doc) is det.
%
%	Constructs a KML document tag containing the specified placemarks.
%	Document = document(+Name,[placemark(+Name,+Shape)|...].

kml_document(KML,D) :- % D = document(Name,[placemark(N,Shape)|...])
	once(phrase(kmldoc(D),KML)).

kmldoc(D) -->
	"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n", blank_star,
	"<kml xmlns=\"http://www.opengis.net/kml/2.2\">", blank_star,
	document_tag(D), blank_star,
	"</kml>", blank_star.

document_tag(document(Name,Shapes)) -->
	"<Document>", blank_star,
	"<name>", name_str(Name), "</name>", blank_star,
	placemarks(Shapes), blank_star,
	"</Document>", blank_star.

name_str(Name) --> 
	(   { var(Name) } 
	->  string(NameStr), 
	    { atom_codes(Name,NameStr) }
	;   { atom_codes(Name,NameStr) },
	    string(NameStr)
	).

placemarks([H|T]) -->
	placemark_tag(H), blank_star,
	placemarks(T).
placemarks([]) --> [], !.

placemark_tag(placemark(Name,Shape)) -->
	"<Placemark>", blank_star,
	"<name>", name_str(Name), "</name>", blank_star,
	geometry_tag(Shape), blank_star,
	"</Placemark>".

% models not supported
geometry_tag(T) --> point_tag(T) ;
	linestring_tag(T) ;
	linearring_tag(T) ;
	polygon_tag(T) ;
	multigeometry_tag(T).

point_tag(T) --> 
	"<Point", id, ">", blank_star,
	point_elements(T), blank_star, 
	"</Point>".
linestring_tag(linestring(T)) -->
	"<LineString", id, ">", blank_star,
	linestring_elements(T), blank_star, 
	"</LineString>".
linearring_tag(linestring(T)) --> linearring(T).
linearring(T) -->
	"<LinearRing", id, ">", blank_star,
	linearring_elements(T), blank_star, 
	"</LinearRing>".
polygon_tag(polygon(T)) -->
	"<Polygon", id, ">", blank_star,
	polygon_elements(T), blank_star, 
	"</Polygon>".
multigeometry_tag(geometrycollection(T)) -->
	"<MultiGeometry", id, ">", blank_star,
	multigeometry_elements(T), blank_star,
	"</MultiGeometry>".

id --> [], !. % no ID support yet

point_elements(T) --> coordinate(T).
linestring_elements(T) --> coordinates(T).
linearring_elements(T) --> coordinates(T).
polygon_elements([O|I]) --> 
	aux_polygon_elements, blank_star,
	outer_boundary_tag(O), blank_star,
	aux_polygon_elements, blank_star,
	inner_boundary_tag(I), blank_star,
	aux_polygon_elements.
aux_polygon_elements --> [], !. % no other elements supported yet
multigeometry_elements([H|T]) --> 
	geometry_tag(H), blank_star, 
	multigeometry_elements(T). 
multigeometry_elements([]) --> [], !.

coordinate(T) --> "<coordinates>", blank_star, point(T), blank_star, "</coordinates>".
coordinates(T) --> "<coordinates>", blank_star, points(T), blank_star, "</coordinates>".

outer_boundary_tag(T) --> 
	"<outerBoundaryIs>", blank_star, 
	linearring(T), blank_star,
	"</outerBoundaryIs>".
inner_boundary_tag(T) -->
	"<innerBoundaryIs>", blank_star,
	inner_boundary_tags(T), blank_star,
	"</innerBoundaryIs>".
inner_boundary_tag([]) --> [], !.
inner_boundary_tags([H|T]) --> 
	linearring(H), blank_star, 
	inner_boundary_tags(T).
inner_boundary_tags([T]) -->
	linearring(T).

points([H|T]) --> point(H), points_star(T).
points_star(T) --> blank_plus, points(T).
points_star([]) --> [], !.

point(point(X,Y)) --> y(Y), ",", blank_star, x(X).
point(point(X,Y)) --> y(Y), ",", blank_star, x(X), ",", blank_star, "0".
x(X) --> float(X).
y(Y) --> float(Y).
blank_plus --> blank, blank_star, !.
blank_plus --> " ", !.
blank_star --> blanks, !.
blank_star --> [], !.


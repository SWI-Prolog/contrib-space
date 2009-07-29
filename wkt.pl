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

:- module(wkt,
	  [ wkt_shape/2
	  ]).

%%	wkt_shape(?WKT,?Shape) is semidet.
%
%	Converts between the WKT serialization of a Shape and
%	its native Prolog term representation.

wkt_shape(WKT,Shape) :-
	(   var(WKT)
	->  phrase(geometry_tagged_text(Shape), WKTlist),
	    concat_atom(WKTlist, WKT)
	;   tokenize_atom(WKT, WKTlist),
	    phrase(geometry_tagged_text(Shape), WKTlist)
	).

geometry_tagged_text(T) --> point_tagged_text(T) ;
	linestring_tagged_text(T) ;
	polygon_tagged_text(T) ;
	multipoint_tagged_text(T) ;
	multilinestring_tagged_text(T) ;
	multipolygon_tagged_text(T) ;
	geometrycollection_tagged_text(T).

point_tagged_text(T) --> 
	['POINT'], point_text(T).
linestring_tagged_text(linestring(T)) --> 
	['LINESTRING'], blank_star, linestring_text(T).
polygon_tagged_text(polygon(T)) -->
	['POLYGON'], blank_star, polygon_text(T).
multipoint_tagged_text(multipoint(T)) --> 
	['MULTIPOINT'], blank_star, multipoint_text(T).
multilinestring_tagged_text(multilinestring(T)) --> 
	['MULTILINESTRING'], blank_star, multilinestring_text(T).
multipolygon_tagged_text(multipolygon(T)) --> 
	['MULTIPOLYGON'], blank_star, multipolygon_text(T).
geometrycollection_tagged_text(geometrycollection(T)) --> 
	['GEOMETRYCOLLECTION'], blank_star, geometrycollection_text(T).

point_text(point(empty)) --> ['EMPTY'], !.
point_text(point(X,Y,Z,M)) --> 
	{nonvar(X)}, blank_plus, ['ZM'], blank_plus, ['('], blank_star, point(zm_point(X,Y,Z,M)), blank_star, [')'], ! ;
	blank_star, ['ZM'], blank_star, ['('], blank_star, point(zm_point(X,Y,Z,M)), blank_star, [')'].
point_text(point(X,Y,Z)) --> 
	{nonvar(X)}, blank_plus, ['Z'], blank_plus, ['('], blank_star, point(z_point(X,Y,Z)), blank_star, [')'], ! ;
	blank_star, ['Z'], blank_star, ['('], blank_star, point(z_point(X,Y,Z)), blank_star, [')'].
point_text(point(X,Y)) --> blank_star, ['('], blank_star, point(xy_point(X,Y)), blank_star, [')'].
linestring_text(empty) --> ['EMPTY'], !.
linestring_text(T) --> ['('], blank_star, points(T), blank_star, [')'].
polygon_text(empty) --> ['EMPTY'], !.
polygon_text(T) --> ['('], blank_star, linestring_texts(T), blank_star, [')'].
multipoint_text(empty) --> ['EMPTY'], !.
multipoint_text(T) --> ['('], blank_star, point_texts(T), blank_star, [')'].
multilinestring_text(empty) --> ['EMPTY'], !.
multilinestring_text(T) --> ['('], blank_star, linestring_texts(T), blank_star, [')'].
multipolygon_text(empty) --> ['EMPTY'], !.
multipolygon_text(T) --> ['('], blank_star, polygon_texts(T), blank_star, [')'].
geometrycollection_text(empty) --> ['EMPTY'], !.
geometrycollection_text(T) --> ['('], blank_star, geometry_tagged_texts(T), blank_star, [')'].

points([point(X,Y)|T]) --> point(xy_point(X,Y)), points_star(T).
points([point(X,Y,Z)|T]) --> point(z_point(X,Y,Z)), points_star(T).
points([point(X,Y,Z,M)|T]) --> point(zm_point(X,Y,Z,M)), points_star(T).
points_star(T) --> [','], blank_star, points(T).
points_star([]) --> [], !.
point_texts([H|T]) --> point_text(H), point_texts_star(T).
point_texts_star(T) -->  [','], blank_star, point_texts(T).
point_texts_star([]) --> [], !.
linestring_texts([H|T]) --> linestring_text(H), linestring_texts_star(T).
linestring_texts_star(T) --> [','], blank_star, linestring_texts(T).
linestring_texts_star([]) --> [], !.
polygon_texts([H|T]) --> polygon_text(H), polygon_texts_star(T).
polygon_texts_star(T) -->  [','], blank_star, polygon_texts(T).
polygon_texts_star([]) --> [], !.
geometry_tagged_texts([H|T]) --> geometry_tagged_text(H), geometry_tagged_texts_star(T).
geometry_tagged_texts_star(T) --> [','], blank_star, geometry_tagged_texts(T).
geometry_tagged_texts_star([]) --> [], !.

point(zm_point(X,Y,Z,M)) --> 
	{nonvar(X)}, c(X), blank_plus, c(Y), blank_plus, c(Z), blank_plus, c(M), ! ;
	c(X), blank_star, c(Y), blank_star, c(Z), blank_star, c(M).
point(z_point(X,Y,Z)) --> 
	{nonvar(X)}, c(X), blank_plus, c(Y), blank_plus, c(Z), ! ;
	c(X), blank_star, c(Y), blank_star, c(Z).
point(xy_point(X,Y)) --> 
	{nonvar(X)}, c(X), blank_plus, c(Y), ! ;
	c(X), blank_star, c(Y).
c(X) --> {var(X)}, [ X ].
c(X) --> {nonvar(X), atom(Xa), atom_number(Xa,X)}, [ Xa ].
c(X) --> {nonvar(X), number(X)}, [ X ].

blank_plus --> [' '], blank_star, !.
blank_star --> [], !.
blank_star --> blank_plus, !.


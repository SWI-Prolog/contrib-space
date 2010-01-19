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

:- module(space,
          [
           set_space/1,               % +Option
           set_space/2,               % +IndexName, +Option

	   space_index_all/1,         % +IndexNames
	   space_index_all/0,         % uses default index

           space_assert/3,            % +URI, +Shape, +IndexName
           space_assert/2,            % uses default index
           space_retract/3,           % +URI, +Shape, +IndexName
           space_retract/2,           % uses default index
           space_index/1,             % +IndexName
           space_index/0,             % uses default index
           space_clear/1,             % +IndexName
           space_clear/0,             % uses default index

           space_bulkload/2,          % +CandidatePred, +IndexName
           space_bulkload/1,          % +CandidatePred (uses default index and 'user' module)
           space_bulkload/0,          % also defaults to uri_shape

           space_contains/3,          % +Shape, -URI, +IndexName
           space_contains/2,          % uses default index
	   space_intersects/3,        % +Shape, -URI, +IndexName
	   space_intersects/2,        % uses default index
           space_nearest/3,           % +Shape, -URI, +IndexName
           space_nearest/2,           % uses default index
           space_nearest_bounded/4,   % +Shape, -URI, +WithinRange, +IndexName
           space_nearest_bounded/3,   % uses default index

	   shape/1,                   % +Shape
	   uri_shape/2,		      % ?URI, ?Shape
	   uri_shape/3,		      % ?URI, ?Shape, +Source
	   space_distance/3,           % +FromShape, +ToShape, -Distance

	   space_distance/3,              % +Shape1, +Shape2, -Distance
	   space_distance/4,              % +Shape1, +Shape2, -Distance, +IndexName
	   space_distance_pythagorean/3,  % +Shape1, +Shape2, -Distance
	   space_distance_greatcircle/4,  % +Shape1, +Shape2, -Distance, +Unit
	   space_distance_greatcircle/3   % +Shape1, +Shape2, -Distance (km)

          ]).

:- use_module(wkt).
:- use_module(kml).
:- use_module(georss). % also GML support
:- use_module(wgs84).
:- use_module(freebase).
:- use_module(library(error)).
:- use_module(library(shlib)).

:- dynamic space_queue/4.
:- dynamic uri_shape/2. % allows you to adapt space_index_all.

:- use_foreign_library(foreign(space)).


/** <module> Core spatial database

*/


%% configuration

:- dynamic space_setting_aux/1.
space_setting_aux(rtree_default_index(space_index)).

% foreign language predicate:
% set_space(IndexName,Option)

%%       set_space(+Option) is det.
%%       set_space(+IndexName,+Option) is det.
%
%        This predicate can be used to change the options of
%        a spatial index (or de default index for set_space/1).
%        Some options, like rtree_storage(S) where S is disk or memory
%        only have effect after clearing or bulkloading.
%        Others, take effect immediately on a running index.
%        More documentation will be provided in the near future.

set_space(Option) :-
        space_setting(rtree_default_index(I)),
        rtree_set_space(I, Option).

set_space(I, Option) :-
        rtree_set_space(I, Option).

/*
set_space(Option) :-
        functor(Option,Name,1),
        functor(Term,Name,1),
        with_mutex(space_mutex,
                   (   retractall(space_setting_aux(Term)),
                       assert(space_setting_aux(Option))
                   )).
*/

% FIXME: make bidirectional for settings stored in C++
space_setting(Option) :-
        with_mutex(space_mutex,space_setting_aux(Option)).

%%	space_assert(+URI,+Shape,+IndexName) is det.
%%	space_assert(+URI,+Shape) is det.
%
%	Insert URI with associated Shape in the queue to be inserted into
%       the index with name IndexName or the default index.
%       Indexing happens lazily at the next call of a query or
%       manually by calling space_index/1.

space_assert(URI,Shape) :-
	space_setting(rtree_default_index(I)),
	space_assert(URI,Shape,I).
space_assert(URI,Shape,IndexName) :-
	dimensionality(Shape,Dimensionality),
	must_be(between(1,3), Dimensionality),
	(   space_queue(IndexName,retract,_,_)
        ->  space_index(IndexName)
        ; true
        ),
        assert(space_queue(IndexName,assert,URI,Shape)).

%%	space_retract(+URI,+Shape,+IndexName) is det.
%%	space_retract(+URI,+Shape) is det.
%
%	Insert URI with associated Shape in the queue to be removed into
%       the index with name IndexName or the default index.
%       Indexing happens lazily at the next call of a query or
%       manually by calling space_index/1.

space_retract(URI,Shape) :-
	space_setting(rtree_default_index(I)),
	space_retract(URI,Shape,I).
space_retract(URI,Shape,IndexName) :-
	shape(Shape),
        (   space_queue(IndexName,assert,_,_)
        ->  space_index(IndexName)
        ; true
        ),
        assert(space_queue(IndexName,retract,URI,Shape)).

%%	space_index(+IndexName) is det.
%%	space_index is det.
%
%	Processes all asserts or retracts in the space queue for index
%	IndexName or the default index if no index is specified.

space_index :-
	space_setting(rtree_default_index(I)),
	space_index(I).
space_index(IndexName) :-
	(   space_queue(IndexName,assert,_,_)
	->  (   findall(object(URI,Shape), space_queue(IndexName,assert,URI,Shape), List),
	        rtree_insert_list(IndexName,List),
	        retractall(space_queue(IndexName,assert,_,_))
	    )
	;   (   space_queue(IndexName,retract,_,_)
	    ->  (   findall(object(URI,Shape), space_queue(IndexName,retract,URI,Shape), List),
		    rtree_delete_list(IndexName,List),
	            retractall(space_queue(IndexName,retract,_,_))
	        )
	    ;   true
	    )
	).

%%	space_clear(+IndexName) is det.
%%	space_clear is det.
%
%	Clears index IndexName or the default index if no index is
%	specified, removing all of its contents.

space_clear :-
	space_setting(rtree_default_index(I)),
	space_clear(I).
space_clear(IndexName) :-
        retractall(space_queue(IndexName,_,_,_)),
        rtree_clear(IndexName).

%%	space_bulkload(:Closure,+IndexName) is det.
%%	space_bulkload(:Closure) is det.
%%      space_bulkload is det.
%
%	Fast loading of many Shapes into the index IndexName.
%	Closure is called with two additional arguments:
%	URI and Shape, that finds candidate URI-Shape
%	pairs to index in the index IndexName.
%
%       space_bulkload/0 defaults to uri_shape/2 for :Closure.
%
%	@see the uri_shape/2 predicate for an example of a suitable functor.

:- meta_predicate space_bulkload(2), space_bulkload(2,+).

space_bulkload :-
        space_bulkload(uri_shape).
space_bulkload(Functor) :-
	space_setting(rtree_default_index(I)),
        space_bulkload(Functor,I).
space_bulkload(Functor,IndexName) :-
        once(call(Functor, _Uri, Shape)),
        dimensionality(Shape,Dimensionality),
	must_be(between(1,3), Dimensionality),
%	space_clear(IndexName),  % FIXME: is this ok to skip?
        rtree_bulkload(IndexName,Functor,Dimensionality).

%%	space_contains(+Shape,?Cont,+IndexName) is nondet.
%%	space_contains(+Shape,?Cont) is nondet.
%
%	Containment query. Unifies Cont with shapes contained in
%	Shape according to index IndexName or the default index.

space_contains(Shape,Cont) :-
	space_setting(rtree_default_index(I)),
	space_contains(Shape,Cont,I).
space_contains(Shape,Cont,IndexName) :-
	shape(Shape),
        space_index(IndexName),
	(   ground(Cont)
	->  (   bagof(Con, rtree_incremental_containment_query(Shape,Con,IndexName), Cons),
	        member(Cont, Cons), !
	    )
	;   rtree_incremental_containment_query(Shape,Cont,IndexName)
	).

%%	space_intersects(+Shape,?Inter,+IndexName) is nondet.
%%	space_intersects(+Shape,?Inter) is nondet.
%
%	Intersection query. Unifies Inter with shapes intersecting with
%	Shape according to index IndexName or the default index.
%	(intersection subsumes containment)

space_intersects(Shape,Inter) :-
	space_setting(rtree_default_index(I)),
	space_intersects(Shape,Inter,I).
space_intersects(Shape,Inter,IndexName) :-
	shape(Shape),
        space_index(IndexName),
	(   ground(Inter)
	->  (   bagof(In, rtree_incremental_intersection_query(Shape, In, IndexName), Ins),
		member(Inter, Ins), !
	    )
	;   rtree_incremental_intersection_query(Shape, Inter, IndexName)
	).


%%	space_nearest(+Shape,-Near,+IndexName) is nondet.
%%	space_nearest(+Shape,-Near) is nondet.
%
%	Incremental Nearest-Neighbor query. Unifies Near with shapes
%	in order of increasing distance to Shape according to index
%	IndexName or the default index.

space_nearest(Shape, Near) :-
	space_setting(rtree_default_index(I)),
	space_nearest(Shape,Near,I).
space_nearest(Shape, Near, IndexName) :-
	shape(Shape),
        space_index(IndexName),
	rtree_incremental_nearest_neighbor_query(Shape, Near, IndexName).

%%	space_nearest(+Shape,?Near,+WithinRange,+IndexName) is nondet.
%%	space_nearest(+Shape,?Near,+WithinRange) is nondet.
%
%	Incremental Nearest-Neighbor query with a bounded distance
%	scope. Unifies Near with shapes in order of increasing distance
%	to Shape according to index IndexName or the default index.
%	Fails when no more objects are within the range WithinRange.

space_nearest_bounded(Shape, Near, WithinRange) :-
	space_setting(rtree_default_index(I)),
	space_nearest_bounded(Shape,Near,WithinRange,I).
space_nearest_bounded(Shape, Near, WithinRange, _IndexName) :-
	shape(Shape),
        ground(Near),
        uri_shape(Near,NearShape),
        space_distance(Shape,NearShape,Distance),
        Distance < WithinRange, !.
space_nearest_bounded(Shape, Near, WithinRange, IndexName) :-
        space_index(IndexName),
        rtree_incremental_nearest_neighbor_query(Shape, Near, IndexName),
        uri_shape(Near,NearShape),
        space_distance(Shape,NearShape,Distance),
	(   Distance > WithinRange
	->  !, fail
	;   true
	).

space_display(IndexName) :-
        rtree_display(IndexName).

space_display_mbrs(IndexName) :-
        rtree_display_mbrs(IndexName).

%%	uri_shape(?URI,?Shape) is nondet.
%
%	Finds pairs of URIs and their corresponding Shapes based on
%	WGS84 RDF properties (e.g. wgs84:lat), GeoRSS Simple properties
%       (e.g. georss:polygon), and GeoRSS GML properties
%       (e.g. georss:where).
%
%	uri_shape/2 is a dynamic predicate, which means it can be
%	extended. If you use uri_shape/2 in this way, the URI argument
%	has to be a canonical URI, not a QName.

uri_shape(URI,Shape) :-
	georss_candidate(URI,Shape).
uri_shape(URI,Shape) :-
	wgs84_candidate(URI,Shape).
uri_shape(URI,Shape) :-
	freebase_candidate(URI,Shape).

%%	uri_shape(?URI,?Shape,+Source) is nondet.
%
%	Finds pairs of URIs and their corresponding Shapes using
%	uri_shape/2 from RDF that was loaded from a given Source.

uri_shape(URI,Shape,Source) :-
	georss_candidate(URI,Shape,Source).
uri_shape(URI,Shape,Source) :-
	wgs84_candidate(URI,Shape,Source).
uri_shape(URI,Shape,Source) :-
	freebase_candidate(URI,Shape,Source).

% allows you to use namespaces in the URI argument when using it to find pairs.
:- rdf_meta(uri_shape(r,?)).

%%	space_index_all(+IndexName) is det.
%%	space_index_all is det.
%
%	Loads all URI-Shape pairs found with uri_shape/2 into
%	index IndexName or the default index name.

space_index_all :-
	space_setting(rtree_default_index(IndexName)),
	space_index_all(IndexName).

space_index_all(IndexName) :-
	space_bulkload(uri_shape,IndexName).


/*
 * Utility predicates
 */

box_polygon(box(point(Lx,Ly),point(Hx,Hy)),
            polygon([[point(Lx,Ly),point(Lx,Hy),point(Hx,Hy),point(Hx,Ly),point(Lx,Ly)]])).

%%	shape(+Shape) is det.
%
%       Checks whether Shape is a valid supported shape.

shape(Shape) :- dimensionality(Shape,_).

dimensionality(Shape,Dim) :- ground(Shape) -> functor(Shape,point,Dim); !, fail.
dimensionality(box(Point,_),Dim) :- dimensionality(Point,Dim).
dimensionality(polygon([[Point|_]|_]),Dim) :- dimensionality(Point,Dim).
dimensionality(circle(Point,_,_),Dim) :- dimensionality(Point,Dim).
dimensionality(linestring([Point|_]),Dim) :- dimensionality(Point,Dim).
dimensionality(multipoint([Point|_]),Dim) :- dimensionality(Point,Dim).
dimensionality(multipolygon([Poly|_]),Dim) :- dimensionality(Poly,Dim).
dimensionality(multilinestring([LS|_]),Dim) :- dimensionality(LS,Dim).
dimensionality(geometrycollection([Geom|_]),Dim) :- dimensionality(Geom,Dim).

%%	space_distance(+Point1,+Point2,-Distance) is det.
%
%	Calculates the distance between Point1 and Point2
%	by default using pythagorean distance.
%
%	@see space_distance_greatcircle/4 for great circle distance.

space_distance(point(A1,A2), point(B1,B2), D) :-
	space_distance_pythagorean(point(A1,A2), point(B1,B2), D), !.

space_distance(A, B, D) :-
      	space_setting(rtree_default_index(IndexName)),
        rtree_distance(IndexName, A, B, D1),
	pythagorean_lat_long_to_kms(D1,D).

space_distance(A, B, D, IndexName) :-
        rtree_distance(IndexName, A, B, D1),
	pythagorean_lat_long_to_kms(D1,D).

space_distance_pythagorean(A,B,D) :-
	space_distance_pythagorean_fastest(A,B,D1),
	pythagorean_lat_long_to_kms(D1,D).

space_distance_pythagorean_fastest(point(A, B), point(X, Y), D) :-
	D2 is ((X - A) ** 2) + ((Y - B) ** 2),
	D is sqrt(D2).

pythagorean_lat_long_to_kms(D1,D) :-
	D is D1 * 111.195083724. % to kms



%%	space_distance_greatcircle(+Point1,+Point2,-Dist) is det.
%%	space_distance_greatcircle(+Point1,+Point2,-Dist,+Unit) is det.
%
%	Calculates great circle distance between Point1 and Point2
%	in the specified Unit, which can take as a value km (kilometers)
%	or nm (nautical miles). By default, nautical miles are used.

space_distance_greatcircle(point(A1,A2), point(B1,B2), D) :-
	space_distance_greatcircle(point(A1,A2), point(B1,B2), D, nm).

space_distance_greatcircle(point(A1,A2), point(B1,B2), D, km) :-
	R is 6371, % kilometers
	space_distance_greatcircle_aux(point(A1,A2), point(B1,B2), D, R).

space_distance_greatcircle(point(A1,A2), point(B1,B2), D, nm) :-
	R is 3440.06, % nautical miles
	space_distance_greatcircle_aux(point(A1,A2), point(B1,B2), D, R).

% Haversine formula
space_distance_greatcircle_aux(point(Lat1deg, Long1deg), point(Lat2deg, Long2deg), D, R) :-
	deg2rad(Lat1deg,Lat1),
	deg2rad(Lat2deg,Lat2),
	deg2rad(Long1deg,Long1),
	deg2rad(Long2deg,Long2),
	DLat is Lat2 - Lat1,
	DLong is Long2 - Long1,
	A is (sin(DLat/2)**2) + cos(Lat1) * cos(Lat2) * (sin(DLong/2)**2),
	SqA is sqrt(A),
	OnemA is 1 - A,
	Sq1mA is sqrt(OnemA),
	C is 2 * atan(SqA,Sq1mA),
	D is R * C.


deg2rad(Deg,Rad) :-
	Rad is (Deg * pi) / 180.










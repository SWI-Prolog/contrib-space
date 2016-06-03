:- module(
  space,
  [
    set_space/1,              % +Opt
    set_space/2,              % +Index, +Opt
    space_setting/1,          % ?Opt
                              
    space_index_all/0,        
    space_index_all/1,        % +Indexs
                              
    space_assert/2,           % +Res, +Shape
    space_assert/3,           % +Res, +Shape, +Index
    space_retract/2,          % +Res, +Shape
    space_retract/3,          % +Res, +Shape, +Index
    space_index/0,            
    space_index/1,            % +Index
    space_clear/0,            
    space_clear/1,            % +Index
                              
    space_bulkload/0,         
    space_bulkload/1,         % +Goal_0
    space_bulkload/2,         % :Goal_0, +Index
                              
    space_contains/2,         % +Query, -Res
    space_contains/3,         % +Query, -Res, +Index
    space_intersects/2,       % +Query, -Res
    space_intersects/3,       % +Query, -Res, +Index
    space_nearest/2,          % +Query, -Res
    space_nearest/3,          % +Query, -Res, +Index
    space_within_range/3,     % +Query, -Res, +WithinRange
    space_within_range/4,     % +Query, -Res, +WithinRange, +Index
    space_nearest_bounded/3,  % +Query, -Res, +WithinRange
    space_nearest_bounded/4,  % +Query, -Res, +WithinRange, +Index
    
    shape/1,                  % +Shape
    uri_shape/2,              % ?Res, ?Shape
    uri_shape/3,              % ?Res, ?Shape, ?G
    
    space_dist/3,             % +Feature1, +Feature2, -Dist
    space_dist/4,             % +Feature1, +Feature2, -Dist, +Index
    space_dist_pythagorean/3, % +Feature1, +Feature2, -Dist
    space_dist_greatcircle/3, % +Feature1, +Feature2, -Dist (nm)
    space_dist_greatcircle/4, % +Feature1, +Feature2, -Dist, +Unit
    
    space_bearing/3           % +Point1, +Point2, -Heading (degrees)
  ]
).

:- use_module(library(error)).
:- use_module(library(shlib)).

:- use_module(dbpedia).
:- use_module(freebase).
:- use_module(georss). % Also contains GML support.
:- use_module(kml).
:- use_module(wgs84).

:- use_foreign_library(space).

:- dynamic
    space_queue/4,
    % allows you to adapt space_index_all.
    shape/1,
    % allows you to adapt space_index_all.
    uri_shape/2,
    % allows you to adapt space_index_all.
    uri_shape/3.

:- meta_predicate
    space_bulkload(2),
    space_bulkload(2,+).

:- rdf_meta
   space_nearest(t,?),
   space_nearest(t,?,?),
   space_intersects(t,?),
   space_intersects(t,?,?),
   space_contains(t,?),
   space_contains(t,?,?),
   space_nearest_bounded(t,?,?),
   space_nearest_bounded(t,?,?,?),
   space_within_range(t,?,?),
   space_within_range(t,?,?,?),
   space_assert(r,?),
   space_assert(r,?,?),
   space_retract(r,?),
   space_retract(r,?,?),
   uri_shape(r,?),
   uri_shape(r,?,?).

/** <module> Core spatial database

@author Willem van Hage
@version 2009-2012

@author Wouter Beek
@version 2016/06
*/

:- dynamic
    space_setting_aux/1.

space_setting_aux(rtree_default_index(space_index)).

% foreign language predicate:
% set_space(Index,Opt)





%! set_space(+Opt) is det.
%! set_space(+Opt, +Index) is det.
%
% Change the options of Index (or the default index for set_space/1).
% Some options, like `rtree_storage(disk)` or `rtree_storage(memory)`
% only have effect after clearing or bulkloading.  Other options take
% effect immediately.

set_space(Opt) :-
  space_setting(rtree_default_index(I)),
  set_space(Opt, I).

set_space(Opt, I) :-
  rtree_set_space(I, Opt).

/*
set_space(Opt) :-
  functor(Opt, Name, 1),
  functor(Term, Name, 1),
  with_mutex(space_mutex, (
    retractall(space_setting_aux(Term)),
    assert(space_setting_aux(Opt))
  )).
*/

% FIXME: make bidirectional for settings stored in C++
space_setting(Opt) :-
  with_mutex(space_mutex, space_setting_aux(Opt)).


%! space_assert(+Res, +Shape) is det.
%! space_assert(+Res, +Shape, +Index) is det.
%
% Insert resource Res with associated Shape into the queue that is to
% be inserted into the index with name Index (or the default
% index).  Indexing happens lazily at the next call of a query or
% manually by calling space_index/1.

space_assert(Res, Shape) :-
  space_setting(rtree_default_index(I)),
  space_assert(Res, Shape, I).


space_assert(Res, Shape, I) :-
  shape(Shape),
  % @tdb Why conditional?  Why update index before assert?
  (space_queue(I,retract,_,_) -> space_index(I) ; true),
  assert(space_queue(I,assert,Res,Shape)).



%! space_retract(+Res, +Shape) is det.
%! space_retract(+Res, +Shape, +Index) is det.
%
% Insert resource Res with associated Shape in the queue that is to be
% removed from the index with name Index (or the default index).
% Indexing happens lazily at the next call of a query or manually by
% calling space_index/1.

space_retract(Res, Shape) :-
  space_setting(rtree_default_index(I)),
  space_retract(Res, Shape, I).


space_retract(Res, Shape, I) :-
  shape(Shape),
  (space_queue(I, assert, _, _) -> space_index(I) ; true),
  assert(space_queue(I,retract,Res,Shape)).



%! space_index is det.
%! space_index(+Index) is det.
%
% Processes all asserts or retracts in the space queue for index
% Index (or the default index).

space_index :-
  space_setting(rtree_default_index(I)),
  space_index(I).


space_index(I) :-
  space_queue(I, assert ,_ ,_), !,
  empty_nb_set(Assertions),
  findall(
    object(Res,Shape),
    (
      space_queue(I, assert, Res, Shape),
      add_nb_set(space_assert(Res,Shape), Assertions)
    ),
    L
  ),
  rtree_insert_list(I, L),
  retractall(space_queue(I,assert,_,_)),
  size_nb_set(Assertions,N),
  format(user_output, "% Added ~w Res-Shape pairs to ~w~n", [N,I]),
  space_index(I).
space_index(I) :-
  space_queue(I, retract, _, _), !,
  empty_nb_set(Retractions),
  findall(
    object(Res,Shape),
    (
      space_queue(I, retract, Res, Shape),
      add_nb_set(space_retract(Res,Shape), Retractions)
    ),
    L
  ),
  rtree_delete_list(I, L),
  retractall(space_queue(I,retract,_,_)),
  size_nb_set(Retractions, N),
  format(user_output, "% Removed ~w Res-Shape pairs from ~w~n", [N,I]),
  space_index(I).
space_index(_).



%! space_clear is det.
%! space_clear(+Index) is det.
%
% Clears Index (or the default index), removing all of its contents.

space_clear :-
  space_setting(rtree_default_index(I)),
  space_clear(I).


space_clear(I) :-
  retractall(space_queue(I,_,_,_)),
  rtree_clear(I).



%! space_bulkload is det.
%! space_bulkload(:Goal_0) is det.
%! space_bulkload(:Goal_0, +Index) is det.
%
% Fast loading of many Shapes into the given Index.  Goal_2 finds
% candidate Res-Shape pairs to add to the Index.
%
% The default Goal_2 is uri_shape/2.

space_bulkload :-
  space_bulkload(uri_shape).


space_bulkload(Goal_2) :-
  space_setting(rtree_default_index(I)),
  space_bulkload(Goal_2, I).


space_bulkload(Goal_2, I) :-
  once(call(Goal_2, _, Shape)),
  dimensionality(Shape, Dim),
  % space_clear(I),  % FIXME: is this ok to skip?
  rtree_bulkload(I, Goal_2, Dim).



%! space_contains(+Query, ?Cont) is nondet.
%! space_contains(+Query, ?Cont, +Index) is nondet.
%
% Containment query, unifying Cont with shapes contained in the Query
% shape (or shape of Query Res).

space_contains(Query, Cont) :-
  space_setting(rtree_default_index(I)),
  space_contains(Query, Cont, I).


space_contains(Query, Cont, I) :-
  (shape(Query) -> Shape = Query ; uri_shape(Query, Shape)),
  space_index(I),
  (   ground(Cont)
  ->  bagof(Con, rtree_incremental_containment_query(Shape, Con, I), Cons),
      Cons = [Cont|_]
  ;   rtree_incremental_containment_query(Shape, Cont, I)
  ).



%! space_intersects(+Query, ?Inter) is nondet.
%! space_intersects(+Query, ?Inter, +Index) is nondet.
%
% Intersection query, unifying Inter with shapes that intersect with
% the Query shape (or with the shape of Query resource).
%
% Intersection subsumes containment.

space_intersects(Query, Inter) :-
  space_setting(rtree_default_index(I)),
  space_intersects(Query, Inter, I).


space_intersects(Query, Inter, I) :-
  (shape(Query) -> Shape = Query ; uri_shape(Query, Shape)),
  space_index(I),
  (   ground(Inter)
  ->  bagof(In, rtree_incremental_intersection_query(Shape, In, I), Ins),
      Ins = [Inter|_]
  ;   rtree_incremental_intersection_query(Shape, Inter, I)
  ).



%! space_nearest(+Query, -Near) is nondet.
%! space_nearest(+Query, -Near, +Index) is nondet.
%
% Incremental Nearest-Neighbor query, unifying Near with shapes in
% order of increasing distance to the Query shape (or to the shape of
% the resource Query).

space_nearest(Query, Near) :-
  space_setting(rtree_default_index(I)),
  space_nearest(Query, Near, I).


space_nearest(Query, Near, I) :-
  (shape(Query) -> Shape = Query ; uri_shape(Query, Shape)),
  space_index(I),
  rtree_incremental_nearest_neighbor_query(Shape, Near, I).



%! space_nearest(+Query, ?Near, +WithinRange) is nondet.
%! space_nearest(+Query, ?Near, +WithinRange, +Index) is nondet.
%
% Incremental Nearest-Neighbor query with a bounded distance scope.
% Unifies Near with shapes in order of increasing distance to Query
% Shape (or Shape of Query Res) according to index Index or the
% default index.  Fails when no more objects are within the range
% WithinRange.

space_nearest_bounded(Query, Near, WithinRange) :-
  space_setting(rtree_default_index(I)),
  space_nearest_bounded(Query, Near, WithinRange, I).


space_nearest_bounded(Query, Near, WithinRange, I) :-
  (shape(Query) -> Shape = Query ; uri_shape(Query, Shape)),
  (   ground(Near)
  ->  uri_shape(Near, NearShape),
      space_dist(Shape, NearShape, Dist),
      Dist < WithinRange
  ;   space_index(I),
      rtree_incremental_nearest_neighbor_query(Shape, Near, I),
      (uri_shape(Near, NearShape, I) -> true ; uri_shape(Near,NearShape)), %?
      space_dist(Shape, NearShape, Dist),
      (   ground(WithinRange)
      ->  (Dist > WithinRange -> !, fail ; true)
      ;   WithinRange = Dist
      )
  ).


%! space_nearest(+Query, ?Near, +WithinRange) is nondet.
%! space_nearest(+Query, ?Near, +WithinRange, +Index) is nondet.
%
% Alias for OGC compatibility.

space_within_range(Query, Near, WithinRange) :-
  space_nearest_bounded(Query, Near, WithinRange).
space_within_range(Query, Near, WithinRange, I) :-
  space_nearest_bounded(Query, Near, WithinRange, I).



space_display(I) :-
  rtree_display(I).



space_display_mbrs(I) :-
  rtree_display_mbrs(I).



%! uri_shape(?Res, ?Shape) is nondet.
%! uri_shape(?Res, ?Shape, ?G) is nondet.
%
% Succeeds if resource Res has geographic Shape.  Shape can be on of
% the following:
%
%   - WGS84 RDF properties (e.g. `wgs84:lat`)
%
%   - GeoRSS Simple properties (e.g. `georss:polygon`)
%
%   - GeoRSS GML properties (e.g. `georss:where`)
%
% This predicate can be dynamically extended.
%
% @tbd Separate dynamicity through hook.

uri_shape(Res, Shape) :-
  uri_shape(Res, Shape, _).


uri_shape(Res, Shape, G) :-
  georss_candidate(Res, Shape, G).
uri_shape(Res, Shape, G) :-
  wgs84_candidate(Res, Shape, G).
uri_shape(Res, Shape, G) :-
  freebase_candidate(Res, Shape, G).
uri_shape(Res, Shape, G) :-
  dbpedia_candidate(Res, Shape, G).
uri_shape(Res, Shape, G) :-
  (var(G) -> space_setting(rtree_default_index(Index)), G = Index ; true),
  rtree_uri_shape(Res, S, G),
  Shape = S. % @tbd: fix in C++



%!  space_index_all is det.
%!  space_index_all(+Index) is det.
%
% Loads all resource-shape pairs found with uri_shape/2 into Index (or
% the default index).

space_index_all :-
  space_setting(rtree_default_index(I)),
  space_index_all(I).


space_index_all(I) :-
  space_bulkload(uri_shape,I).





% HELPERS %

box_polygon(
  box(point(Lx,Ly),point(Hx,Hy)),
  polygon([[point(Lx,Ly),point(Lx,Hy),point(Hx,Hy),point(Hx,Ly),point(Lx,Ly)]])
).



%! shape(+Shape) is det.
%
% Checks whether Shape is a valid and supported shape.

shape(Shape) :-
  dimensionality(Shape, Dim),
  must_be(between(1,3), Dim).



%! dimensionality(+Shape, -Dim) is det.

dimensionality(Shape, Dim) :-
  functor(Shape, point, Dim), !.
dimensionality(box(Point,_),Dim) :- !,
  dimensionality(Point,Dim).
dimensionality(circle(Point,_,_),Dim) :- !,
  dimensionality(Point,Dim).
dimensionality(geometrycollection([Geom|_]),Dim) :- !,
  dimensionality(Geom,Dim).
dimensionality(linestring([Point|_]),Dim) :- !,
  dimensionality(Point,Dim).
dimensionality(multipoint([Point|_]),Dim) :- !,
  dimensionality(Point,Dim).
dimensionality(multipolygon([Poly|_]),Dim) :- !,
  dimensionality(Poly,Dim).
dimensionality(multilinestring([LS|_]),Dim) :- !,
  dimensionality(LS,Dim).
dimensionality(polygon([[Point|_]|_]),Dim) :- !,
  dimensionality(Point,Dim).



%! space_dist(+Point1, +Point2, -Dist) is det.
%
% Calculates the Pythagorian Dist between Point1 and Point2.
%
% @see space_dist_greatcircle/4 for great circle distance.

space_dist(X, Y, Dist) :-
  space_dist(X, Y, Dist, _).

space_dist(X, X, 0, _).
space_dist(X, Y, Dist, G) :-
  (rdf_is_iri(X) -> uri_shape(X, Xs, G) ; Xs = X),
  (rdf_is_iri(Y) -> uri_shape(Y, Ys, G) ; Ys = Y),
  space_dist_shape(Xs, Ys, Dist, G).


space_dist_shape(point(X1,X2), point(Y1,Y2), Dist) :-
  space_dist_pythagorean(point(X1,X2), point(Y1,Y2), Dist), !.
space_dist_shape(X, Y, Dist, I) :-
  rtree_distance(I, X, Y, Dist0),
  pythagorean_lat_long_to_kms(Dist0, Dist).


% for speed, first assume X and Y are shapes, not resources.  If this
% fails, proceed to interpret them as resources.
space_dist_pythagorean(X, Y, D) :-
  space_dist_pythagorean_fastest(X, Y, D1),
  pythagorean_lat_long_to_kms(D1, D).
space_dist_pythagorean(X, Y, D) :-
  (atom(X) -> uri_shape(X, Xs) ; Xs = X),
  (atom(Y) -> uri_shape(Y, Ys) ; Ys = Y),
  space_dist_pythagorean(Xs, Ys, D).

space_dist_pythagorean_fastest(point(A, B), point(X, Y), D) :-
  D2 is ((X - A) ** 2) + ((Y - B) ** 2),
  D is sqrt(D2).

pythagorean_lat_long_to_kms(D1, D) :-
  D is D1 * 111.195083724. % to kms


%!  space_dist_greatcircle(+Point1,+Point2,-Dist) is det.
%!  space_dist_greatcircle(+Point1,+Point2,-Dist,+Unit) is det.
%
%  Calculates great circle distance between Point1 and Point2
%  in the specified Unit, which can take as a value km (kilometers)
%  or nm (nautical miles). By default, nautical miles are used.

space_dist_greatcircle(A, B, D) :-
  (   atom(A)
  ->  uri_shape(A, As)
  ;   As = A
  ),
  (   atom(B)
  ->  uri_shape(B, Bs)
  ;   Bs = B
  ),
  space_shape_dist_greatcircle(As, Bs, D).

space_dist_greatcircle(A, B, D, Unit) :-
  (   atom(A)
  ->  uri_shape(A, As)
  ;   As = A
  ),
  (   atom(B)
  ->  uri_shape(B, Bs)
  ;   Bs = B
  ),
  space_shape_dist_greatcircle(As, Bs, D, Unit).


space_shape_dist_greatcircle(point(A1,A2), point(B1,B2), D) :-
  space_shape_dist_greatcircle(point(A1,A2), point(B1,B2), D, nm).

space_shape_dist_greatcircle(point(A1,A2), point(B1,B2), D, km) :-
  R is 6371, % kilometers
  space_dist_greatcircle_aux(point(A1,A2), point(B1,B2), D, R).
space_shape_dist_greatcircle(point(A1,A2), point(B1,B2), D, nm) :-
  R is 3440.06, % nautical miles
  space_dist_greatcircle_aux(point(A1,A2), point(B1,B2), D, R).

% Haversine formula
space_dist_greatcircle_aux(point(Lat1deg, Long1deg), point(Lat2deg, Long2deg), D, R) :-
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
rad2deg(Rad,Deg) :-
  Deg is (Rad * 180) / pi.

space_bearing(point(Lat1deg, Long1deg), point(Lat2deg, Long2deg), Bearing) :-
  deg2rad(Lat1deg,Lat1),
  deg2rad(Lat2deg,Lat2),
  deg2rad(Long1deg,Long1),
  deg2rad(Long2deg,Long2),
  DLong is Long2 - Long1,
  Y is sin(DLong) * cos(Lat2),
  X is cos(Lat1) * sin(Lat2) - sin(Lat1) * cos(Lat2) * cos(DLong),
  Bearing0 is atan(Y, X),
  rad2deg(Bearing0, Bearing).

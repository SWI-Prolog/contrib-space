:- module(
  gis,
  [
    set_space/1,            % +Opt
    set_space/2,            % +Index, +Opt
    gis_setting/1,          % ?Opt
                              
    gis_populate_index/0,        
    gis_populate_index/1,   % +Index

    gis_default_index/1,    % -Index
    gis_assert/2,           % +Res, +Shape
    gis_assert/3,           % +Res, +Shape, +Index
    gis_retract/2,          % +Res, +Shape
    gis_retract/3,          % +Res, +Shape, +Index
    gis_update_index/0,            
    gis_update_index/1,     % +Index
    gis_clear/0,            
    gis_clear/1,            % +Index
    gis_queue/2,            % +Index, +Mode
    gis_queue/4,            % +Index, +Mode, ?Res, ?Shape
    
    gis_contains/2,         % +Query, -Res
    gis_contains/3,         % +Query, -Res, +Index
    gis_intersects/2,       % +Query, -Res
    gis_intersects/3,       % +Query, -Res, +Index
    gis_nearest/2,          % +Query, -Res
    gis_nearest/3,          % +Query, -Res, +Index
    gis_within_range/3,     % +Query, -Res, +WithinRange
    gis_within_range/4,     % +Query, -Res, +WithinRange, +Index
    gis_nearest_bounded/3,  % +Query, -Res, +WithinRange
    gis_nearest_bounded/4,  % +Query, -Res, +WithinRange, +Index
    
    gis_is_shape/1,         % +Shape
    resource_shape/2,       % ?Res, ?Shape
    resource_shape/3,       % ?Res, ?D, ?Shape
    resource_shape/5,       % ?Res, ?D, ?Shape, ?G, ?Index
    
    gis_dist/3,             % +Feature1, +Feature2, -Dist
    gis_dist/4,             % +Feature1, +Feature2, -Dist, +Index
    gis_dist_pythagorean/3, % +Feature1, +Feature2, -Dist
    gis_dist_greatcircle/3, % +Feature1, +Feature2, -Dist (nm)
    gis_dist_greatcircle/4, % +Feature1, +Feature2, -Dist, +Unit
    
    gis_bearing/3           % +Point1, +Point2, -Heading (degrees)
  ]
).

/** <module> Geographic Information System (GIS)

@author Willem van Hage
@version 2009-2012

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(shlib)).

:- use_foreign_library(space).

:- dynamic
    gis:resource_shape_hook/4,
    gis:gis_setting/1,
    gis_queue0/4.

:- multifile
   gis:resource_shape_hook/4.

gis:gis_setting(rtree_default_index(default)).

:- rdf_meta
   gis_assert(r, ?),
   gis_assert(r, ?, ?),
   gis_contains(r, r),
   gis_contains(r, r, +),
   gis_intersects(r, r),
   gis_intersects(r, r, ?),
   gis_nearest(r, r),
   gis_nearest(r, r, ?),
   gis_nearest_bounded(r, r, r),
   gis_nearest_bounded(r, r, r, ?),
   gis_queue(?, ?, r, ?),
   gis_retract(r, ?),
   gis_retract(r, ?, ?),
   resource_shape(r, ?),
   resource_shape(r, r, ?),
   resource_shape(r, r, ?, r, ?),
   gis_within_range(r, r, ?),
   gis_within_range(r, r, ?, ?).

:- debug(space(index)).





%! gis_default_index(-Index) is det.

gis_default_index(Index) :-
  gis_setting(rtree_default_index(Index)).



%! set_space(+Opt) is det.
%! set_space(+Opt, +Index) is det.
%
% Change the options of Index (or the default index for set_space/1).
% Some options, like `rtree_storage(disk)` or `rtree_storage(memory)`
% only have effect after clearing or bulkloading.  Other options take
% effect immediately.

set_space(Opt) :-
  gis_default_index(Index),
  set_space(Opt, Index).


set_space(Opt, Index) :-
  rtree_set_space(Index, Opt).



%! gis_assert(+Res, +Shape) is det.
%! gis_assert(+Res, +Shape, +Index) is det.
%
% Insert resource Res with associated Shape into the queue that is to
% be inserted into the given Index (or the default index).  Indexing
% happens lazily at the next call of a query or manually by calling
% gis_update_index/1.

gis_assert(Res, Shape) :-
  gis_default_index(Index),
  gis_assert(Res, Shape, Index).


gis_assert(Res, Shape, Index) :-
  gis_is_shape(Shape),
  % First process all queued retracts, since these may otherwise
  % inadvertently remove the newly asserted fact.
  (gis_queue(Index, retract) -> gis_update_index(Index) ; true),
  assert(gis_queue0(Index,assert,Res,Shape)).



%! gis_retract(+Res, +Shape) is det.
%! gis_retract(+Res, +Shape, +Index) is det.
%
% Insert resource Res with associated Shape in the queue that is to be
% removed from the given Index (or the default index).  Indexing
% happens lazily at the next call of a query or manually by calling
% gis_update_index/1.

gis_retract(Res, Shape) :-
  gis_default_index(Index),
  gis_retract(Res, Shape, Index).


gis_retract(Res, Shape, Index) :-
  gis_is_shape(Shape),
  (gis_queue(Index, assert) -> gis_update_index(Index) ; true),
  assert(gis_queue0(Index,retract,Res,Shape)).



%! gis_update_index is det.
%! gis_update_index(+Index) is det.
%
% Processes all asserts or retracts in the space queue for the given
% Index (or for the default index).

gis_update_index :-
  gis_default_index(Index),
  gis_update_index(Index).


gis_update_index(Index) :-
  gis_queue0(Index, assert ,_ ,_), !,
  empty_nb_set(Assertions),
  findall(
    object(Res,Shape),
    (
      gis_queue0(Index, assert, Res, Shape),
      add_nb_set(gis_assert(Res,Shape), Assertions)
    ),
    L
  ),
  rtree_insert_list(Index, L),
  retractall(gis_queue0(Index,assert,_,_)),
  % @tbd Why the need for `Assertions`?
  size_nb_set(Assertions,N),
  debug(space(index), "% Added ~w Res-Shape pairs to ~w", [N,Index]),
  gis_update_index(Index).
gis_update_index(Index) :-
  gis_queue0(Index, retract, _, _), !,
  empty_nb_set(Retractions),
  findall(
    object(Res,Shape),
    (
      gis_queue0(Index, retract, Res, Shape),
      add_nb_set(gis_retract(Res,Shape), Retractions)
    ),
    L
  ),
  rtree_delete_list(Index, L),
  retractall(gis_queue0(Index,retract,_,_)),
  size_nb_set(Retractions, N),
  debug(space(index), "% Removed ~w Res-Shape pairs from ~w", [N,Index]),
  gis_update_index(Index).
gis_update_index(_).



%! gis_clear is det.
%! gis_clear(+Index) is det.
%
% Clears the given Index (or the default index), removing all of its
% contents.

gis_clear :-
  gis_default_index(Index),
  gis_clear(Index).


gis_clear(Index) :-
  retractall(gis_queue0(Index,_,_,_)),
  rtree_clear(Index).



%! gis_contains(+Query, ?Shape) is nondet.
%! gis_contains(+Query, ?Shape, +Index) is nondet.
%
% Containment query, unifying Cont with shapes contained in the Query
% shape (or shape of Query Res).

gis_contains(Query, Shape) :-
  gis_default_index(Index),
  gis_contains(Query, Shape, Index).


gis_contains(Query, Shape2, Index) :-
  resource_shape(Query, Shape1),
  gis_update_index(Index),
  (   ground(Shape2)
  ->  bagof(
        Shape0,
        rtree_incremental_containment_query(Shape1, Shape0, Index),
        Shape0s
      ),
      memberchk(Shape2, Shape0s)
  ;   rtree_incremental_containment_query(Shape1, Shape2, Index)
  ).



%! gis_intersects(+Query, ?Inter) is nondet.
%! gis_intersects(+Query, ?Inter, +Index) is nondet.
%
% Intersection query, unifying Inter with shapes that intersect with
% the Query shape (or with the shape of Query resource).
%
% Intersection subsumes containment.

gis_intersects(Query, Shape) :-
  gis_default_index(Index),
  gis_intersects(Query, Shape, Index).


gis_intersects(Query, Shape2, Index) :-
  resource_shape(Query, Shape1),
  gis_update_index(Index),
  (   ground(Shape2)
  ->  % @tbd Apparently C++ cannot receive run-time instantiated
      % arguments?
      bagof(
        Shape0,
        rtree_incremental_intersection_query(Shape1, Shape0, Index),
        Shape0s
      ),
      memberchk(Shape2, Shape0s)
  ;   rtree_incremental_intersection_query(Shape1, Shape2, Index)
  ).



%! gis_nearest(+Query, -Shape) is nondet.
%! gis_nearest(+Query, -Shape, +Index) is nondet.
%
% Incremental Nearest-Neighbor query, unifying Shape with shapes in
% order of increasing distance to the Query shape (or to the shape of
% the resource Query).

gis_nearest(Query, Shape) :-
  gis_default_index(Index),
  gis_nearest(Query, Shape, Index).


gis_nearest(Query, Shape2, Index) :-
  resource_shape(Query, Shape1),
  gis_update_index(Index),
  rtree_incremental_nearest_neighbor_query(Shape1, Shape2, Index).



%! gis_nearest(+Query, ?Near, +WithinRange) is nondet.
%! gis_nearest(+Query, ?Near, +WithinRange, +Index) is nondet.
%
% Incremental Nearest-Neighbor query with a bounded distance scope.
% Unifies Near with shapes in order of increasing distance to Query
% Shape (or Shape of Query Res) according to Index or the default
% index.  Fails when no more objects are within the range WithinRange.

gis_nearest_bounded(Query, Near, WithinRange) :-
  gis_default_index(Index),
  gis_nearest_bounded(Query, Near, WithinRange, Index).


gis_nearest_bounded(Query, Near, WithinRange, Index) :-
  resource_shape(Query, Shape),
  (   ground(Near)
  ->  resource_shape(Near, NearShape),
      gis_dist(Shape, NearShape, Dist),
      Dist < WithinRange
  ;   gis_update_index(Index),
      rtree_incremental_nearest_neighbor_query(Shape, Near, Index),
      resource_shape(Near, _, NearShape, _, Index),
      gis_dist(Shape, NearShape, Dist),
      (   ground(WithinRange)
      ->  (Dist > WithinRange -> !, fail ; true)
      ;   WithinRange = Dist
      )
  ).



%! gis_queue(+Index, ?Mode:oneof([assert,retract])) is nondet.
%! gis_queue(+Index, ?Mode:oneof([assert,retract]), ?Res, ?Shape) is nondet.

gis_queue(Index, Mode) :-
  once(gis_queue(Index, Mode, _, _)).


gis_queue(Index, Mode, Res, Shape) :-
  gis_queue0(Index, Mode, Res, Shape).



%! gis_nearest(+Query, ?Near, +WithinRange) is nondet.
%! gis_nearest(+Query, ?Near, +WithinRange, +Index) is nondet.
%
% Alias for OGC compatibility.

gis_within_range(Query, Near, WithinRange) :-
  gis_nearest_bounded(Query, Near, WithinRange).


gis_within_range(Query, Near, WithinRange, Index) :-
  gis_nearest_bounded(Query, Near, WithinRange, Index).



gis_display(Index) :-
  rtree_display(Index).



gis_display_mbrs(Index) :-
  rtree_display_mbrs(Index).



%! resource_shape(?Res, ?Shape) is nondet.
%! resource_shape(?Res, ?D, ?Shape) is nondet.
%! resource_shape(?Res, ?D, ?Shape, ?G, ?Index) is nondet.
%
% Succeeds if resource Res has a geographic Shape.
%
% This predicate can be dynamically extended through the
% gis:resource_shape_hook/4.

resource_shape(Res, Shape) :-
  resource_shape(Res, _, Shape).


resource_shape(Res, D, Shape) :-
  gis_default_index(Index),
  resource_shape(Res, D, Shape, _, Index).


% Exceptional case to allow resources and shapes to be supplied as
% arguments to the same predicates.
resource_shape(Res, _, Shape, _, _) :-
  ground(Res),
  gis_is_shape(Res), !,
  Shape = Res.
resource_shape(Res, D, Shape, G, _) :-
  gis:resource_shape_hook(Res, D, Shape, G).
resource_shape(Res, _, Shape, _, Index0) :-
  (var(Index0) -> gis_default_index(Index) ; Index = Index0),
  rtree_uri_shape(Res, Shape0, Index),
  Shape = Shape0. % @tbd: fix in C++



%!  gis_populate_index is det.
%!  gis_populate_index(+Index) is det.
%
% Loads all resource-shape pairs found with resource_shape/5 into the
% given Index (or the default index).

gis_populate_index :-
  gis_default_index(Index),
  gis_populate_index(Index).


gis_populate_index(Index) :-
  once(resource_shape(_, _, Shape, _, Index)),
  dimensionality(Shape, Dim),
  rtree_bulkload(Index, uri_shape, Dim).

system:uri_shape(X, Y) :-
  resource_shape(X, Y),
  debug(space(index), "[LOAD] ~w ~w~n", [X,Y]).





% HELPERS %

box_polygon(
  box(point(Lx,Ly),point(Hx,Hy)),
  polygon([[point(Lx,Ly),point(Lx,Hy),point(Hx,Hy),point(Hx,Ly),point(Lx,Ly)]])
).



%! gis_is_shape(+Shape) is det.
%
% Checks whether Shape is a valid and supported shape.

gis_is_shape(Shape) :-
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



%! gis_dist(+Point1, +Point2, -Dist) is det.
%
% Calculates the Pythagorian Dist between Point1 and Point2.
%
% @see gis_dist_greatcircle/4 for great circle distance.

gis_dist(X, Y, Dist) :-
  gis_dist(X, Y, Dist, _).

gis_dist(X, X, 0, _).
gis_dist(X, Y, Dist, G) :-
  resource_shape(X, XShape),
  resource_shape(Y, YShape),
  gis_dist_shape(XShape, YShape, Dist, G).


gis_dist_shape(point(X1,X2), point(Y1,Y2), Dist) :-
  gis_dist_pythagorean(point(X1,X2), point(Y1,Y2), Dist), !.
gis_dist_shape(X, Y, Dist, G) :-
  rtree_distance(G, X, Y, Dist0),
  pythagorean_lat_long_to_kms(Dist0, Dist).


% For speed, first assume X and Y are shapes, not resources.  If this
% fails, proceed to interpret them as resources.
gis_dist_pythagorean(X, Y, D) :-
  gis_dist_pythagorean_fastest(X, Y, D0),
  pythagorean_lat_long_to_kms(D0, D).
gis_dist_pythagorean(X, Y, Dist) :-
  resource_shape(X, XShape),
  resource_shape(Y, YShape),
  gis_dist_pythagorean(XShape, YShape, Dist).

gis_dist_pythagorean_fastest(point(A, B), point(X, Y), D) :-
  D2 is ((X - A) ** 2) + ((Y - B) ** 2),
  D is sqrt(D2).

pythagorean_lat_long_to_kms(D1, D) :-
  D is D1 * 111.195083724. % to kms


%!  gis_dist_greatcircle(+Point1,+Point2,-Dist) is det.
%!  gis_dist_greatcircle(+Point1,+Point2,-Dist,+Unit) is det.
%
%  Calculates great circle distance between Point1 and Point2
%  in the specified Unit, which can take as a value km (kilometers)
%  or nm (nautical miles). By default, nautical miles are used.

gis_dist_greatcircle(A, B, Dist) :-
  resource_shape(A, AShape),
  resource_shape(B, BShape),
  gis_shape_dist_greatcircle(AShape, BShape, Dist).

gis_dist_greatcircle(A, B, Dist, Unit) :-
  resource_shape(A, AShape),
  resource_shape(B, BShape),
  gis_shape_dist_greatcircle(AShape, BShape, Dist, Unit).


gis_shape_dist_greatcircle(point(A1,A2), point(B1,B2), D) :-
  gis_shape_dist_greatcircle(point(A1,A2), point(B1,B2), D, nm).

gis_shape_dist_greatcircle(point(A1,A2), point(B1,B2), D, km) :-
  R is 6371, % kilometers
  gis_dist_greatcircle0(point(A1,A2), point(B1,B2), D, R).
gis_shape_dist_greatcircle(point(A1,A2), point(B1,B2), D, nm) :-
  R is 3440.06, % nautical miles
  gis_dist_greatcircle0(point(A1,A2), point(B1,B2), D, R).

% Haversine formula
gis_dist_greatcircle0(point(Lat1deg, Long1deg), point(Lat2deg, Long2deg), D, R) :-
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

gis_bearing(point(Lat1deg, Long1deg), point(Lat2deg, Long2deg), Bearing) :-
  deg2rad(Lat1deg,Lat1),
  deg2rad(Lat2deg,Lat2),
  deg2rad(Long1deg,Long1),
  deg2rad(Long2deg,Long2),
  DLong is Long2 - Long1,
  Y is sin(DLong) * cos(Lat2),
  X is cos(Lat1) * sin(Lat2) - sin(Lat1) * cos(Lat2) * cos(DLong),
  Bearing0 is atan(Y, X),
  rad2deg(Bearing0, Bearing).

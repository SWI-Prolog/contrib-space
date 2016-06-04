:- module(
  wgs84,
  [
    wgs84_candidate/2, % ?Res, ?Point
    wgs84_candidate/3, % ?Res, ?Point, ?G
    alt/2,             % ?Res, ?Alt
    alt/3,             % ?Res, ?Alt, ?G
    coords/3,          % ?Res, ?Lat, ?Long
    coords/4,          % ?Res, ?Lat, ?Long, ?Alt
    lat/2,             % ?Res, ?Lat
    lat/3,             % ?Res, ?Lat, ?G
    long/2,            % ?Res, ?Long
    long/3             % ?Res, ?Long, ?G
  ]
).

/** <module> WGS84

@author Willem Robert van Hage
@version 2009-2012

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(semweb/rdf11)).

:- rdf_register_prefix(wgs84, 'http://www.w3.org/2003/01/geo/wgs84_pos#').

:- dynamic
   gis:has_shape_hook/3.

:- multifile
   gis:has_shape_hook/3.

gis:has_shape_hook(Res, Shape, G) :-
  wgs84_candidate(Res, Shape, G).

:- rdf_meta
   wgs84_candidate(r,?),
   wgs84_candidate(r,?,?).





%! wgs84_candidate(?Res, ?Point) is nondet.
%! wgs84_candidate(?Res, ?Point, ?G) is nondet.
%
% Succeeds if Point denotes a geo-location of resource Res.
%
% Point is either of the form `point(?Lat,?Long)` or
% `point(?Lat,?Long,?Alt)`.

wgs84_candidate(Res, Point) :-
  wgs84_candidate(Res, Point, _).


wgs84_candidate(Res, Point, G) :-
  lat(Res, Lat, G),
  long(Res, Long, G),
  (alt(Res, Alt, G) -> Point = point(Lat,Long,Alt) ; Point = point(Lat,Long)).



%! alt(?Res, ?Alt) is nondet.
%! alt(?Res, ?Alt, ?G) is nondet.
%
% Succeeds if Alt is the WGS84 altitude of resource Res.

alt(Res, Alt) :-
  alt(Res, Alt, _).


alt(Res, Alt, G) :-
  rdf(Res, wgs84:alt, Alt^^xsd:float, G).



%! coords(?Res, ?Lat, ?Long) is nondet.
%! coords(?Res, ?Lat, ?Long, ?Alt) is nondet.
%
% Succeeds if 〈Lat,Long〉 or 〈Lat,Long,Alt〉 is a coordinate of
% resource Res.

coords(Res, Lat, Long) :-
  wgs84_candidate(Res, point(Lat, Long)).


coords(Res, Lat, Long, Alt) :-
  wgs84_candidate(Res, point(Lat, Long, Alt)).



%! lat(?Res, ?Lat) is nondet.
%! lat(?Res, ?Lat, ?G) is nondet.
%
% Succeeds if Lat is the WGS84 latitude of resource Res.

lat(Res, Lat) :-
  lat(Res, Lat, _).


lat(Res, Lat, G) :-
  rdf(Res, wgs84:lat, Lat^^xsd:float, G).



%! long(?Res, ?Long) is nondet.
%! long(?Res, ?Long, ?G) is nondet.
%
% Succeeds if Long is the WGS84 longitude of resource Res.

long(Res, Long) :-
  long(Res, Long, _).


long(Res, Long, G) :-
  rdf(Res, wgs84:long, Long^^xsd:float, G).

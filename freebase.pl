:- module(
  freebase,
  [
    freebase_candidate/2, % ?Res, ?Point
    freebase_candidate/3  % ?Res, ?Point, ?G
  ]
).

/** <module> Freebase

@author Willem van Hage
@version 2009-2013

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(semweb/rdf11)).

:- rdf_register_prefix(fb, 'http://rdf.freebase.com/ns/').

:- rdf_meta
   freebase_candidate(r,?),
   freebase_candidate(r,?,?).





%! freebase_candidate(?Res, ?Point) is nondet.
%! freebase_candidate(?Res, ?Point, ?G) is nondet.
%
% Succeeds if Point is a geolocation for resources Res in Freebase.

freebase_candidate(Res, Point) :-
  freebase_candidate(Res, Point, _).


freebase_candidate(Res, point(Lat,Long), G) :-
  rdf(Res, fb:'location.location.geolocation', Loc, G),
  rdf_has(Loc, fb:'location.geocode.latitude', Lat^^_),
  rdf_has(Loc, fb:'location.geocode.longitude', Long^^_).

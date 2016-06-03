:- module(
  dbpedia,
  [
    dbpedia_candidate/2,
    dbpedia_candidate/3
  ]
).

/** <module> Freebase

@author Willem van Hage
@version 2009-2013

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(semweb/rdf11)).

:- rdf_register_prefix(dbp, 'http://dbpedia.org/property/').

:- rdf_assert(
     dbp:'co_percent_e3_percent_b6rdinatenProperty',
     rdfs:subPropertyOf,
     dbp:coordinateProperty
   ).





%! dbpedia_candidate(?Res, ?Point) is nondet.
%! dbpedia_candidate(?Res, ?Point, ?G) is nondet.
%
% Succeeds if Point is a geolocation for resources Res in DBpedia.

dbpedia_candidate(Res, Point) :-
  dbpedia_candidate(Res, Point, _).


dbpedia_candidate(Res, point(Lat,Long), G) :-
  rdf_has(Res, dbp:coordinateProperty, Lex@nl, _, G),
  sub_string(Lex, Before, _, _, "_type:"), !,
  sub_string(Lex, 0, Before, _, Prefix),
  string_phrase(
    dbpedia_coord(LatDeg, LatMin, LatSec, NS, LongDeg, LongMin, LongSec, EW),
    Prefix
  ),
  Lat is NS * (LatDeg + (LatMin / 60) + (LatSec / 3600)),
  Long is EW * (LongDeg + (LongMin / 60) + (LongSec / 3600)).


dbpedia_coord(LatDeg, LatMin, LatSec, NS, LongDeg, LongMin, LongSec, EW) -->
  num(LatDeg), "_",
  num(LatMin), "_",
  num(LatSec), "_",
  ns(NS), "_",
  num(LongDeg), "_",
  num(LongMin), "_",
  num(LongSec), "_",
  ew(EW).


ns(-1.0) --> "S".
ns(1.0) --> "N".
ns(-1.0) --> "Z".


ew(-1.0) --> "W".
ew(1.0) --> "E".
ew(1.0) --> "O".


num(N) -->
  +(digit, Ds),
  {pos_sum(Ds, N)}.

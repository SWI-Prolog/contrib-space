/*  Part of SWI-Prolog

    Author:        Willem Robert van Hage
    E-mail:        W.R.van.Hage@vu.nl
    WWW:           http://www.few.vu.nl/~wrvhage
    Copyright (c)  2009-2012, Vrije Universiteit Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

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

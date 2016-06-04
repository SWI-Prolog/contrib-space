:- use_module(library(rdf/rdf_io)).
:- use_module(library(semweb/rdf11)).

:- use_module(space).

:- rdf_register_prefix(geo, 'http://www.geonames.org/ontology#').
:- rdf_register_prefix(wgs84, 'http://www.w3.org/2003/01/geo/wgs84_pos#').

:- writef("Loading demo RDF file of GeoNames features in the Rotterdam harbor.\n").
:- rdf_load_file('demo_geonames.nt').
:- writef("Selecting features with coordinates to put into the spatial index.\n").
:- gis_populate_index(demo).
:- writef("done loading demo\n\n----\n\n").

% Find features in order of proximity to the point 〈Lat,Long〉.
nearest_features(Point, Name) :-
  gis_nearest(Point, Nearest, demo),
  rdfs_instance(Nearest, geo:'Feature'),
  rdf_pref_lex(Nearest, geo:name, Name).

% Find features contained in the box defined by the two points.
contained_features(box(point(NWLat,NWLong),point(SELat,SELong)), Name) :-
  gis_contains(box(point(NWLat,NWLong),point(SELat,SELong)), Contained, demo),
  rdfs_instance(Contained, geo:'Feature'),
  rdf_pref_lex(Contained, geo:name, Name).

% Find Features in order of proximity, but restrict them to those with
% featureCode harbor.  Also fetch and show their coordinates.
nearest_harbors(Point, Name, point(HarborLat,HarborLong)) :-
  gis_nearest(Point, Nearest, demo),
  rdfs_instance(Nearest, geo:'Feature'),
  rdf_has(Nearest, geo:featureCode, geo:'H.HBR'),
  rdf_pref_lex(Nearest, geo:name, Name),
  rdf_has(Nearest, wgs84:lat, HarborLat^^xsd:float),
  rdf_has(Nearest, wgs84:long, HarborLong^^xsd:float).

:- writef("Welcome to the SWI-Prolog \"space\" package demo.\n\n").
:- writef("Try finding features in the Rotterdam harbor.\n").
:- writef("To find features near lat 51.96 long 4.13, try the following:\n").
:- writef("?- nearest_features(point(51.96,4.13), Name).\n\n").
:- writef("To find the nearest harbor to that point, try the following:\n").
:- writef("?- nearest_harbors(point(51.96,4.13), Name, point(HarborLat,HarborLong)).\n\n").
:- writef("To find features in the rectangular area between ").
:- writef("lat 51.93 long 4.10 and lat 51.96 long 4.19, try the following:\n").
:- writef("?- contained_features(box(point(51.93,4.10),point(51.96,4.19)), Name).\n\n").

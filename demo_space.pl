/*  $Id$

    Author:        Willem Robert van Hage
    E-mail:        W.R.van.Hage@vu.nl
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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

% load the Prolog module that uses the space shared object file
:- use_module(library(space/space)).

% load the semantic web package
:- use_module(library('semweb/rdf_db.pl')).

% eye candy, declare shorthand for namespaces
:- rdf_register_ns(geo, 'http://www.geonames.org/ontology#').
:- rdf_register_ns(wgs84, 'http://www.w3.org/2003/01/geo/wgs84_pos#').

:- writef("\n----\n\nLoading demo RDF file of Geonames features in the Rotterdam harbor.\n").
% load the demo RDF file containing Geonames features around the Rotterdam harbor
:- rdf_load('demo_geonames.rdf').

% this adds all URIs with associated coordinates to the space indexing queue
:- writef("Selecting features with coordinates to put in the spatial index.\n").
:- space_bulkload(uri_shape,'demo_index').
:- writef("done loading demo\n\n----\n\n").

% find Features in order of proximity to the point Lat Long
nearest_features(point(Lat,Long), Name) :-
        space_nearest(point(Lat,Long), Nearest,'demo_index'),
        rdf(Nearest, rdf:type, geo:'Feature'), % atoms starting with capitals have to be quoted.
        rdf(Nearest, geo:name, literal(Name)).

% find Features contained in the box defined by the two points        
contained_features(box(point(NWLat,NWLong),point(SELat,SELong)), Name) :-
        space_contains(box(point(NWLat,NWLong),point(SELat,SELong)), Contained, 'demo_index'),
        rdf(Contained, rdf:type, geo:'Feature'),
        rdf(Contained, geo:name, literal(Name)).

% find Features in order of proximity, but restrict them to those with featureCode harbor
% also, fetch and show their coordinates
nearest_harbors(point(Lat,Long), Name, point(HarborLat,HarborLong)) :-
        space_nearest(point(Lat,Long), Nearest,'demo_index'),
        rdf(Nearest, rdf:type, geo:'Feature'),
        rdf(Nearest, geo:featureCode, geo:'H.HBR'),
        rdf(Nearest, geo:name, literal(Name)),
        rdf(Nearest, wgs84:lat, literal(HarborLat)),
        rdf(Nearest, wgs84:long, literal(HarborLong)).



:- writef("Welcome to the SWI-Prolog \"space\" package demo.\n\n").
:- writef("Try finding features in the Rotterdam harbor.\n").
:- writef("To find features near lat 51.96 long 4.13, try the following:\n").
:- writef("?- nearest_features(point(51.96,4.13), Name).\n\n").
:- writef("To find the nearest harbor to that point, try the following:\n").
:- writef("?- nearest_harbors(point(51.96,4.13), Name, point(HarborLat,HarborLong)).\n\n").
:- writef("To find features in the rectangular area between ").
:- writef("lat 51.93 long 4.10 and lat 51.96 long 4.19, try the following:\n").
:- writef("?- contained_features(box(point(51.93,4.10),point(51.96,4.19)), Name).\n\n").
:- writef("Have fun experimenting with the \"space\" package!\n").
:- writef("If you have questions, please e-mail me.\n\n").
:- writef("Willem Robert van Hage <W.R.van.Hage@vu.nl>\n\n").


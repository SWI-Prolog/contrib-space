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

:- module(web_loader,
          [ load_url/1,
	    load_url/2,
            unload_url/1,
            unload_url/2
	  ]).

:- use_module(library(space/space)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_http_plugin)).

%%	load_url(+URL) is det.
%
%	Retrieve RDF over HTTP from a URL, load it in the rdf_db and
%	index all URI-Shape pairs that can be found in it into the
%	default index.

load_url(URL) :-
	rdf_load(URL),
	foreach(uri_shape(URI, Shape, URL),
		space_assert(URI, Shape)).

%%	load_url(+URL,+IndexName) is det.
%
%	Load using load_url/1, but index the URI-Shape pairs into
%	index named IndexName.

load_url(URL, IndexName) :-
	rdf_load(URL),
	foreach(uri_shape(URI, Shape, URL),
	        space_assert(URI, Shape, IndexName)).

%%	unload_url(+URL) is det.
%
%	Unload the RDF that was fetched from URL and remove all
%	URI-Shape pairs that are contained in it from the default index.

unload_url(URL) :-
	foreach(uri_shape(URI, Shape, URL),
		space_retract(URI, Shape)),
	rdf_unload(URL).

%%	unload_url(+URL,+IndexName) is det.
%
%	Unload the RDF that was fetched from URL and remove all
%	URI-Shape pairs that are contained in it from the index named
%	IndexName.

unload_url(URL, IndexName) :-
	foreach(uri_shape(URI, Shape, URL),
		space_retract(URI, Shape, IndexName)),
	rdf_unload(URL).





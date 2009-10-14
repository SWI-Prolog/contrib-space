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

:- module(space_web_loader,
          [ space_load_url/1,
	    space_load_url/2,
            space_unload_url/1,
            space_unload_url/2
	  ]).

:- use_module(library(space/space)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_http_plugin)).

%%	space_load_url(+URL) is det.
%
%	Retrieve RDF over HTTP from a URL, load it in the rdf_db and
%	index all URI-Shape pairs that can be found in it into the
%	default index.

space_load_url(URL) :-
	rdf_load(URL),
	Counter = counter(0),
	forall(uri_shape(URI, Shape, URL),
	       (   space_assert(URI, Shape),
		   arg(1, Counter, N0),
		   N is N0 + 1,
		   nb_setarg(1, Counter, N)
	       )),
	arg(1, Counter, C),
	space:rtree_default_index(IndexName),
	print_message(informational,space_load_url(C,IndexName)).

%%	space_load_url(+URL,+IndexName) is det.
%
%	Load using space_load_url/1, but index the URI-Shape pairs into
%	index named IndexName.

space_load_url(URL, IndexName) :-
	rdf_load(URL),
	Counter = counter(0),
	forall(uri_shape(URI, Shape, URL),
	       (   space_assert(URI, Shape, IndexName),
		   arg(1, Counter, N0),
		   N is N0 + 1,
		   nb_setarg(1, Counter, N)
	       )),
	arg(1, Counter, C),
	print_message(informational,space_load_url(C,IndexName)).

%%	space_unload_url(+URL) is det.
%
%	Unload the RDF that was fetched from URL and remove all
%	URI-Shape pairs that are contained in it from the default index.

space_unload_url(URL) :-
	Counter = counter(0),
	forall(uri_shape(URI, Shape, URL),
	       (   space_retract(URI, Shape),
		   arg(1, Counter, N0),
		   N is N0 + 1,
		   nb_setarg(1, Counter, N)
	       )),
	arg(1, Counter, C),
	space:rtree_default_index(IndexName),
	print_message(informational,space_unload_url(C,IndexName)),
	rdf_unload(URL).

%%	space_unload_url(+URL,+IndexName) is det.
%
%	Unload the RDF that was fetched from URL and remove all
%	URI-Shape pairs that are contained in it from the index named
%	IndexName.

space_unload_url(URL, IndexName) :-
	Counter = counter(0),
	forall(uri_shape(URI, Shape, URL),
	       (   space_retract(URI, Shape, IndexName),
		   arg(1, Counter, N0),
		   N is N0 + 1,
		   nb_setarg(1, Counter, N)
	       )),
	arg(1, Counter, C),
	print_message(informational,space_unload_url(C,IndexName)),
	rdf_unload(URL).



:- multifile prolog:message//1.

prolog:message(space_load_url(C,IndexName)) -->
	[ 'Added ~w URI-Shape ~w to ~w'-[C, P, IndexName] ],
	{ plural(C,P) }.

prolog:message(space_unload_url(C,IndexName)) -->
	[ 'Removed ~w URI-Shape ~w from ~w'-[C, P, IndexName] ],
	{ plural(C,P) }.

plural(1,pair) :- !.
plural(_,pairs).







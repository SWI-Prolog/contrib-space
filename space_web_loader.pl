:- module(
  gis_io,
  [
    gis_load/1,    % +Source
    gis_load/2,    % +Source, +Opts
    gis_unload/1,  % +Source
    gis_unload/2,  % +Source, +Opts
    gis_crawl/1,   % +Source
    gis_crawl/2,   % +Source, +Opts
    gis_uncrawl/1, % +Source
    gis_uncrawl/2  % +Source, +Opts
  ]
).

/** <module> GIS I/O

@author Willem van Hage
@version 2009-2012

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(dict_ext)).
:- use_module(library(option)).
:- use_module(library(rdf/rdf_io)).
:- use_module(library(semweb/rdf11)).

:- use_module(space).





%! gis_load(+Source) is det.
%! gis_load(+Source, +Opts) is det.
%
% Retrieve RDF from a Source, load it in the RDF DB and index all
% 〈resource,shape〉-pairs that can be found into the default index.
% 
% The following options are supported:
%
%   * index(+atom) The name of the index where the
%   〈resource,shape〉-pairs are stored.  Default is `default`.
%
%    * graph(+atom) The name of the graph in which the RDF
%    presentation of the 〈resource,shape〉-pairs is stored.  These
%    pairs can be retrieved with has_shape/[2,3].  Default is
%    `default`.

gis_load(Source) :-
  gis_load(Source, []).


gis_load(Source, Opts) :-
  rdf_load_file(Source, Opts),
  gis_default_index(IndexDef),
  option(index(Index), Opts, IndexDef),
  State = _{count:0},
  option(graph(G), Opts, _VAR),
  forall(resource_shape(Res, Shape, G, Index), (
    gis_assert(Res, Shape, Index),
    dict_inc(count, State)
  )),
  print_message(informational, gis_load(State.count, Index)).



%! gis_unload(+Source) is det.
%! gis_unload(+Source, +Opts) is det.
%
% Unload the RDF that was fetched from Source and remove all
% 〈resource,shape〉-pairs that are contained in it from the default
% index.
%
% The following options are supported:
%
%   * index(+atom) Remove from the given Index.
%
%   * graph(+Graph) Remove from the given Graph.

gis_unload(Source) :-
  gis_unload(Source, []).


gis_unload(Source, Opts) :-
  gis_default_index(IndexDef),
  option(index(Index), Opts, IndexDef),
  option(graph(G), Opts),
  State = _{count:0},
  forall(resource_shape(Res, Shape, G, Index), (
    gis_retract(Res, Shape, Index),
    dict_inc(count, State)
  )),
  print_message(informational, gis_unload(State.count, Index)),
  rdf_unload(G).

:- multifile
    prolog:message//1.

prolog:message(gis_load(0,_)) -->  [], !.
prolog:message(gis_load(C,IndexName)) -->
  [ 'Added ~w URI-Shape ~w to ~w'-[C, P, IndexName] ],
  { plural(C,P) }.

prolog:message(gis_unload(0,_)) --> [], !.
prolog:message(gis_unload(C,IndexName)) -->
  [ 'Removed ~w URI-Shape ~w from ~w'-[C, P, IndexName] ],
  { plural(C,P) }.

plural(1,pair) :- !.
plural(_,pairs).

prolog:message(gis_crawl(C)) -->
  [ 'Crawling ~w'-[C] ].

prolog:message(gis_uncrawl(C)) -->
  [ 'Uncrawling ~w'-[C] ].


%! link_property(+Property) is det.
%
%  RDF properties declared a link_property will be traversed by
%  gis_crawl. link_property is a dynamic property.
%  By default owl:sameAs, skos:exactMatch, and skos:closeMatch are
%  link properties.

:- dynamic link_property/1.
link_property('http://www.w3.org/2002/07/owl#sameAs').
link_property('http://www.w3.org/2004/02/skos/core#exactMatch').
link_property('http://www.w3.org/2004/02/skos/core#closeMatch').

%! gis_crawl(+Source) is det.
%
%  Retrieve RDF over HTTP from a Source, load it in the rdf_db and
%  index all URI-Shape pairs that can be found in it into the
%  default index.
%  Also attempt to resolve all URIs that appear as object in a
%  link_property statement downloaded from the Source. Retrieve
%  these URIs and process them in the same way. Iterate this
%  process until there are no new links that have not already
%  been crawled.

gis_crawl(Source) :-  gis_crawl(Source,[]).

%! gis_crawl(+Source,+Opts) is det.
%
%  Crawl using gis_crawl/1, with additional options.
%
%               * index(+IndexName)
%    Index the URI-Shape pairs into index named IndexName.
%
%    * graph(+Graph)
%    Store the URI-Shape pairs in the named graph Graph.
%    The pairs are recorded as `has_shape(URI,Shape,Graph)`.

gis_crawl(Source,Opts) :-
  with_mutex(message,print_message(informational,gis_crawl(Source))),
  gis_load(Source,Opts),
  findall( NewLink, new_link(Source:_,NewLink,_Type), NewLinks ),
  forall( member(NL, NewLinks),
          thread_create(gis_crawl(NL,Opts),_,[])
        ).

%! gis_uncrawl(+Source) is det.
%
%  Unload the RDF that was fetched from Source and remove all
%  URI-Shape pairs that are contained in it from the default index.
%  Also unload all data that were crawled by iteratively resolving
%  the URIs linked to with a link_property.

gis_uncrawl(Source) :- gis_uncrawl(Source,[]).

%! gis_uncrawl(+Source,+IndexName) is det.
%
%  Unload using gis_uncrawl/1, but remove the URI-Shape pairs
%  from the index named IndexName.
%
%               * index(+IndexName)
%    Remove the URI-Shape pairs from index named IndexName.
%
%    * graph(+Graph)
%    Remove the URI-Shape pairs from the named graph Graph.

gis_uncrawl(Source,Opts) :-
  with_mutex(message,print_message(informational,gis_uncrawl(Source))),
  findall( Link, old_link(Source:_,Link,_Type), Links ),
  gis_unload(Source,Opts),
  forall(member(L, Links), gis_uncrawl(L,Opts)).

new_link(FromSource,NewLink,P) :-
  link_property(P),
  rdf(_,P,NewLink,FromSource),
  \+once(rdf(_,_,_,NewLink:_)).

old_link(FromSource,Link,P) :-
  link_property(P),
  rdf(_,P,Link,FromSource),
  once(rdf(_,_,_,Link:_)).

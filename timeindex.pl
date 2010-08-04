/*  $Id$

    Author:        Willem Robert van Hage
    E-mail:        wrvhage@few.vu.nl
    WWW:           http://www.few.vu.nl/~wrvhage
    Copyright (C): 2010, Vrije Universiteit Amsterdam

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

% TODO: Make version of atom_map that allows double type datums and
%	that has nondet search functions.
%	This would remove the need for the explicit EpochOffset and
%	would speed up all search predicates.

:- module(timeindex,
	  [  time_assert/4,       % +URI, +Begin, +End, +Index
	     time_assert/3,	  % +URI, +Begin, +End  (uses default index)
	     time_retract/4,      % +URI, +Begin, +End, +Index
	     time_retract/3,      % +URI, +Begin, +End  (uses default index)
	     time_clear/2,	  % +Index, +NewEpochOffset
	     time_clear/1,	  % +Index
	     time_clear/0,        % (uses default index)
	     time_index_all/1,    % +Index
	     time_index_all/0,    % (uses default index)
	     time_intersects/4,	  % +Begin, +End, -URI, +Index
	     time_intersects/3,	  % (uses default index)
	     time_contains/4,	  % +Begin, +End, -URI, +Index
	     time_contains/3,	  % (uses default index)
	     time_previous_end/3, % +Time, -URI, +Index
	     time_previous_end/2, % (uses default index)
	     time_next_begin/3,   % +Time, -URI, +Index
	     time_next_begin/2,   % (uses default index)
	     uri_time/5,          % ?URI, ?Begin, ?End, ?Source, +EpochOffset
	     uri_time/3           % ?URI, ?Begin, ?End (uses offset 0)
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).

:- dynamic time_indices/4.

%%	time_new(+IndexName) is det.
%%	time_new(+IndexName,+Offset) is det.
%
%	Creates a new temporal index.
%       Offset is the epoch used internally by the index.
%
time_new(Index) :- time_new(Index, 0).
time_new(Index, EpochOffset) :-
	rdf_new_literal_map(B),
	rdf_new_literal_map(E),
	assert(time_indices(Index, B, E, EpochOffset)).

:- time_new(default).

%%	time_assert(+URI,+Begin,+End) is det.
%%	time_assert(+URI,+Begin,+End,+IndexName) is det.
%
%	Inserts a new URI-TimeInterval pair into the index.
%
time_assert(URI, TB, TE) :- time_assert(URI, TB, TE, default).
time_assert(URI, TB, TE, Index) :-
	time_indices(Index, IdxB, IdxE, EpochOffset),
	TBE is TB - EpochOffset,
	TEE is TE - EpochOffset,
	rdf_insert_literal_map(IdxB, TBE, URI),
	rdf_insert_literal_map(IdxE, TEE, URI).

%%	time_retract(+URI,+Begin,+End) is det.
%%	time_retract(+URI,+Begin,+End,+IndexName) is det.
%
%	Removes a URI-TimeInterval pair from the index.
%
time_retract(URI, TB, TE) :- time_retract(URI, TB, TE, default).
time_retract(URI, TB, TE, Index) :-
	time_indices(Index, IdxB, IdxE, EpochOffset),
	TBE is TB - EpochOffset,
	TEE is TE - EpochOffset,
	rdf_delete_literal_map(IdxB, TBE, URI),
	rdf_delete_literal_map(IdxE, TEE, URI).

%%	time_clear(+IndexName,+NewOffset) is det.
%%	time_clear(+IndexName) is det.
%%	time_clear is det.
%
%	Clears an index. Optionally sets a new epoch for the index
%	that will be used for all future asserts into the index.
%
time_clear :- time_clear(default).
time_clear(Index) :-
	time_indices(Index, IdxB, IdxE, OldEpochOffset),
	retractall(time_indices(Index, _, _, _)),
	rdf_destroy_literal_map(IdxB),
	rdf_destroy_literal_map(IdxE),
	time_new(Index, OldEpochOffset).
time_clear(Index, NewEpochOffset) :-
	time_indices(Index, IdxB, IdxE, _OldEpochOffset),
	retractall(time_indices(Index, _, _, _)),
	rdf_destroy_literal_map(IdxB),
	rdf_destroy_literal_map(IdxE),
	time_new(Index, NewEpochOffset).

%%	time_index_all(+IndexName) is det.
%%	time_index_all is det.
%
%	Adds all URI-TimeInterval pairs found by the uri_time predicate
%	into the index.
%
time_index_all :- time_index_all(default).
time_index_all(Index) :-
	forall(uri_time(URI, Begin, End),
	       time_assert(URI, Begin, End, Index)).

%%	time_intersects(+Begin,+End,-URI,+Index) is nondet.
%%	time_intersects(+Begin,+End,-URI) is nondet.
%
%	Finds all URIs that have an indexed time interval
%	that intersects with the interval [Begin,End].
%
time_intersects(TB, TE, URI) :- time_intersects(TB, TE, URI, default).
time_intersects(TB, TE, URI, Index) :-
	time_indices(Index, IdxB, IdxE, EO),
	timestamp_epoch(TB, TBE, EO),
	timestamp_epoch(TE, TEE, EO),
	rdf_keys_in_literal_map(IdxB, between(TBE, TEE), BeginMatch),
	rdf_litindex:list_to_or(BeginMatch, between(TBE, TEE), BeginOr),
	rdf_litindex:lookup(BeginOr, IdxB, B2, B3),
	match_results(B2, B3, B4),
	rdf_keys_in_literal_map(IdxE, between(TBE, TEE), EndMatch),
	rdf_litindex:list_to_or(EndMatch, between(TBE, TEE), EndOr),
	rdf_litindex:lookup(EndOr, IdxE, E2, E3),
	match_results(E2, E3, E4),
	append(B4, E4, BE),
	predsort(ord, BE, Matches), !,
	pairs_values(Matches, Values),
	list_to_set(Values, ValueSet),
	member(URI, ValueSet).

%%	time_contains(+Begin,+End,-URI,+Index) is nondet.
%%	time_contains(+Begin,+End,-URI) is nondet.
%
%	Finds all URIs that have an indexed time interval
%	that are contained by the interval [Begin,End].
%
time_contains(TB, TE, URI) :- time_contains(TB, TE, URI, default).
time_contains(TB, TE, URI, Index) :-
	time_indices(Index, IdxB, IdxE, EO),
	timestamp_epoch(TB, TBE, EO),
	timestamp_epoch(TE, TEE, EO),
	rdf_keys_in_literal_map(IdxB, between(TBE, TEE), BeginMatch),
	rdf_litindex:list_to_or(BeginMatch, between(TBE, TEE), BeginOr),
	rdf_litindex:lookup(BeginOr, IdxB, B2, B3),
	match_results(B2, B3, B4),
	rdf_keys_in_literal_map(IdxE, between(TBE, TEE), EndMatch),
	rdf_litindex:list_to_or(EndMatch, between(TBE, TEE), EndOr),
	rdf_litindex:lookup(EndOr, IdxE, E2, E3),
	match_results(E2, E3, E4),
	predsort(ord, B4, BS),
	predsort(ord, E4, ES),
	pairs_values(BS, BSValues),
	pairs_values(ES, ESValues),
	ord_intersection(BSValues, ESValues, Matches), !,
	member(URI, Matches).

%%	time_previous_end(+Time,-URI,+Index) is nondet.
%%	time_previous_end(+Time,-URI) is nondet.
%
%	Finds all URIs that have an indexed time interval
%	that ends before time point Time in order of increasing
%	duration.
%
time_previous_end(T, URI) :- time_previous_end(T, URI, default).
time_previous_end(T, URI, Index) :-
	time_indices(Index, _, IdxE, EO),
	timestamp_epoch(T, TE, EO),
	rdf_keys_in_literal_map(IdxE, le(TE), EndMatch),
	rdf_litindex:list_to_or(EndMatch, le(TE), EndOr),
	rdf_litindex:lookup(EndOr, IdxE, E2, E3),
	match_results(E2, E3, E4),
	predsort(rev, E4, ES), !,
	member(le(_,T1)-URI, ES),
	T1 =< TE.

%%	time_previous_end(+Time,-URI,+Index) is nondet.
%%	time_previous_end(+Time,-URI) is nondet.
%
%	Finds all URIs that have an indexed time interval
%	that begins after time point Time in order of increasing
%	duration.
%
time_next_begin(T, URI) :- time_next_begin(T, URI, default).
time_next_begin(T, URI, Index) :-
	time_indices(Index, IdxB, _, EO),
	timestamp_epoch(T, TE, EO),
	rdf_keys_in_literal_map(IdxB, ge(TE), BeginMatch),
	rdf_litindex:list_to_or(BeginMatch, ge(TE), BeginOr),
	rdf_litindex:lookup(BeginOr, IdxB, B2, B3),
	match_result(B2, B3, _-URI).

zip_tree_member([H0|T0], [H1|T1], R) :-
	is_list(H0),
	(   zip_tree_member(H0, H1, R)
	;   zip_tree_member(T0, T1, R)
	).
zip_tree_member([[H0]], [H1|T1], R) :-
	(   member(H, H1),
	    R = H0-H
	;   zip_tree_member([H0], T1, R)
	).

zip_tree(_, [], []).
zip_tree(A, B, A-B) :- \+is_list(A).
zip_tree([H0], [H1|T1], [H|T]) :-
	\+is_list(H0),
	zip_tree(H0, H1, H),
	zip_tree([H0], T1, T).
zip_tree([H0|T0], [H1|T1], [H|T]) :-
	zip_tree(H0, H1, H),
	zip_tree(T0, T1, T).

match_results(A, B, C) :-
	zip_tree(A, B, Z),
	flatten(Z, C).
match_result(A, B, C) :-
	zip_tree_member(A, B, C).

ord(>, between(_,_,A)-_, between(_,_,B)-_) :- A > B.
ord(<, between(_,_,A)-_, between(_,_,B)-_) :- A < B.
ord(>, le(_,A)-_, le(_,B)-_) :- A > B.
ord(<, le(_,A)-_, le(_,B)-_) :- A < B.
ord(=, _, _).
rev(>, between(_,_,A)-_, between(_,_,B)-_) :- A < B.
rev(<, between(_,_,A)-_, between(_,_,B)-_) :- A > B.
rev(>, le(_,A)-_, le(_,B)-_) :- A < B.
rev(>, le(_,A)-_, le(_,B)-_) :- A > B.
rev(=, _, _).

%%	uri_time(?URI,?Begin,?End,?Source,+Offset) is semidet.
%%	uri_time(?URI,?Begin,?End,?Source) is semidet.
%%	uri_time(?URI,?Begin,?End) is semidet.
%
%	Finds all URI-TimeInterval pairs described in the RDF database.
%	Source matches the graphs in which the pair is described.
%	Optionally, the Begin and End of the time interval are
%	returned with respect to a given Offset.
%
uri_time(URI, Begin, End) :- uri_time(URI, Begin, End, _Source, 0).
uri_time(URI, Begin, End, Source) :- uri_time(URI, Begin, End, Source, 0).
uri_time(URI, Begin, End, Source, EpochOffset) :-
	timestamp(URI, begin, TimeStamp, Source1),
	timestamp(URI, end, TimeStamp, Source2),
	(   Source1 = Source2
	->  Source = Source1
	;   Source = Source1
	;   Source = Source2
	),
	iso_timestamp(TimeStamp, Begin0, End0),
	Begin is Begin0 - EpochOffset,
	End is End0 - EpochOffset.

:- rdf_register_ns(sem, 'http://semanticweb.cs.vu.nl/2009/11/sem/').

timestamp(URI, TimeStamp) :- timestamp(URI, _, TimeStamp, _Source).
timestamp(URI, begin, TimeStamp, Source) :-
	rdf_has(URI, sem:hasBeginTimeStamp, literal(T), Pred),
	rdf(URI, Pred, literal(T), Source),
	(   T = type(_,TimeStamp)
	->  true
	;   TimeStamp = T
	).
timestamp(URI, end, TimeStamp, Source) :-
	rdf_has(URI, sem:hasEndTimeStamp, literal(T), Pred),
	rdf(URI, Pred, literal(T), Source),
	(   T = type(_,TimeStamp)
	->  true
	;   TimeStamp = T
	).
timestamp(URI, _, TimeStamp, Source) :-
	rdf_has(URI, sem:hasTimeStamp, literal(T), Pred),
	rdf(URI, Pred, literal(T), Source),
	(   T = type(_,TimeStamp)
	->  true
	;   TimeStamp = T
	).

iso_timestamp(TimeStamp, T) :-
	parse_time(TimeStamp, T).
iso_timestamp(TimeStamp, Begin, Begin) :-
	parse_time(TimeStamp, Begin).

timestamp_epoch(TimeStamp, Epoch) :- timestamp_epoch(TimeStamp, Epoch, 0).
timestamp_epoch(TimeStamp, Epoch, EpochOffset) :-
	(   number(TimeStamp)
	->  E = TimeStamp
	;   iso_timestamp(TimeStamp, E)
	),
	Epoch is E - EpochOffset.

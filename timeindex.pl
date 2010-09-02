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

/*
    Internally, all time objects are stored as intervals.
    point(T) is translated to interval(T,T).
    If fuzzy time intervals,
    interval(EarlyBegin,LateBegin,EarlyEnd,LateEnd),
    should ever be implemented, this could be done by postprocessing
    over operations on an index containing interval(EarlyBegin,LateEnd).

    Per handle there are two indices, one containing the begin points
    and the other containing the end points of the intervals.
    The index containing the end points actually contains negative time
    points to inverse the order of the index.
*/

% TODO: Make version of atom_map that allows double type datums and
%	that has nondet search functions.
%	This would remove the need for the explicit EpochOffset and
%	would speed up all search predicates.

:- module(timeindex,
	  [  time_index/1,        % ?Index
	     time_index/4,        % +Index, ?BeginIndex, ?EndIndex, ?Epoch
	     time_setting/2,	  % +Index ?Setting
	     time_setting/1,	  % ?Setting  (uses default index)
	     time_assert/3,       % +URI, +Time, +Index
	     time_assert/2,	  % +URI, +Time  (uses default index)
	     time_retract/3,      % +URI, +Time, +Index
	     time_retract/2,      % +URI, +Time  (uses default index)
	     time_clear/2,	  % +Index, +NewEpochOffset
	     time_clear/1,	  % +Index
	     time_clear/0,        % (uses default index)
	     time_index_all/1,    % +Index
	     time_index_all/0,    % (uses default index)
	     time_bulkload/2,     % :CandidatePred, +Index
	     time_bulkload/1,     % :CandidatePred
	     time_intersects/3,	  % +Time, -URI, +Index
	     time_intersects/2,	  % (uses default index)
	     time_contains/3,	  % +Time, -URI, +Index
	     time_contains/2,	  % (uses default index)
	     time_prev_end/3,	  % +Time, -URI, +Index
	     time_prev_end/2,     % (uses default index)
	     time_next_begin/3,   % +Time, -URI, +Index
	     time_next_begin/2,   % (uses default index)
	     uri_time/4,          % ?URI, ?Time, ?Source, +EpochOffset
	     uri_time/3,          % ?URI, ?Time, ?Source (uses offset 0)
	     uri_time/2,          % ?URI, ?Time (uses offset 0)
	     parse_timestamp/3,   % +TimeStamp, -Epoch, +EpochOffset
	     parse_timestamp/2    % ?TimeStamp, ?Epoch (uses offset 0)
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).

:- dynamic time_indices/4.

:- rdf_meta  time_index(r),
	     time_index(r,?,?,?),
	     time_setting(r,?),
	     time_assert(r,?,r),
	     time_assert(r,?),
	     time_retract(r,?,r),
	     time_retract(r,?),
	     time_clear(r,?),
	     time_clear(r),
	     time_index_all(r),
	     time_bulkload(?,r),
	     time_intersects(?,r,r),
	     time_intersects(?,r),
	     time_contains(?,r,r),
	     time_contains(?,r),
	     time_prev_end(?,r,r),
	     time_prev_end(?,r),
	     time_next_begin(?,r,r),
	     time_next_begin(?,r),
	     uri_time(r,?,t,?),
	     uri_time(r,?,t),
	     uri_time(r,?).


time_index(Index) :- time_indices(Index,_,_,_).

time_index(Index, IdxB, IdxE, Epoch) :-
	time_indices(Index, IdxB, IdxE, Epoch), !.
time_index(Index, IdxB, IdxE, Epoch) :-
	nonvar(Epoch),
	time_new(Index, Epoch),
	time_indices(Index, IdxB, IdxE, Epoch), !.
time_index(Index, IdxB, IdxE, Epoch) :-
	var(Epoch),
%	time_new(Index),
	time_indices(Index, IdxB, IdxE, Epoch), !.

%%	time_setting(?Option) is det.
%
%	Sets/retrieves settings and current values of an index.
%	Supported Options are:
%	size(-N), N is the number of URI-Time pairs in the index.
%	epoch(+Epoch), sets a new Epoch for the index, clears the index.
%       epoch(-Epoch), Epoch is the current Epoch of the index.
%
time_setting(Option) :- time_setting(default, Option).
time_setting(Index, size(N)) :-
	time_indices(Index,B,_,_), !,
	rdf_statistics_literal_map(B,size(N,_)).
time_setting(Index, epoch(E)) :-
	var(E),
	time_indices(Index,_,_,E), !.
time_setting(Index, epoch(E)) :-
	nonvar(E),
	format('% Clearing index ~w, setting new Epoch to ~w\n', [Index,E]),
	time_clear(Index, E).


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


%%	time_assert(+URI,+Time) is det.
%%	time_assert(+URI,+Time,+IndexName) is det.
%
%	Inserts a new URI-Time pair into the index.
%
time_assert(URI, T) :- time_assert(URI, T, default).
time_assert(URI, interval(TB, TE), Index) :-
	(   time_index(Index)
	->  true
	;   time_clear(Index, TB)
	),
	time_index(Index, IdxB, IdxE, EpochOffset),
	TBE is integer(TB - EpochOffset),
	TEE is integer(-1 * (TE - EpochOffset)),
	rdf_insert_literal_map(IdxB, TBE, URI),
	rdf_insert_literal_map(IdxE, TEE, URI), !.
time_assert(URI, point(T), Index) :-
	time_assert(URI, interval(T,T), Index), !.
time_assert(URI, TimeExpr, Index) :-
	atom(TimeExpr),
	parse_timestamp(TimeExpr, T),
	time_assert(URI, point(T), Index), !.
time_assert(URI, TimeExpr, Index) :-
	number(TimeExpr),
	time_assert(URI, point(TimeExpr), Index), !.

%%	time_retract(+URI,+Time) is det.
%%	time_retract(+URI,+Time,+IndexName) is det.
%
%	Removes a URI-Time pair from the index.
%
time_retract(URI, T) :- time_retract(URI, T, default).
time_retract(URI, interval(TB, TE), Index) :-
	time_indices(Index, IdxB, IdxE, EpochOffset),
	TBE is integer(TB - EpochOffset),
	TEE is integer(-1 * (TE - EpochOffset)),
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
	time_index(Index, IdxB, IdxE, OldEpochOffset),
	retractall(time_indices(Index, _, _, _)),
	rdf_destroy_literal_map(IdxB),
	rdf_destroy_literal_map(IdxE),
	time_new(Index, OldEpochOffset).
time_clear(Index, NewEpochOffset) :-
	number(NewEpochOffset),
	(   time_index(Index, IdxB, IdxE, _OldEpochOffset)
	->  retractall(time_indices(Index, _, _, _)),
	    rdf_destroy_literal_map(IdxB),
	    rdf_destroy_literal_map(IdxE)
	;   true
	),
	time_new(Index, NewEpochOffset), !.

%%	time_index_all(+IndexName) is det.
%%	time_index_all is det.
%
%	Adds all URI-Time pairs found by the uri_time predicate
%	into the index.
%
time_index_all :- time_index_all(default).
time_index_all(Index) :- time_bulkload(uri_time, Index).

:- meta_predicate time_bulkload(2), time_bulkload(2,+).

%%	time_bulkload(:CandidatePred,+Index) is det.
%%	time_bulkload(:CandidatePred) is det.
%
%	Like time_index_all, but indexes URI-Time pairs found by the
%	custom predicate CandidatePred.
%
time_bulkload(CandidatePred) :- time_bulkload(CandidatePred, default).
time_bulkload(CandidatePred, Index) :-
	time_clear(Index),
	forall(call(CandidatePred, URI, Time),
	       time_assert(URI, Time, Index)),
	time_index(Index,B,_,_),
	rdf_statistics_literal_map(B,size(K,_)),
	format('% Added ~w URI-Time pairs to ~w\n',[K,Index]).


%%	time_intersects(+Time,-URI,+Index) is nondet.
%%	time_intersects(+Time,-URI) is nondet.
%
%	Finds all URIs that have an indexed time interval
%	that intersects with the interval Time = interval(Begin,End).
%
%       NB! The implementation currently does not return intervals
%       that contain the query interval, hence the name time_intersects
%       is currently a misnomer.
%
time_intersects(T, URI) :- time_intersects(T, URI, default).
% FIXME: buggy implementation, does not find intervals that start
% before the query interval and end after the query interval.
% The obvious solution would be to compute the intersection of the
% set of intervals ending after the begin of the query and the
% set of intervals starting before the end of the query, but that
% is a very expensive query.
% As soon as there is a nondet version of rdf_keys_in_literal_map
% this implementation could become viable if the hard solutions are
% delayed until after all easy solutions have been found.
time_intersects(interval(TB, TE), URI, Index) :-
	time_index(Index, IdxB, IdxE, EO),
	parse_timestamp(TB, TBE, EO),
	parse_timestamp(TE, TEE, EO),
	TBI is integer(TBE),
	TEI is integer(TEE),
	rdf_keys_in_literal_map(IdxB, between(TBI, TEI), BeginMatch),
	rdf_litindex:list_to_or(BeginMatch, between(TBI, TEI), BeginOr),
	rdf_litindex:lookup(BeginOr, IdxB, B2, B3),
	match_results(B2, B3, B4),
	TBR is integer(-1 * TBE),
	TER is integer(-1 * TEE),
	rdf_keys_in_literal_map(IdxE, between(TER, TBR), EndMatch),
	rdf_litindex:list_to_or(EndMatch, between(TER, TBR), EndOr),
	rdf_litindex:lookup(EndOr, IdxE, E2, E3),
	match_results(E2, E3, E4),
	append(E4, B4, Matches),
	% predsort(ord, E4B4, Matches), !,
	pairs_values(Matches, Values), !,
	list_to_set(Values, ValueSet),
	member(URI, ValueSet).

%%	time_contains(+Time,-URI,+Index) is nondet.
%%	time_contains(+Time,-URI) is nondet.
%
%	Finds all URIs that have an indexed time interval
%	that are contained by the interval Time = interval(Begin,End).
%
time_contains(T, URI) :- time_contains(T, URI, default).
time_contains(interval(-,-), URI, Index) :- !,
	time_contains_all(interval(-,-), URI, Index).
time_contains(interval(-,End), URI, Index) :- !,
	time_contains_le(interval(-,End), URI, Index).
time_contains(interval(Begin,-), URI, Index) :- !,
	time_contains_ge(interval(Begin,-), URI, Index).
time_contains(interval(TB, TE), URI, Index) :-
	time_index(Index, IdxB, IdxE, EO),
	parse_timestamp(TB, TBE, EO),
	TBI is integer(TBE),
	TBR is integer(-1 * TBE),
	parse_timestamp(TE, TEE, EO),
	TEI is integer(TEE),
	TER is integer(-1 * TEE),
	rdf_keys_in_literal_map(IdxB, between(TBI, TEI), BeginMatch),
	rdf_litindex:list_to_or(BeginMatch, between(TBI, TEI), BeginOr),
	rdf_litindex:lookup(BeginOr, IdxB, B2, B3),
	match_results(B2, B3, B4),
	rdf_keys_in_literal_map(IdxE, between(TER, TBR), EndMatch),
	rdf_litindex:list_to_or(EndMatch, between(TER, TBR), EndOr),
	rdf_litindex:lookup(EndOr, IdxE, E2, E3),
	match_results(E2, E3, E4),
	predsort(ord, B4, BS),
	predsort(rev, E4, ES),
	pairs_values(BS, BSValues),
	pairs_values(ES, ESValues),
	ord_intersection(BSValues, ESValues, Matches), !,
	member(URI, Matches).

lookup(Index,Key,A,B) :-
       rdf_litindex:lookup(Key, Index, [A], [B]).

time_contains_all(interval(-,-), URI, Index) :-
	time_index(Index, IdxB, IdxE, _EO),
	rdf_keys_in_literal_map(IdxB, all, BeginMatch),
	rdf_keys_in_literal_map(IdxE, all, EndMatch),
	maplist(lookup(IdxB),BeginMatch,BM1,BM2),
	maplist(lookup(IdxB),EndMatch,EM1,EM2),
	match_results(BM1, BM2, BMatches),
	match_results(EM1, EM2, EMatches),
	pairs_values(BMatches, BSValues),
	pairs_values(EMatches, ESValues),
	append(BSValues, ESValues, Matches2),
	list_to_set(Matches2,Matches), !,
	member(URI, Matches).
time_contains_le(interval(-, TE), URI, Index) :-
	time_prev_end(point(TE), URI, Index).
time_contains_ge(interval(TB, -), URI, Index) :-
	time_next_begin(point(TB), URI, Index).

%%	time_prev_end(+Time,-URI,+Index) is nondet.
%%	time_prev_end(+Time,-URI) is nondet.
%
%	Finds all URIs that have an indexed time interval
%	that ends before time point or interval Time in
%	order of increasing duration.
%
time_prev_end(point(T), URI) :- time_prev_end(interval(T,T), URI, default).
time_prev_end(interval(T,T1), URI) :- time_prev_end(interval(T,T1), URI, default).
time_prev_end(point(T), URI, Index) :- time_prev_end(interval(T,T), URI, Index).
time_prev_end(interval(T,_), URI, Index) :-
	time_index(Index, _, IdxE, EO),
	parse_timestamp(T, TE, EO),
	TER is integer(-1 * TE),
	rdf_keys_in_literal_map(IdxE, ge(TER), EndMatch),
	rdf_litindex:list_to_or(EndMatch, ge(TER), EndOr),
	rdf_litindex:lookup(EndOr, IdxE, E2, E3),
	match_result(E2, E3, _-URI).

%%	time_next_begin(+Time,-URI,+Index) is nondet.
%%	time_next_begin(+Time,-URI) is nondet.
%
%	Finds all URIs that have an indexed time interval
%	that begins after time point or interval Time in order of
%	increasing duration.
%
time_next_begin(point(T), URI) :- time_next_begin(interval(T,T), URI, default).
time_next_begin(interval(T0,T), URI) :- time_next_begin(interval(T0,T), URI, default).
time_next_begin(point(T), URI, Index) :- time_next_begin(interval(T,T), URI, Index).
time_next_begin(interval(_,T), URI, Index) :-
	time_index(Index, IdxB, _, EO),
	parse_timestamp(T, TE, EO),
	TEI is integer(TE),
	rdf_keys_in_literal_map(IdxB, ge(TEI), BeginMatch),
	rdf_litindex:list_to_or(BeginMatch, ge(TEI), BeginOr),
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
zip_tree([], [B], nil-B).
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

%%	uri_time(?URI,?Time,?Source,+Offset) is semidet.
%%	uri_time(?URI,?Time,?Source) is semidet.
%%	uri_time(?URI,?Time) is semidet.
%
%	Finds all URI-Time pairs described in the RDF database.
%	Source matches the graphs in which the pair is described.
%	Optionally, the Begin and End of the time interval are
%	returned with respect to a given Offset.
%
uri_time(URI, interval(Begin, End)) :- uri_time(URI, interval(Begin, End), _Source, 0).
uri_time(URI, interval(Begin, End), Source) :- uri_time(URI, interval(Begin, End), Source, 0).
uri_time(URI, interval(Begin, End), Source, EpochOffset) :-
	time_candidate(URI, interval(TB,TE), Source),
	parse_timestamp(TB, Begin0),
	parse_timestamp(TE, End0),
	Begin is Begin0 - EpochOffset,
	End is End0 - EpochOffset.

:- rdf_register_ns(sem, 'http://semanticweb.cs.vu.nl/2009/11/sem/').
:- rdf_register_ns(owltime, 'http://www.w3.org/2006/time#').

time_candidate(URI, TimeStamp) :- time_candidate(URI, TimeStamp, _Source).
time_candidate(URI, TimeStamp, Source) :-
	owl_time_xsd_candidate(URI, TimeStamp, Source).
time_candidate(URI, TimeStamp, Source) :-
	sem_time_candidate(URI, TimeStamp, Source).

sem_time_candidate(URI, TimeStamp) :- sem_time_candidate(URI, TimeStamp, _Source).
sem_time_candidate(URI, interval(Begin,End), Source) :-
	(   rdf(URI, sem:hasBeginTimeStamp, literal(TB), Source1)
	->  (   Source1 = Source:_ % work around
	    ->  true
	    ;   Source1 = Source
	    ),
	    (   TB = type(_,Begin)
	    ->  true
	    ;   Begin = TB
	    )
	;   Begin = -
	),
        (   rdf(URI, sem:hasEndTimeStamp, literal(TE), Source2)
	->  (   Source2 = Source:_ % work around
	    ->  true
	    ;   Source2 = Source
	    ),
	    (   TE = type(_,End)
	    ->  true
	    ;   End = TE
	    )
	;   End = -
	),
	(   Begin = -, End = -
	->  fail
	;   true
	).

sem_time_candidate(URI, interval(T,T), Source) :-
	rdf(URI, sem:hasTimeStamp, literal(T), Source:_),
	(   T = type(_,TimeStamp)
	->  true
	;   TimeStamp = T
	).

owl_time_xsd_candidate(URI, interval(T,T), Source) :-
        rdf(URI, owltime:inXSDDateTime, literal(T), Source1),
	(   Source1 = Source:_ % work around
	->  true
	;   Source1 = Source
	),
        (   T = type(_,TimeStamp)
        ->  true
        ;   TimeStamp = T
        ).

%%	parse_timestamp(?TimeStampAtom, ?EpochTimeStamp) is det.
%
%	Converts in both directions between a literal time
%	representation and a numerical time representation based on the
%	epoch.
%	The format of the generated atom is ISO 8601 in UTC.
%
parse_timestamp(TimeStamp, Epoch) :-
	var(TimeStamp),
	number(Epoch),
	stamp_date_time(Epoch, Date, 'UTC'),
	format_time(atom(TimeStamp), '%FT%TZ', Date), !.
parse_timestamp(TimeStamp, Epoch) :- parse_timestamp(TimeStamp, Epoch, 0).

%%	parse_timestamp(+TimeStampAtom, -Epoch, +EpochOffset) is det.
%
%	Converts between a literal TimeStamp atom and a numerical
%	time representation based on the epoch - EpochOffset.
%	This allows for a more fine grained representation of time
%	for time points far away from the epoch.
%
parse_timestamp(TimeStamp, Epoch, EpochOffset) :-
	nonvar(TimeStamp),
	(   number(TimeStamp)
	->  E = TimeStamp
	;   atom(TimeStamp), iso_timestamp_epoch(TimeStamp, E), !
	;   atom(TimeStamp), sic_timestamp_epoch(TimeStamp, E), !
	;   \+atom(TimeStamp), timex_timestamp_epoch(TimeStamp, E), !
	% extend here
	),
	Epoch is E - EpochOffset.


iso_timestamp_epoch(TimeStamp, T) :-
	parse_time(TimeStamp, T).

sic_timestamp_epoch(TimeStamp, T) :-
	atom_number(TimeStamp, T).

timex_timestamp_epoch(type('http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral',TimeStamp), T) :-
	(   xpath(TimeStamp, //(timex2), element(_,Attr,_)) % plain XML timex2 tag
	->  true
	;   xpath(TimeStamp, //(_:timex2), element(_,Attr,_)) % timex2 in some namespace
	),
	memberchk('VAL'=ISO, Attr),
	parse_time(ISO, T). % Doesn't deal with local time





:- module(spacetimeindex,
	  [ spacetime_candidate/2,
	    spacetime_clear/1,
	    spacetime_clear/0,
	    spacetime_assert/4,
	    spacetime_assert/3,
	    spacetime_bulkload/3,
	    spacetime_bulkload/2,
	    spacetime_index_all/2,
	    spacetime_index_all/1,
	    spacetime_within_range/5,
	    spacetime_within_range/4,
	    spacetime_intersects/4,
	    spacetime_intersects/3,
	    bucket/3,
	    buckets/3
	  ]
	 ).

:- use_module('../../sem/sem').
:- use_module(library(space/space)).
:- use_module(library(space/timeindex)).

:- dynamic spacetime_index/4.

spacetime_candidate(URI, time_shape(Time,Shape)) :-
        uri_time(URI,Time),
        uri_shape(URI,Shape).
spacetime_candidate(URI, time_shape(Time,Shape)) :-
	property(URI,sem:hasPlace,Place),
	uri_time(URI,Time),
        uri_shape(Place,Shape).

bucket(Time,IntervalSize,Bucket) :-
       Bucket is floor(Time/IntervalSize) * IntervalSize.

buckets(point(T),IS,BS) :- buckets(T,T,IS,BS).
buckets(interval(T1,T2),IS,BS) :- buckets(T1,T2,IS,BS).
buckets(T1,T2,IntervalSize,[]) :- T1 >= T2 + IntervalSize, !.
buckets(T0,T2,IntervalSize,[Bucket|Tail]) :-
	bucket(T0,IntervalSize,Bucket),
	T1 is T0 + IntervalSize,
	buckets(T1,T2,IntervalSize,Tail).

bucket_indexname(Bucket,BaseName,IndexName) :-
       atom_number(BucketAtom,Bucket),
       atomic_list_concat([BaseName,BucketAtom],'_',IndexName).

spacetime_index_in_interval(point(T),IntervalSize,spacetime_index(Bucket,IS,BN,IndexName)) :-
	spacetime_index_in_interval(interval(T,T),IntervalSize,spacetime_index(Bucket,IS,BN,IndexName)).
spacetime_index_in_interval(interval(T1,T2),IntervalSize,spacetime_index(Bucket,IS,BN,IndexName)) :-
	From is T1 - IntervalSize,
	To is T2 + IntervalSize,
	spacetime_index(Bucket,IS,BN,IndexName),
	UBND is Bucket + IS,
	Bucket =< To,
	UBND >= From.

spacetime_clear :- spacetime_clear(st_default).
spacetime_clear(BaseName) :-
	findall(spacetime_index(B,I,BaseName,IndexName),
		spacetime_index(B,I,BaseName,IndexName),
		Indices),
	forall(member(spacetime_index(B,I,BN,Index), Indices),
	       (   space_clear(Index),
		   retractall(spacetime_index(B,I,BN,Index))
	       )).

spacetime_assert(URI,TS,IS) :- spacetime_assert(URI,TS,IS,st_default).
spacetime_assert(URI,time_shape(Time,Shape),IntervalSize,BaseName) :-
	buckets(Time,IntervalSize,Buckets),
	forall(member(Bucket,Buckets),
	       (   bucket_indexname(Bucket,BaseName,IndexName),
		   space_assert(URI,Shape,IndexName),
		   (   \+spacetime_index(Bucket,IntervalSize,BaseName,IndexName)
		   ->  assert(spacetime_index(Bucket,IntervalSize,BaseName,IndexName))
		   ;   true
		   )
	       )).

spacetime_bulkload(Candidate,IntervalSize) :-
	spacetime_bulkload(Candidate,IntervalSize,st_default).
spacetime_bulkload(Candidate,IntervalSize,BaseName) :-
	empty_nb_set(Assertions),
	forall(call(Candidate, URI, TimeShape),
	       (   spacetime_assert(URI, TimeShape, IntervalSize, BaseName),
		   add_nb_set(space_assert(URI,TimeShape),Assertions)
	       )),
	forall(spacetime_index(_,_,_,I),space_index(I)),
	size_nb_set(Assertions,N),
	format('% Added ~w URI-Time/Shape pairs to ~w\n',[N,BaseName]).

spacetime_index_all(IntervalSize) :- spacetime_index_all(IntervalSize,st_default).
spacetime_index_all(IntervalSize,BaseName) :-
	spacetime_bulkload(spacetime_candidate,IntervalSize,BaseName).



spacetime_within_range(time_shape(Time,Shape),Target,TimeInterval,SpaceRadius) :-
	spacetime_within_range(time_shape(Time,Shape),Target,TimeInterval,SpaceRadius,st_default).
spacetime_within_range(time_shape(Time,Shape),Target,TimeInterval,SpaceRadius,BaseName) :-
	spacetime_index_in_interval(Time,TimeInterval,spacetime_index(_Bucket,_IntervalSize,BaseName,IndexName)),
	space_within_range(Shape,Target,SpaceRadius,IndexName).

spacetime_intersects(time_shape(Time,Shape), Target, TimeInterval) :-
	spacetime_intersects(time_shape(Time,Shape), Target, TimeInterval, st_default).
spacetime_intersects(time_shape(Time,Shape), Target, TimeInterval, BaseName) :-
	spacetime_index_in_interval(Time,TimeInterval,spacetime_index(_Bucket,_IntervalSize,BaseName,IndexName)),
	space_intersects(Shape,Target,IndexName).








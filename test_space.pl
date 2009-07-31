:- use_module(library(plunit)).
:- use_module(space).
:- use_module(library(semweb/rdf_db)).

:- rdf_register_ns(poseidon,'http://semanticweb.cs.vu.nl/poseidon/ns/instances/').




:- begin_tests(space, [ setup( rdf_load(clearways) ),
			cleanup((   rdf_reset_db,
				    space_clear(test_index)
				)) ]).



test(space_clear, [ fail ]) :-
	I = test_index,
	space_clear(I),
	space_nearest(point(0.0,0.0),_N,I).

test(space_bulkload, [cleanup(space_clear(test_index))]) :-
	space_bulkload(space,uri_shape, test_index).

test(space_nearest) :-
	space_bulkload(space,uri_shape, test_index),
	space_nearest(point(0.0,0.0),_N,test_index), !.

% FIXME: fill in shape, because this test will always succeed now
test(shape, [ true(Maas1Shape = polygon(_)),
	      true(Maas4Shape = polygon(_)) ]) :-
	rdf_global_id(poseidon:'ScheepvaartrouteMaas_1',Maas1URI),
	rdf_global_id(poseidon:'ScheepvaartrouteMaas_4',Maas4URI),
	uri_shape(Maas1URI,Maas1Shape),
	uri_shape(Maas4URI,Maas4Shape),
	!.

test(space_intersects) :-
	I = test_index,
	rdf_global_id(poseidon:'ScheepvaartrouteMaas_1',Maas1URI),
	rdf_global_id(poseidon:'ScheepvaartrouteMaas_4',Maas4URI),
	uri_shape(Maas1URI,Maas1Shape),
	uri_shape(Maas4URI,Maas4Shape),
	rdf_global_id(poseidon:'ScheepvaartrouteZuid_richting_noord',NoordURI),
	findall(Inter,space_intersects(Maas1Shape,Inter,I),Inters),
	\+member(NoordURI,Inters),
	!,
	space_intersects(Maas4Shape,NoordURI,I).

test(space_index) :-
	I = test_index,
	rdf_global_id(poseidon:testPoint1,P1),
	rdf_global_id(poseidon:testPoint2,P2),
	rdf_global_id(poseidon:testPoint3,P3),
	space_assert(P1,point(52.4389,3.07118),I),
	space_assert(P2,point(52.3983,3.13086),I),
	space_assert(P3,point(52.3254,3.06849),I),
	space:space_queue(I,assert,P1,point(52.4389,3.07118)),
	space_index(I),
	\+space:space_queue(I,assert,P1,point(52.4389,3.07118)),
	space_retract(P1,point(52.4389,3.07118),I),
	space_retract(P2,point(52.3983,3.13086),I),
	space_retract(P3,point(52.3254,3.06849),I),
	space:space_queue(I,retract,P1,point(52.4389,3.07118)),
	space_index(I),
	\+space:space_queue(I,retract,P1,point(52.4389,3.07118)),
	space_assert(P1,point(52.4389,3.07118),I),
	space_assert(P2,point(52.3983,3.13086),I),
	space_assert(P3,point(52.3254,3.06849),I),
	space_index(I),
	!.


test(space_contains) :-
	I = test_index,
	rdf_global_id(poseidon:'ScheepvaartrouteMaas_4',Maas4URI),
	rdf_global_id(poseidon:testPoint1,P1),
	rdf_global_id(poseidon:testPoint2,P2),
	rdf_global_id(poseidon:testPoint3,P3),
	uri_shape(Maas4URI,Maas4Shape),
	findall(C,space_contains(Maas4Shape,C,I),Cs),
	\+member(P3,Cs),
	!,
	space_contains(Maas4Shape,P1,I),
	space_contains(Maas4Shape,P2,I).


test(space_intersects) :-
	I = test_index,
	rdf_global_id(poseidon:'ScheepvaartrouteZuid_richting_noord',NoordURI),
	rdf_global_id(poseidon:'ScheepvaartrouteMaas_4',Maas4URI),
	rdf_global_id(poseidon:testPoint1,P1),
	rdf_global_id(poseidon:testPoint3,P3),
	uri_shape(Maas4URI,Maas4Shape),
	findall(Int,space_intersects(Maas4Shape,Int,I),Ints),
	\+member(P3,Ints),
	!,
	space_intersects(Maas4Shape,P1,I),
	space_intersects(Maas4Shape,NoordURI,I).


test(space_nearest, [ true(Pts = [P2,P1,P3]) ]) :-
	I = test_index,
	rdf_global_id(poseidon:'Deep-draught_anchorage_Aanloopgebied_IJmuiden',DeepURI),
	rdf_global_id(poseidon:testPoint1,P1),
	rdf_global_id(poseidon:testPoint2,P2),
	rdf_global_id(poseidon:testPoint3,P3),
	uri_shape(DeepURI,DeepShape),
	findall(Pt,(space_nearest(DeepShape,Pt,I),
		    rdf_global_id(poseidon:Lt,Pt),
		    atom_concat(testPoint,_,Lt)), Pts),
	!.

test(space_retract, [ true(Ps = [P2,P3]) ]) :-
	I = test_index,
	rdf_global_id(poseidon:'Deep-draught_anchorage_Aanloopgebied_IJmuiden',DeepURI),
	rdf_global_id(poseidon:testPoint1,P1),
	rdf_global_id(poseidon:testPoint2,P2),
	rdf_global_id(poseidon:testPoint3,P3),
	uri_shape(DeepURI,DeepShape),
	space_retract(P1,point(52.4389,3.07118),I),
	findall(P,(space_nearest(DeepShape,P,I),
		   rdf_global_id(poseidon:L,P),
		   atom_concat(testPoint,_,L)), Ps),
	!.

:- end_tests(space).

test_space :-
	run_tests(space).


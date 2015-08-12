(* Following tradition, a simple Hello world! *)


open Printf ;;
open List ;;
open Format ;;
open Pervasives ;;
let print_list (lst) =
	List.iter (printf "%d " ) lst ;
	printf "\n";;

let print_list_list (lst) = List.iter print_list lst ;;

(* here real TSP starts *)


let rec insert(path, p) =
	if path = []
	then
		[p]
	else
		hd(path) :: insert(tl(path), p) ;;

let rec elemn(visit,idx) =
	if idx = 0
	then
		hd(visit)
	else
		elemn(tl(visit),idx - 1) ;;

let rec remove(visit, p) =
	if visit = []
	then
		[]
	else
		if hd(visit) = p
		then
			tl(visit)
		else
			hd(visit) :: remove(tl(visit), p) ;;

let rec contains(visit, p) =
	if visit = []
	then
		false
	else
		if hd(visit) = p
		then
			true
		else
			contains(tl(visit), p) ;;

let rec hamiltonian(p, path, visit) =
	let e = elemn(path, p - 1)
	in
		if contains(visit, e)
		then
			let visit2 = remove(visit, e)
			in	
				if visit2 = []
				then
					true
				else
					hamiltonian(e,path,visit2)
		else
			false ;;

let rec optimize(dom, dist, path, cost, ub, visit) =
(*	uncomment for debugging
	printf "%i %i\n" cost ub ;
	print_list( path ) ;
	printf "---\n" ; *)
	if dom = []
	then
		if hamiltonian(1, path, visit)
		then
			(cost, path)
		else
			(-1, [])
	else
		if length(hd(dom)) = 1
		then
			if cost + hd(hd(dist)) <= ub
			then
				optimize(tl(dom), tl(dist), insert(path, hd(hd(dom))), hd(hd(dist)) + cost, ub, visit)
			else
				(* violates upper bound condition, backtrack *)
				(-1, [])
		else
			let (pub, ppath) = optimize(tl(dom), tl(dist), insert(path, hd(hd(dom))), hd(hd(dist)) + cost, ub, visit)
			in
				(* check if feasible *)
				if pub != -1
				then
					(* check if upper bound is improved *)
					if pub <= ub
					then
						let (qub, qpath) = optimize(tl(hd(dom)) :: tl(dom), tl(hd(dist)) :: tl(dist), path, cost, pub, visit)
						in
							(* check if other branch is feasible *)
 							if qub != -1
							then
								(* return better solution *)
								if pub < qub
								then
									(pub, ppath)
								else if qub < pub
								then
									(qub, qpath)
								else
									(* solutions equivalent *)
									(qub, qpath)
							else
								(* return previously found, feasible *)
								(pub, ppath)
					else
						(* same as previous branch, but with older ub *)
						optimize(tl(hd(dom)) :: tl(dom), tl(hd(dist)) :: tl(dist), path, cost, ub, visit)
				else
					optimize(tl(hd(dom)) :: tl(dom), tl(hd(dist)) :: tl(dist), path, cost, ub, visit) ;;


(* test instance 1 
let d1 = [2;3;4] ;;
let d2 = [1;3;4] ;;
let d3 = [1;2;4] ;;
let d4 = [1;2;3] ;;

let doms = [d1;d2;d3;d4] ;;

let dd1 = [390;1326;744] ;;
let dd2 = [390;339;738] ;;
let dd3 = [1326;339;648] ;;
let dd4 = [744;738;648] ;;

let dists = [dd1;dd2;dd3;dd4] ;;

let cst,tour = optimize(doms, dists, [], 0, 9999, [1;2;3;4]) ;;

printf "cost %i\n tour : " cst ;;
print_list( tour );;
*)

(* test instance 2 
let d1 = [2;3;4;5;6] ;;
let d2 = [1;3;4;5;6] ;;
let d3 = [1;2;4;5;6] ;;
let d4 = [1;2;3;5;6] ;;
let d5 = [1;2;3;4;6] ;;
let d6 = [1;2;3;4;5] ;;

let doms = [d1;d2;d3;d4;d5;d6] ;;

let dd1 = [390;1326;744;678;678] ;;
let dd2 = [390;939;738;929;1037] ;;
let dd3 = [1326;939;648;501;755] ;;
let dd4 = [744;738;648;159;128] ;;
let dd5 = [678;929;501;159;247] ;;
let dd6 = [678;1037;755;128;247] ;;

let dists = [dd1;dd2;dd3;dd4;dd5;dd6] ;;

let cst,tour = optimize(doms, dists, [], 0, 99999, [1;2;3;4;5;6]) ;;

printf "cost %i\n tour : " cst ;;
print_list( tour );;
*)

(* test of Hamiltonian path check
let testpath1 = [2;3;4;1] ;;
let testvisit1 = [1;2;3;4] ;;
let testham1 = hamiltonian(1, testpath1, testvisit1) ;;
printf "%B\n" testham1 ;;
printf "\n";;

let testpath2 = [2;1;4;3] ;;
let testvisit2 = [1;2;3;4] ;;
let testham2 = hamiltonian(1, testpath2, testvisit2) ;;
printf "%B\n" testham2 ;;
printf "\n";;
*)

(* test of utility functions
let test1 = contains([1;2;3;4],8) ;;
printf "8 in {1;2;3;4} %B\n" test1 ;;
let test2 = contains([1;2;3;4],4) ;;
printf "4 in {1;2;3;4} %B\n" test2 ;;
let test3 = elemn([1;2;3;4],2) ;;
printf "{1;2;3;4}[2] %i\n" test3 ;;
let test4 = elemn([1;2;3;4],3) ;;
printf "{1;2;3;4}[3] %i\n" test4 ;;
*)

(* begin *)

printf "TSPML, traveling salesman problem OCaml demo \n" ;;
printf "by Marek Mateusz Narozniak \n" ;;
printf "marek.narozniak (at) gmail.com \n\n" ;;

(* get instance filename *)

printf "Processing instance : %s\n" Sys.argv.(1) ;;

(* read file *)

let cities = [] ;;
let doms = [] ;;
let dist = [] ;;

let rec output_cities ( cities, tour, citiesc ) =
	let n = length(cities)
	in
		if n > 0
		then
			let from = hd(cities)
			in
				let dest = elemn(citiesc, hd(tour) - 1)
				in
					printf "city[ %i ] = %s -> %s \n" n from dest ;
					output_cities ( tl(cities), tl(tour) , citiesc )
		else
			printf "\n" ;;

let rec build_all( n ) =
	if n == 0
	then
		[]
	else
		n :: build_all( n - 1 ) ;;

let rec build_domain_single( n, i ) =
	if i == 0
	then
		[]
	else
		if i != n
		then
			insert( build_domain_single( n , i - 1 ) , i )
		else
			build_domain_single( n, i - 1 ) ;;

let rec build_domain( n, nc ) =
	if n = 0
	then
		[]
	else
		insert( build_domain( n - 1, nc ),  build_domain_single( n, nc )) ;;

let rec read_distances_single ( ic, n) =
	if n = 0
	then
		[]
	else
		try let line = input_line ic in
		let d = int_of_string line
		in
			insert( read_distances_single (ic , n - 1), d)
	with e -> 
		close_in_noerr ic ;
		raise e

let rec read_distances ( ic, n, nc ) =
	if n = 0
	then
		[]
	else
		insert(read_distances ( ic, n - 1, nc ), read_distances_single(ic, nc - 2));;

let rec read_cities ( ic , n ) = 
	if n = 0
	then
		[]
	else
		try let city = input_line ic in
			city :: read_cities ( ic , n - 1 )
		with e -> 
			close_in_noerr ic ;
			raise e

let read_instance ( file ) =
	let ic = open_in file in
	try let line = input_line ic in
		(* get number of cities *)
		let n = int_of_string line
		in
			let cities = read_cities( ic, n)
			in
				let dist = read_distances( ic, n, n + 1)
				in
					let dom = build_domain( n, n )
					in
						(n, cities, dist, dom)
	with e -> 
		close_in_noerr ic ;
		raise e

let k, cities, dist, dom = read_instance ( Sys.argv.(1) ) ;;

let visit = build_all( k ) ;;

(* solve *)

let cst,tour = optimize(dom, dist, [], 0, 999999999, visit) ;;


printf "tour cost : %i \n" cst ;;
print_list( tour ) ;;
printf "\n" ;;
output_cities( cities , tour, cities ) ;;

printf "model processed correctly \n" ;;

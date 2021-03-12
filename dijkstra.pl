actor([1,1]).
covid([1,7]).
covid([3,9]).
visited([]).
queue([[1,1]]).
queue_protected([]).
path([[1,1]]).
path_protected([]).
doctor([9,1]).
mask([9,9]).
mask([0,0]).
home([1,9]).
tmp_path([]).
%map([9,9]).

:- dynamic([
	actor/1,
	covid/1,
	mask/1,
	doctor/1,
	home/1, 
	visited/1,
	queue/1,
	queue_protected/1,
	path/1,
	path_protected/1,
	tmp_path/1,
	map/1
	]).

adj_list([X,Y], List) :-
	X1 is X+1,
	X2 is X-1,
	Y1 is Y+1,
	Y2 is Y-1,
	List = [[X1,Y],[X2,Y],[X,Y1],[X,Y2],[X1,Y1],[X1,Y2],[X2,Y1],[X2,Y2]].

valid_move([X,Y]) :-
	between(1, 9, X),
	between(1, 9, Y).

is_infected([X,Y]) :-
	covid([X1,Y1]),
	X0 is X1 - 1, X2 is X1 + 1,
	Y0 is Y1 - 1, Y2 is Y1 + 1,
	between(X0, X2, X),
	between(Y0, Y2, Y).

is_safe([X,Y]) :-
	not(is_infected([X,Y])).

choose_check(Elem, List, Curr):-
	visited(Visited), 
	queue(Queue), 
	queue_protected(Queue_Protected),


	nth0(Elem, List, Direction),
	valid_move(Direction),
	not(member(Direction, Visited)),
	not(member(Direction, Queue)),
	not(member(Direction, Queue_Protected)),
	is_safe(Direction),

	path(Path), last(Path, Curr), append(Path, [Direction], New_Path), asserta(path(New_Path)),


	(
		(mask(Direction); doctor(Direction)) -> 
		append(Queue_Protected, [Direction], New_Queue_Protected), retract(queue_protected(Queue_Protected)), 
		asserta(queue_protected(New_Queue_Protected))
		%,write("Curr: "), writeln(Curr), write("New_Queue_Protected: "), writeln(New_Queue_Protected)
		;
		append(Queue, [Direction], New_Queue), retract(queue(Queue)), asserta(queue(New_Queue))
	).

recursive_walk():-
	queue([Head|Tail]),
	%write("Queue: "), writeln([Head|Tail]), 
	%write("Head: "), writeln(Head), 
	retract(queue([Head|Tail])), asserta(queue(Tail)),
	adj_list(Head, Adj_List),


	(
		( choose_check(0, Adj_List, Head) -> true; true ),
		( choose_check(1, Adj_List, Head) -> true; true ),
		( choose_check(2, Adj_List, Head) -> true; true ),
		( choose_check(3, Adj_List, Head) -> true; true ),
		( choose_check(4, Adj_List, Head) -> true; true ),
		( choose_check(5, Adj_List, Head) -> true; true ),
		( choose_check(6, Adj_List, Head) -> true; true ),
		( choose_check(7, Adj_List, Head) -> true; true )
	),

	%queue(Q), write("New Queue: "), writeln(Q),

	visited(Visited), 
	append(Visited, [Head], New_Visited),
	retract(visited(Visited)), assert(visited(New_Visited)),
	%write("Visited: "), writeln(New_Visited),
	recursive_walk().

make_equal([X,Y], [XH,YH]):-
	%write("XY: "), writeln([X,Y]),
	tmp_path(Path),
	last(Path, [X,Y]),
	%write("Path: "), writeln(Path),

	(
		(X=XH, Y=YH) -> false ; true
	),
	(
		(
			(X>XH, Y>YH) -> New_X is X-1, New_Y is Y-1; false
		);
		(
			(X>XH, Y=YH) -> New_X is X-1, New_Y is Y; false
		);
		(
			(X>XH, Y<YH) -> New_X is X-1, New_Y is Y+1; false
		);
		(
			(X=XH, Y>YH) -> New_X is X, New_Y is Y-1; false
		);
		(
			(X=XH, Y<YH) -> New_X is X, New_Y is Y+1; false
		);
		(
			(X<XH, Y=YH) -> New_X is X+1, New_Y is Y; false
		);
		(
			(X<XH, Y>YH) -> New_X is X+1, New_Y is Y-1; false
		);
		(
			(X<XH, Y<YH) -> New_X is X+1, New_Y is Y+1; false
		)
		 
	),
	append(Path, [[New_X,New_Y]], New_Path), asserta(tmp_path(New_Path)),
	%write("New_Path: "), writeln(New_Path),
	make_equal([New_X,New_Y], [XH,YH]).


find_minimal_path() :-
	queue_protected([[X,Y]|Tail]),
	path(Path_to_m_d), last(Path_to_m_d, [X,Y]),
	%write("head: "), writeln([X,Y]),
	retractall(tmp_path(_)), assert(tmp_path(Path_to_m_d)),

	retract(queue_protected([[X,Y]|Tail])), asserta(queue_protected(Tail)),
	home([XH,YH]),

	( make_equal([X,Y], [XH,YH]); true),

	tmp_path(Path_from_m_d), last(Path_from_m_d, [XH,YH]),
	%write("AAAAAAAAAAAAAAAAAPath_from_m_d: "), writeln(Path_from_m_d),
	%forall(Path_from_m_d, writeln(Path_from_m_d)),
	%append(Path_to_m_d, Path_from_m_d, Protected_Path),

	%write("Path_from_m_d2: "), writeln(Protected_Path),
	( choose_min(Path_from_m_d) -> true;  true ),
	find_minimal_path().

choose_min1(List):-
	home([HX,HY]),
	path(Path), 
	length(List, L2),
	( 
		last(Path, [HX,HY]) -> length(Path, L1); L1 is (L2 + 10)
	), 
	retractall(path(_)), 
	(
		(L1>=L2) -> assert(path(List)), write("LAlalal"), writeln(List); assert(path(Path)), write("NO"), writeln(Path)
	).

choose_min(List):-
	%write('NewList: '), write(List), writeln(''),
	home([HX,HY]),
	length(List, L2),
	( 
		(path(Path), last(Path, [HX,HY])) -> true; retract(path(Path)), asserta(path(List)), false
	), 
	(path(Path), last(Path, [HX,HY])),
	length(Path, L1),
	%write('CurrentList: '), write(Path), writeln(''),
	(
		(L1 > L2) -> retract(path(Path)), asserta(path(List)), true; false
	).

generator(Actor, Covid1, Covid2, Mask, Doctor):-

	retract(actor(_)), assert(actor(Actor)),
	retract(covid(_)), assert(covid(Covid1)), assert(covid(Covid2)),
	retract(mask(_)), assert(mask(Mask)),
	retract(doctor(_)), assert(doctor(Doctor)),

	retract(queue(_)), assert(queue(Actor)),
	retract(path(_)), assert(path(Actor)).

game():-
	%generator(Actor, Covid1, Covid2, Mask, Doctor),
	( 
		recursive_walk();
		find_minimal_path() ; true), home([HX, HY]),

	path(Path), 
	( last(Path, [HX, HY]) -> length(Path, L), write("Steps: "), writeln(L),
		write("Path: "), writeln(Path) ; write("Loose")).



main:-
	game().





actor([1,1]).
covid([3,3]).
visited([[1,1]]).
:- dynamic visited/1.

adj_list([X,Y], List) :-
	X1 is X+1,
	X2 is X-1,
	Y1 is Y+1,
	Y2 is Y-1,
	List = [[X1,Y],[X2,Y],[X,Y1],[X,Y2],[X1,Y1],[X1,Y2],[X2,Y1],[X2,Y2]].

valid_move([X,Y]) :-
	between(1, 9, X),
	between(1, 9, Y).

choose_check(Elem, List, Direction):-
	visited(Visited),

	nth0(Elem, List, Direction), 
	(	(valid_move(Direction), not(member(Direction, Visited))) -> 
		append(Visited, [Direction], New_Visited), retract(visited(Visited)), assert(visited(New_Visited)), true
		;
		false
	).

recursive_walk([Cur_X,Cur_Y]):-
	%visited(Visited),
	adj_list([Cur_X,Cur_Y], Adj_List),

	(
		choose_check(0, Adj_List, Right) -> recursive_walk(Right); true
	),
	(
		choose_check(1, Adj_List, Left) -> recursive_walk(Left); true
	),
	(
		choose_check(2, Adj_List, Up) -> recursive_walk(Up); true
	),
	(
		choose_check(3, Adj_List, Down) -> recursive_walk(Down); true
	),
	(
		choose_check(4, Adj_List, Up_Right) -> recursive_walk(Up_Right); true
	),
	(
		choose_check(5, Adj_List, Down_Right) -> recursive_walk(Down_Right); true
	),
	(
		choose_check(6, Adj_List, Up_Left) -> recursive_walk(Up_Left); true
	),
	(
		choose_check(7, Adj_List, Down_Left) -> recursive_walk(Down_left); true
	).


main:-
	adj_list([1,1], List), writeln(List),
	choose_check(0, List, Direction), visited(Q), write(Q).


actor(1, 1).
home(9, 9).
covid(3, 2).
covid(8, 8).
doctor(8, 3).
mask(5, 7).
covid(6, 2).
%covid(6, 2).
:- dynamic minimal_path/1.
minimal_path([]).

is_covid(X, Y) :-
    covid(X, Y).

get_protected(X, Y) :-
    doctor(X, Y).
get_protected(X, Y) :-
     mask(X, Y).

is_infected(X, Y) :-
      covid(X1, Y1),
      X0 is X1 - 1, X2 is X1 + 1,
      Y0 is Y1 - 1, Y2 is Y1 + 1,
      between(X0, X2, X),
      between(Y0, Y2, Y).

move_up(X, Y, New_X, New_Y) :- New_X is X, New_Y is Y + 1.
move_down(X, Y, New_X, New_Y) :- New_X is X, New_Y is Y - 1.
move_left(X, Y, New_X, New_Y) :- New_X is X - 1, New_Y is Y.
move_right(X, Y, New_X, New_Y) :- New_X is X + 1, New_Y is Y.
move_up_right(X, Y, New_X, New_Y) :- New_X is X + 1, New_Y is Y + 1.
move_down_right(X, Y, New_X, New_Y) :- New_X is X + 1, New_Y is Y - 1.
move_up_left(X, Y, New_X, New_Y) :- New_X is X - 1, New_Y is Y + 1.
move_down_left(X, Y, New_X, New_Y) :- New_X is X - 1, New_Y is Y - 1.

valid_move(X, Y) :- 
 between(1, 9, X),
 between(1, 9, Y).

i_am_at(X, Y) :-
    write('('), write(X), write(','), write(Y), writeln(')').

recursive_walk(X, Y, Visited, Protected) :-
       (   
        home(X, Y) ->  write("Lovely home: "), length(Visited, Len), write(Len),
        write("my path:"), writeln(Visited), 
        minimal_path(Z),
        (
            length(Z, 0) -> retract(minimal_path(Z)),
            asserta(minimal_path(Visited))
            ;
            length(Z, L1), length(Visited, L2),
            ( 
            L1 > L2 ->
            retract(minimal_path(Z)),
            asserta(minimal_path(Visited))
            )
        ),
        false
       );
    (
        minimal_path(Z),
        length(Z, L1), 
        length(Visited, L2),
            ( L3 is L2+1,
            ((L3 >= L1), (L1 > 0)) ->
            false
            ; write('')
            )
      ),

      (   
        (
        move_up(X, Y, New_X, New_Y), valid_move(New_X, New_Y), 
        not(member((New_X;New_Y), Visited)), 
        (Protected; not(is_infected(New_X, New_Y))) ->  
        %i_am_at(New_X, New_Y), 
        append(Visited, [(New_X;New_Y)], Visited1), 
        recursive_walk(New_X, New_Y, Visited1, (Protected ; get_protected(New_X, New_Y))) 
        ; false
        );

        (   
        move_up_right(X, Y, New_X, New_Y), valid_move(New_X, New_Y),
        not(member((New_X;New_Y), Visited)), 
        (Protected; not(is_infected(New_X, New_Y))) ->  
        %i_am_at(New_X, New_Y), 
        append(Visited, [(New_X;New_Y)], Visited1), 
        recursive_walk(New_X, New_Y, Visited1, (Protected; get_protected(New_X, New_Y)))
        ; false
        );

        (   
        move_up_left(X, Y, New_X, New_Y), valid_move(New_X, New_Y),
        not(member((New_X;New_Y), Visited)), 
        (Protected; not(is_infected(New_X, New_Y))) ->
        %i_am_at(New_X, New_Y), 
        append(Visited, [(New_X;New_Y)], Visited1), 
        recursive_walk(New_X, New_Y, Visited1, (Protected; get_protected(New_X, New_Y)))
        ; false
        );

        (   
        move_down(X, Y, New_X, New_Y), valid_move(New_X, New_Y),
        not(member((New_X;New_Y), Visited)), 
        (Protected; not(is_infected(New_X, New_Y))) ->  
        %i_am_at(New_X, New_Y), 
        append(Visited, [(New_X;New_Y)], Visited1), 
        recursive_walk(New_X, New_Y, Visited1, (Protected; get_protected(New_X, New_Y)))
        ; false
        );

        (   
        move_down_right(X, Y, New_X, New_Y), valid_move(New_X, New_Y),
        not(member((New_X;New_Y), Visited)), 
        (Protected; not(is_infected(New_X, New_Y))) -> 
        %i_am_at(New_X, New_Y), 
        append(Visited, [(New_X;New_Y)], Visited1), 
        recursive_walk(New_X, New_Y, Visited1, (Protected; get_protected(New_X, New_Y)))
        ; false
        );

        (   
        move_down_left(X, Y, New_X, New_Y), valid_move(New_X, New_Y),
        not(member((New_X;New_Y), Visited)),
        (Protected; not(is_infected(New_X, New_Y))) ->
        %i_am_at(New_X, New_Y), 
        append(Visited, [(New_X;New_Y)], Visited1), 
        recursive_walk(New_X, New_Y, Visited1, (Protected; get_protected(New_X, New_Y)))
        ; false
        );

        (   
        move_right(X, Y, New_X, New_Y), valid_move(New_X, New_Y),
        not(member((New_X;New_Y), Visited)), 
        (Protected; not(is_infected(New_X, New_Y))) -> 
        %i_am_at(New_X, New_Y), 
        append(Visited, [(New_X;New_Y)], Visited1), 
        recursive_walk(New_X, New_Y, Visited1, (Protected; get_protected(New_X, New_Y)))
        ; false
        );

        (   
        move_left(X, Y, New_X, New_Y), valid_move(New_X, New_Y),
        not(member((New_X;New_Y), Visited)), 
        (Protected; not(is_infected(New_X, New_Y))) -> 
        %i_am_at(New_X, New_Y), 
        append(Visited, [(New_X;New_Y)], Visited1), 
        recursive_walk(New_X, New_Y, Visited1, (Protected; get_protected(New_X, New_Y)))
        ; false
        )
      ).
game() :-
    recursive_walk(1, 1, [(1;1)], get_protected(1, 1));
    (minimal_path(Z), write(Z)).

 main :- write('').
 ?- game().


%a)
initial_board(Length, Width, Board) :-
    length(Row, Width),
    maplist(=(0), Row),
    length(Board, Length),
    maplist(=(Row), Board).

%b)
print_board([]).
print_board([H|T]) :-
    atomic_list_concat(H,' ',Hconcat),
    write(Hconcat),nl,print_board(T).

%d)
add_move(Player,X,Y,InitialBoard,FinalBoard) :-
    nth0(X,InitialBoard,Other),
    replace(Y,Other,Player,Final),
    replace(X,InitialBoard,Final,FinalBoard).

replace(_, _, [], []).
replace(Position,Input,Value,Output) :-
    nth0(Position,Input,_,Other),
    nth0(Position,Output,Value,Other).

%e)
generate_move(_,[],[]).
generate_move(Player,[H|T],[FinalLine|FinalBoard]) :-
    write('Header: '),
    write(H),nl,
    findall(X0,(select(0, H, Player, X0)),FinalLine),
    write('Results:'),
    write(FinalLine),nl,
    join_results(FinalLine,T,FinalBoard),
    write('exit'),nl,
    write(FinalBoard),nl,
    generate_move(Player,T,FinalBoard).


%juntar cada resultado na lista final
join_results([],[],[]).
join_results([H|T],LeftOvers,FinalBoard) :-
    write('join'),nl,
    write(H),nl,
    write(T),nl,
    write(LeftOvers),nl,
    append([H],LeftOvers,FinalBoard),
    write(FinalBoard),nl.
    %join_results(T,LeftOvers,FinalBoard).

%f)
% condições de game_over
game_over(Board, Player) :- rowWin(Board, Player).
game_over(Board, Player) :- columnWin(Board, Player).
game_over(Board, Player) :- diagonalWin(Board, Player).
game_over(Board, Player) :- draw(Board, Player).

% empate - sem posições livres
draw(Board, 0):-
    \+(rowWin(Board, _)),
    \+(columnWin(Board, _)),
    \+(diagonalWin(Board, _)).

%4 peças consecutivas numa coluna
columnWin(Board, Player) :-
    append(_,[Column|_], Board),
    append(_,[Player, Player, Player, Player|_], Column).

% 4 peças consecutivas numa linha
rowWin(Board, Player) :-
    transpose(Board, Board1),
    columnWin(Board1, Player).

transpose(A, B) :- transpose(A, [], B).
transpose(M, X, X) :- empty(M), !.
transpose(M, A, X) :- columns(M, Hs, Ts), transpose(Ts, [Hs|A], X).

empty([[]|A]) :- empty(A).
empty([]).

columns([[Rh|Rt]|Rs], [Rh|Hs], [Rt|Ts]) :- columns(Rs, Hs, Ts).
columns([[]], [], []).
columns([], [], []).

% 4 peças consecutivas numa diagonal (\)
diagonalWin(Board, Player) :- diagonalRight(Board, Player).

% 4 peças consecutivas numa diagonal (/)
diagonalWin(Board, Player) :- diagonalLeft(Board, Player).

% checkar a diagonal (\)
diagonalRight(Board, Player) :-
    append(_,[Column1, Column2, Column3, Column4|_], Board),
    append(Elem1, [Player|_], Column1),
    append(Elem2, [Player|_], Column2),
    append(Elem3, [Player|_], Column3),
    append(Elem4, [Player|_], Column4),
    length(Elem1, N1),
    length(Elem2, N2),
    length(Elem3, N3),
    length(Elem4, N4),
    N2 is N1 + 1,
    N3 is N2 + 1,
    N4 is N3 + 1.

% checkar a diagonal (/)
diagonalLeft(Board, Player) :-
    append(_,[Column1, Column2, Column3, Column4|_], Board),
    append(Elem1, [Player|_], Column1),
    append(Elem2, [Player|_], Column2),
    append(Elem3, [Player|_], Column3),
    append(Elem4, [Player|_], Column4),
    length(Elem1, N1),
    length(Elem2, N2),
    length(Elem3, N3),
    length(Elem4, N4),
    N2 is N1 - 1,
    N3 is N2 - 1,
    N4 is N3 - 1.


%g)
calculate_board_value(1,Value) :-
    Value = 1.
calculate_board_value(2,Value) :-
    Value = -1.
calculate_board_value(0,Value) :-
    Value = 0.

%h)
calculate_board_heuristic(_,_,Value) :-
    Value = 0.

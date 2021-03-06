%next_player(Player1, Player2) - permite saber qual é o próximo jogador
next_player(1,2).
next_player(2,1).

%play_game/0
% Este predicado começa por criar um tabuleiro com a dimensao 6x6 e
% validar a jogada do computador (player 1).
play_game():-
    initial_board(6, 6, B0),
    play(1,B0).

%play/1
%O predicado play tem como argumentos o jogador que deve jogar e o tabuleiro sobre o qual deve fazer a sua jogada.
% Este predicado e' recursivo de modo a permitir a alternancia das
% jogadas. A sua implementaçao e constituida pela jogada do computador
% (play(1,B)) , pela jogada do jogador (play(2,B)) e pela verificaçao
% do fim do jogo.
play(_,B0):-
    game_over(B0,T),
    calculate_board_value(T,Value),
    print_board(B0),
    write('Game Over!'),
    nl,
    display(Value),
    !.

play(1,B0):-
    write('Player:'),nl,
    print_board(B0),
    alphabeta(B0,6,-100,100,B1,_,1),
    !,
    play(2,B1).

play(2,B0):-
    write('Computer:'),nl,
    print_board(B0),
    write('Where to play? (C,L)'),
    read(C),
    read(L),
    valid_move(C,L,B0),
    add_move(2,C,L,B0,B1),
    !,
    play(1,B1).

%alphabeta/7
%minimax-alpha-beta
% O predicado que implementa o minimax e' chamado alfabeta e tem como
% argumentos o tabuleiro, o valor de profundidade que ainda e' permitido
% explorar, o alfa, o beta, o tabuleiro resultado, o score da avaliaçao
% do resultado na o'tica do computador e o jogador que se esta' a
% avaliar (minimizar ou a maximizar).initial_board(Length, Width, Board) :-
%   length(Row, Width),
%   maplist(=(0), Row),
%   length(Board, Length),
%   maplist(=(Row), Board).
alphabeta(Bi, 0, _, _, Bi, Value, P):-
    calculate_board_heuristic(P,Bi,Value),
    !.

alphabeta(Bi, _, _, _, Bi, Value, _):-
    game_over(Bi,T),
    calculate_board_value(T,Value),
    !.


alphabeta(Bi, D, Alfa, Beta, Bf, Value, Player):-
    next_player(Player,Other),
    possible_moves(Player,Bi,L),
    !,
    evaluate_child(Other, L, D, Alfa, Beta, Bf, Value).

%evaluate_child/7
evaluate_child(Player, [B], D, Alfa, Beta, B, Value):-
    D1 is D-1,
    !,
    alphabeta(B, D1, Alfa, Beta, _, Value, Player).


evaluate_child(2, [Bi|T], D, Alfa, Beta,Bf, Value):-
    D1 is D-1,
    alphabeta(Bi, D1, Alfa, Beta, _, Value1, 2),
    !,
    evaluate_next_child_max(Value1,Bi, T, D, Alfa, Beta, Value, Bf).

evaluate_child(1, [Bi|T], D, Alfa, Beta,Bf, Value):-
    D1 is D-1,
    alphabeta(Bi, D1, Alfa, Beta, _, Value1, 1),
    !,
    evaluate_next_child_min(Value1,Bi, T, D, Alfa, Beta, Value, Bf).

%evaluate_next_child_max/8
evaluate_next_child_max(Value1,Bi, T, D, Alfa, Beta, Value, Bf):-
    Value1 < Beta,
    max(Value1,Alfa,NewAlfa),
    !,
    evaluate_child(2, T, D, NewAlfa, Beta, B2, Value2),
    max_board(Value1,Bi,Value2,B2,Value,Bf).

evaluate_next_child_max(Value1,Bi, _, _, _, _, Value1, Bi):- !.

%evaluate_next_child_min/8
evaluate_next_child_min(Value1,Bi, T, D, Alfa, Beta, Value, Bf):-
     Value1 > Alfa,
     min(Value1,Beta,NewBeta),
     !,
     evaluate_child(1, T, D, Alfa, NewBeta, B2, Value2),
     min_board(Value1,Bi,Value2,B2,Value,Bf).

evaluate_next_child_min(Value1,Bi, _, _, _, _, Value1, Bi):- !.


%possible_moves/3
possible_moves(X,B,L):-
    bagof(BP,generate_move(X,B,BP),L).

%max_board/6
max_board(Value1,B1,Value2,_,Value1,B1):-
    Value1 >= Value2.

max_board(Value1,_,Value2,B2,Value2,B2):-
    Value1 < Value2.

%min_board/6
min_board(Value1,B1,Value2,_,Value1,B1):-
    Value1 =< Value2.

min_board(Value1,_,Value2,B2,Value2,B2):-
    Value1 > Value2.

%max/3
max(X,Y,X):-
    X>=Y,!.
max(X,Y,Y):-
    Y>X.
%min/3
min(X,Y,X):-
    X=<Y,!.
min(X,Y,Y):-
    Y<X.

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
%c)
valid_move(X,Y,Board):-
    line(Y,X,Board,Value),
    Value == 0.

line(NColumn,0,[H|_],Value):-
    column(NColumn,H,Value).
line(NColumn,NLine,[_|T],Line):-
    Y is NLine-1,
    line(NColumn,Y,T,Line).

column(0,[H|_],H).
column(NColumn,[_|T],Column):-
    Y is NColumn-1,
    column(Y,T,Column).

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
generate_move(Player,[H|T],FinalBoard):-
    length(H,LenY),
    between(0,LenY,Y),
    length([H|T],LenX),
    between(0,LenX,X),
    add_move(Player,X,Y,[H|T],FinalBoard).

%f)
% condi��es de game_over
game_over(Board, Player) :- rowWin(Board, Player).
game_over(Board, Player) :- columnWin(Board, Player).
game_over(Board, Player) :- diagonalWin(Board, Player).
game_over(Board, Player) :- draw(Board, Player).

% empate - sem posi��es livres
draw(Board, 0):-
    \+(rowWin(Board, _)),
    \+(columnWin(Board, _)),
    \+(diagonalWin(Board, _)).

%4 pe�as consecutivas numa coluna
columnWin(Board, Player) :-
    append(_,[Column|_], Board),
    append(_,[Player, Player, Player, Player|_], Column).

% 4 pe�as consecutivas numa linha
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

% 4 pe�as consecutivas numa diagonal (\)
diagonalWin(Board, Player) :- diagonalRight(Board, Player).

% 4 pe�as consecutivas numa diagonal (/)
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

:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(between)).

% congratulate_winner(+Winner)
% Displays a congratulatory message for the winner
congratulate_winner(Winner) :-
    write('+---------------------------+'), nl,
    write(''), nl,
    write(''), nl,
    write('           WINNER!          '), nl,
    write(''), nl,
    format('            ~w           ', [Winner]), nl,
    write(''), nl,
    write(''), nl,
    write('+---------------------------+').

% sum_list(+List, -Sum)
% Sum the elements of a list
sum_list(List, Sum) :-
    sum_list(List, 0, Sum).
sum_list([], Acc, Acc).
sum_list([Head|Tail], Acc, Sum) :-
    NewAcc is Acc + Head,
    sum_list(Tail, NewAcc, Sum).

% within_bounds(+X, +Y, +Size)
% Check if a position is within bounds for a given board size
within_bounds(X, Y, Size) :-
    between(1, Size, X),
    between(1, Size, Y).

% count_kings(+Board, +Player, -Count)
% Count the number of kings on the board for a specific player
count_kings(Board, Player, Count) :-
    findall(_, (member(Row, Board), member(Piece, Row), king_piece(Player, Piece)), Kings),
    length(Kings, Count).

% count_kings(+Board, -Count)
% Count the total number of kings on the board
count_kings(Board, Count) :-
    findall(_, (member(Row, Board), member(Piece, Row), (Piece = kw; Piece = kb)), Kings),
    length(Kings, Count).

% clear_path_jump(+Board, +CX, +CY, +NX, +NY, +CurrentPlayer)
clear_path_jump(Board, CX, CY, NX, NY, CurrentPlayer) :- % Ensure the path is clear for jumps (no empty or enemy pieces) when moving vertically
    CX = NX,
    DX = 0,
    DY is sign(NY - CY),
    check_line(Board, CX, CY, NX, NY, DX, DY, CurrentPlayer).
clear_path_jump(Board, CX, CY, NX, NY, CurrentPlayer) :- % Ensure the path is clear for jumps (no empty or enemy pieces) when moving horizontally
    CY = NY,
    DY = 0,
    DX is sign(NX - CX),
    check_line(Board, CX, CY, NX, NY, DX, DY, CurrentPlayer).

% check_line(+Board, +X, +Y, +NX, +NY, +DX, +DY, +CurrentPlayer)
% Check each position along the line for friendly pieces
check_line(Board, X, Y, NX, NY, DX, DY, CurrentPlayer) :-
    NextX is X + DX,
    NextY is Y + DY,
    check_line_continue(Board, NextX, NextY, NX, NY, DX, DY, CurrentPlayer).

% check_line_continue(+Board, +X, +Y, +NX, +NY, +DX, +DY, +CurrentPlayer)
% Ensure piece belongs to the current player and continue checking
check_line_continue(_, NX, NY, NX, NY, _, _, _) :- !.
check_line_continue(Board, X, Y, NX, NY, DX, DY, CurrentPlayer) :-
    piece_at(Board, X, Y, Piece),
    player_piece(CurrentPlayer, Piece),
    NextX is X + DX,
    NextY is Y + DY,
    check_line_continue(Board, NextX, NextY, NX, NY, DX, DY, CurrentPlayer).

% empty_or_opponent(+Board, +X, +Y, +CurrentPlayer)
% Check if a cell is empty or occupied by an opponent
empty_or_opponent(Board, X, Y, CurrentPlayer) :-
    piece_at(Board, X, Y, Piece),
    (Piece = e; opponent_piece(CurrentPlayer, Piece)).

% piece_at(+Board, +X, +Y, -Piece)
% Get the piece at a position
piece_at(Board, X, Y, Piece) :-
    nth1(Y, Board, Row),
    nth1(X, Row, Piece).

% piece_positions(+Board, +Player, -PositionValue)
% Calculate cumulative piece values for a player
piece_positions(Board, Player, PositionValue) :-
    findall(Value, (
        nth1(Y, Board, Row),
        nth1(X, Row, Piece),
        player_piece(Player, Piece),
        piece_value(Piece, Value)
    ), Values),
    sum_list(Values, PositionValue).

% replace(+Board, +X, +Y, +NewValue, -NewBoard)
% Replace a cell in the board
replace(Board, X, Y, NewValue, NewBoard) :-
    nth1(Y, Board, Row),
    replace_in_list(Row, X, NewValue, NewRow),
    replace_in_list(Board, Y, NewRow, NewBoard).

% replace_in_list(+List, +Index, +NewElem, -NewList)
% Replace an element in a list at a given index
replace_in_list([_|T], 1, NewElem, [NewElem|T]).
replace_in_list([H|T], Index, NewElem, [H|NewT]) :-
    Index > 1,
    NewIndex is Index - 1,
    replace_in_list(T, NewIndex, NewElem, NewT).

% adjacent(+CX, +CY, -NX, -NY)
% Check adjacency for simple moves (up, down, left, right, and diagonally)
adjacent(CX, CY, NX, NY) :-
    NX is CX, NY is CY + 1. % Down
adjacent(CX, CY, NX, NY) :-
    NX is CX, NY is CY - 1. % Up
adjacent(CX, CY, NX, NY) :-
    NX is CX + 1, NY is CY. % Right
adjacent(CX, CY, NX, NY) :-
    NX is CX - 1, NY is CY. % Left
adjacent(CX, CY, NX, NY) :-
    NX is CX + 1, NY is CY + 1. % Down-Right
adjacent(CX, CY, NX, NY) :-
    NX is CX - 1, NY is CY - 1. % Up-Left
adjacent(CX, CY, NX, NY) :-
    NX is CX + 1, NY is CY - 1. % Up-Right
adjacent(CX, CY, NX, NY) :-
    NX is CX - 1, NY is CY + 1. % Down-Left

% clear_path(+Board, +X1, +Y1, +X2, +Y2)
% Check if the path between two positions is clear
clear_path(_, X, Y, X, Y, _, _) :- !.
clear_path(Board, X1, Y1, X2, Y2) :-
    DX is sign(X2 - X1),
    DY is sign(Y2 - Y1),
    clear_path(Board, X1, Y1, X2, Y2, DX, DY).
clear_path(Board, X1, Y1, X2, Y2, DX, DY) :-
    NX is X1 + DX,
    NY is Y1 + DY,
    piece_at(Board, NX, NY, e),
    clear_path(Board, NX, NY, X2, Y2, DX, DY).

%
%                   Value
%

% simulate_move(+GameState, +Move, -Value)
% Simulate a move and evaluate its value
simulate_move(GameState, Move, Value) :-
    move(GameState, Move, NewGameState),
    value(NewGameState, Value).
simulate_move(GameState, Move, _) :-
    \+ move(GameState, Move, _),
    fail.
simulate_move(GameState, Move, _) :-
    move(GameState, Move, NewGameState),
    \+ value(NewGameState, _),
    fail.

% value(+GameState, -Value)
% Evaluate the value of a game state
% Strategy:
% 1. Extract the board and current player from the game state.
% 2. Calculate the distance to the goal for the current player using distance_to_goal/3.
% 3. Invert the distance to prioritize shorter distances (lower is better).
% 4. Check if the current player can capture an opponent's king using can_capture_king/3.
% 5. Calculate the final value by prioritizing king captures and then movement towards the goal.
value(GameState, Value) :-
    GameState = game_state(Board, Player, _, _),
    distance_to_goal(Board, Player, Distance),
    InvertedDistance is 10 / (Distance + 1), % +1 to avoid division by zero
    can_capture_king(Board, Player, CaptureBonus),
    Value is 100 * CaptureBonus + InvertedDistance.

% can_capture_king(+Board, +Player, -CaptureBonus)
% Predicate to check if the player can capture a king
can_capture_king(Board, Player, CaptureBonus) :-
    findall(1, (
        piece_at(Board, X, Y, OpponentKing),
        opponent_piece(Player, OpponentKing),  % Detects opponent's piece
        (OpponentKing = kw; OpponentKing = kb), % Ensure it's a king
        valid_moves(game_state(Board, Player, _, _), Moves), % Check if any move lands on the opponent king's position
        member(move(_, _, _, X, Y), Moves) % Any move that lands on (X, Y)
    ), CaptureMoves),
    length(CaptureMoves, Count),
    CaptureBonus is Count.

% distance_to_goal(+Board, +Player, -Distance)
% Calculate the total distance to the goal for all pieces of the player
distance_to_goal(Board, Player, Distance) :-
    findall(Dist, distance_to_goal_helper(Board, Player, Dist), Distances),
    calculate_total_distance(Player, Distances, Distance).

% distance_to_goal_helper(+Board, +Player, -Dist)
% Helper predicate to calculate the distance of each piece to the goal
distance_to_goal_helper(Board, white, Dist) :-
    length(Board, Size),
    between(1, Size, X),
    between(1, Size, Y),
    piece_at(Board, X, Y, Piece),
    (Piece = w ; Piece = kw),
    goal_position(white, GX, GY),
    chebyshev_distance(X, Y, GX, GY, Dist).
distance_to_goal_helper(Board, black, Dist) :-
    length(Board, Size),
    between(1, Size, X),
    between(1, Size, Y),
    piece_at(Board, X, Y, Piece),
    (Piece = b ; Piece = kb),
    goal_position(black, Board, GX, GY),
    chebyshev_distance(X, Y, GX, GY, Dist).

% calculate_total_distance(+Player, +Distances, -Distance)
% Calculate the total distance to the goal for all pieces of the player
calculate_total_distance(_, Distances, Distance) :-
    Distances \= [],
    sum_list(Distances, Distance).
calculate_total_distance(_, [], 0).

% goal_position(+Player, -GX, -GY)
goal_position(white, 1, 1). % Define the goal position for the white player
goal_position(black, Board, GX, GY) :- % Define the goal position for the black player
    length(Board, Size),
    GX is Size,
    GY is Size.

% chebyshev_distance(+X1, +Y1, +X2, +Y2, -Distance)
% Calculate Chebyshev Distance (Diagonal Distance)
chebyshev_distance(X1, Y1, X2, Y2, Distance) :-
    DX is abs(X2 - X1),
    DY is abs(Y2 - Y1),
    Distance is max(DX, DY).

% count_pieces(+Board, +Player, -Count)
% Count pieces on the board for a player
count_pieces(Board, Player, Count) :-
    findall(_, (member(Row, Board), member(Piece, Row), player_piece(Player, Piece)), Pieces),
    length(Pieces, Count).

% evaluate_moves(+GameState, +Moves, -BestMove)
% Evaluate all moves and select the best one
evaluate_moves(GameState, Moves, BestMove) :-
    evaluate_all_moves(GameState, Moves, WeightedMoves),
    select_best_move(Moves, WeightedMoves, BestMove).

% evaluate_all_moves(+GameState, +Moves, -WeightedMoves)
% Evaluate all moves and return them with their weights
evaluate_all_moves(GameState, Moves, WeightedMoves) :-
    findall(Value-Move, (
        member(Move, Moves),
        once(evaluate_single_move(GameState, Move, Value))
    ), WeightedMoves).

% evaluate_single_move(+GameState, +Move, -Value)
% Simulate a move and evaluate its value
evaluate_single_move(GameState, Move, Value) :-
    simulate_move(GameState, Move, Value).
evaluate_single_move(_, Move, _) :-
    fail.

% select_best_move(+Moves, +WeightedMoves, -BestMove)
select_best_move(Moves, [], BestMove) :- % Select a random move if no weighted moves are available
    random_member(BestMove, Moves).
select_best_move(_, WeightedMoves, BestMove) :- % Sort weighted moves and select the best one
    keysort(WeightedMoves, AscendingMoves),
    reverse(AscendingMoves, SortedMoves),
    extract_top_moves(SortedMoves, TopMoves),
    handle_top_moves(TopMoves, BestMove).
select_best_move([BestValue-BestMove|_], BestMove). % Extract the best move from the sorted weighted moves

% extract_top_moves(+WeightedMoves, -TopMoves)
% Extract moves with the highest value
extract_top_moves([], []).
extract_top_moves([BestValue-Move | Rest], TopMoves) :-
    include(same_value(BestValue), [BestValue-Move | Rest], TopMoves).

% same_value(+Value, +WeightedMove)
% Check if a move has the same value
same_value(Value, Value-_).

% handle_top_moves(+TopMoves, -BestMove)
% Select a random move from the top moves
handle_top_moves([], BestMove) :-
    fail.
handle_top_moves(TopMoves, BestMove) :-
    length(TopMoves, Len),
    random(0, Len, Index),
    nth0(Index, TopMoves, _-BestMove).

%
%                   Greedy
%

% choose_greedy_move(+GameState, -BestMove)
% Choose the best move using the greedy algorithm
choose_greedy_move(GameState, BestMove) :-
    valid_moves(GameState, Moves),
    handle_moves(GameState, Moves, BestMove).

% handle_moves(+GameState, +Moves, -BestMove)
% Handle the evaluation of moves to find the best one
handle_moves(_, [], _) :-
    fail.
handle_moves(GameState, Moves, BestMove) :-
    Moves \= [],
    evaluate_moves(GameState, Moves, BestMove).

% perform_move(+Board, +CX, +CY, +NX, +NY, -NewBoard)
% Perform the move on the board
perform_move(Board, CX, CY, NX, NY, NewBoard) :-
    piece_at(Board, CX, CY, Piece),
    replace(Board, CX, CY, e, TempBoard),
    replace(TempBoard, NX, NY, Piece, NewBoard).

% perform_transform(+Board, +X, +Y, -NewBoard)
% Perform the transform on the board
perform_transform(Board, X, Y, NewBoard) :-
    piece_at(Board, X, Y, Piece),
    transform_piece(Piece, NewPiece),
    replace(Board, X, Y, NewPiece, NewBoard).

% transform_piece(+Piece, -NewPiece)
% Transform a piece into its king version
transform_piece(w, kw).
transform_piece(b, kb).

%
%                           Minimax algorithm
%

% minimax(+GameState, +Depth, +MaximizingPlayer, +History, -BestMove, -BestValue)
minimax(GameState, 0, _, _, _, Value) :- % Base case: Evaluate the value of the game state when depth is 0
    value(GameState, Value), !.
minimax(GameState, _, _, _, _, Value) :- % Base case: Evaluate the value of the game state if the game is over
    game_over(GameState, _),
    value(GameState, Value), !.
minimax(GameState, _, _, History, _, Value) :- % Base case: Avoid cycles by assigning a large negative value if the state is repeated
    member(GameState, History),
    Value is -10000, !.
minimax(GameState, Depth, true, History, BestMove, BestValue) :- % Maximizing player
    findall(Move, valid_moves(GameState, Move), Moves),
    Moves \= [],
    Depth1 is Depth - 1,
    findall(Value-Move, (
        member(Move, Moves),
        move(GameState, Move, NewGameState),
        \+ member(NewGameState, History),
        minimax(NewGameState, Depth1, false, [NewGameState | History], _, TempValue),
        Value is TempValue
    ), ScoredMoves),
    max_member(BestValue-BestMove, ScoredMoves).
minimax(GameState, Depth, false, History, BestMove, BestValue) :- % Recursive case: Minimizing player
    findall(Move, valid_moves(GameState, Move), Moves),
    Moves \= [],
    Depth1 is Depth - 1,
    findall(Value-Move, (
        member(Move, Moves),
        move(GameState, Move, NewGameState),
        \+ member(NewGameState, History),
        minimax(NewGameState, Depth1, true, [NewGameState | History], _, TempValue),
        Value is TempValue
    ), ScoredMoves),
    min_member(BestValue-BestMove, ScoredMoves).

% choose_minimax_move_if_valid(+Moves, +GameState, -Move)
choose_minimax_move_if_valid([], _, Move):- % Choose a random move if no valid moves are available
    random_member(Move, []).
choose_minimax_move_if_valid(Moves, GameState, Move) :- % Choose the best move using the minimax algorithm if valid moves are available
    Moves \= [],
    choose_minimax_move(GameState, 3, Move).

% choose_minimax_move(+GameState, +Depth, -BestMove)
% Choose the best move using the minimax algorithm
choose_minimax_move(GameState, Depth, BestMove) :-
    minimax(GameState, Depth, _, History, BestMove, BestValue).

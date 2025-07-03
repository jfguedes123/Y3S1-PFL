%%% main.pl %%%

% game_cycle(+GameState)
% Main game cycle that handles game progression
game_cycle(GameState) :-
    game_over(GameState, Winner), !,
    display_game(GameState),
    congratulate_winner(Winner), nl, nl.
game_cycle(GameState) :-
    display_game(GameState),
    make_move(GameState, NewGameState),
    game_cycle(NewGameState).
game_cycle(GameState) :-
    write('Invalid move. Try again.'), nl,
    game_cycle(GameState).

% make_move(+GameState, -NewGameState)
% Handles making a move
make_move(game_state(Board, white, WhitePlayer, BlackPlayer), game_state(NewBoard, black, WhitePlayer, BlackPlayer)) :-
    format('~w (white), choose your move (step, jump, or transform): ', [WhitePlayer]),
    read(MoveType),
    handle_move(MoveType, game_state(Board, white, WhitePlayer, BlackPlayer), game_state(NewBoard, black, WhitePlayer, BlackPlayer), WhitePlayer).
make_move(game_state(Board, black, WhitePlayer, BlackPlayer), game_state(NewBoard, white, WhitePlayer, BlackPlayer)) :-
    format('~w (black), choose your move (step, jump, or transform): ', [BlackPlayer]),
    read(MoveType),
    handle_move(MoveType, game_state(Board, black, WhitePlayer, BlackPlayer), game_state(NewBoard, white, WhitePlayer, BlackPlayer), BlackPlayer).
make_move(_, _) :-
    fail.

% handle_move(+MoveType, +GameState, -NewGameState, +Player)
% Handle the move based on the move type
handle_move(step, game_state(Board, CurrentPlayer, WhitePlayer, BlackPlayer), game_state(NewBoard, NextPlayer, WhitePlayer, BlackPlayer), Player) :-
    format('~w, enter current position (X-Y): ', [Player]),
    read(CurrentX-CurrentY),
    format('~w, enter next position (X-Y): ', [Player]),
    read(NextX-NextY),
    valid_move(Board, CurrentPlayer, CurrentX, CurrentY, NextX, NextY, step),
    move(game_state(Board, CurrentPlayer, WhitePlayer, BlackPlayer), move(step, CurrentX, CurrentY, NextX, NextY), game_state(NewBoard, NextPlayer, WhitePlayer, BlackPlayer)).
handle_move(jump, game_state(Board, CurrentPlayer, WhitePlayer, BlackPlayer), game_state(NewBoard, NextPlayer, WhitePlayer, BlackPlayer), Player) :-
    format('~w, enter current position (X-Y): ', [Player]),
    read(CurrentX-CurrentY),
    format('~w, enter next position (X-Y): ', [Player]),
    read(NextX-NextY),
    valid_move(Board, CurrentPlayer, CurrentX, CurrentY, NextX, NextY, jump),
    move(game_state(Board, CurrentPlayer, WhitePlayer, BlackPlayer), move(jump, CurrentX, CurrentY, NextX, NextY), game_state(NewBoard, NextPlayer, WhitePlayer, BlackPlayer)).
handle_move(transform, game_state(Board, CurrentPlayer, WhitePlayer, BlackPlayer), game_state(NewBoard, NextPlayer, WhitePlayer, BlackPlayer), Player) :-
    format('~w, enter position to transform (X-Y): ', [Player]),
    read(X-Y),
    format('~w, enter position of the king (KX-KY): ', [Player]),
    read(KX-KY),
    valid_move(Board, CurrentPlayer, X, Y, KX, KY, transform),
    move(game_state(Board, CurrentPlayer, WhitePlayer, BlackPlayer), move(transform, X, Y, KX, KY), game_state(NewBoard, NextPlayer, WhitePlayer, BlackPlayer)).

% move(+GameState, +Move, -NewGameState)
% Executes a move and updates the game state
% Strategy:
% 1. Create a list with every valid move for the chosen move
% 2. Validate the move by checking if it is in the list of valid moves.
% 3. Perform the move on the board using perform_move/5 or perform_transform/3.
% 4. Switch the current player to the next player using switch_player/2.
move(game_state(Board, CurrentPlayer, WhitePlayer, BlackPlayer), move(step, CX, CY, NX, NY), game_state(NewBoard, NextPlayer, WhitePlayer, BlackPlayer)) :-
    valid_moves(game_state(Board, CurrentPlayer, WhitePlayer, BlackPlayer), Moves),
    member(move(step, CX, CY, NX, NY), Moves),
    perform_move(Board, CX, CY, NX, NY, NewBoard),
    switch_player(CurrentPlayer, NextPlayer).
move(game_state(Board, CurrentPlayer, WhitePlayer, BlackPlayer), move(jump, CX, CY, NX, NY), game_state(NewBoard, NextPlayer, WhitePlayer, BlackPlayer)) :-
    valid_moves(game_state(Board, CurrentPlayer, WhitePlayer, BlackPlayer), Moves),
    member(move(jump, CX, CY, NX, NY), Moves),
    perform_move(Board, CX, CY, NX, NY, NewBoard),
    switch_player(CurrentPlayer, NextPlayer).
move(game_state(Board, CurrentPlayer, WhitePlayer, BlackPlayer), move(transform, X, Y, KX, KY), game_state(NewBoard, NextPlayer, WhitePlayer, BlackPlayer)) :-
    valid_moves(game_state(Board, CurrentPlayer, WhitePlayer, BlackPlayer), Moves),
    member(move(transform, X, Y, KX, KY), Moves),
    perform_transform(Board, X, Y, NewBoard),
    switch_player(CurrentPlayer, NextPlayer).

% valid_moves(+GameState, -Moves)
% Generate all valid moves for the current player
valid_moves(game_state(Board, CurrentPlayer, _, _), Moves) :-
    findall(move(step, CX, CY, NX, NY), valid_move(Board, CurrentPlayer, CX, CY, NX, NY, step), StepMoves),
    findall(move(jump, CX, CY, NX, NY), valid_move(Board, CurrentPlayer, CX, CY, NX, NY, jump), JumpMoves),
    findall(move(transform, X, Y, KX, KY), valid_move(Board, CurrentPlayer, X, Y, KX, KY, transform), TransformMoves),
    append(StepMoves, JumpMoves, TempMoves),
    append(TempMoves, TransformMoves, Moves).

% valid_move(+Board, +CurrentPlayer, +CX, +CY, +NX, +NY, +MoveType)
% Checks if a step move is valid
valid_move(Board, CurrentPlayer, CX, CY, NX, NY, step) :-
    length(Board, Size),
    within_bounds(CX, CY, Size),
    within_bounds(NX, NY, Size),
    piece_at(Board, CX, CY, Piece),
    player_piece(CurrentPlayer, Piece),
    adjacent(CX, CY, NX, NY),
    empty_or_opponent(Board, NX, NY, CurrentPlayer).

% valid_move(+Board, +CurrentPlayer, +CX, +CY, +NX, +NY, +MoveType)
% Checks if a jump move is valid
valid_move(Board, CurrentPlayer, CX, CY, NX, NY, jump) :-
    length(Board, Size),
    within_bounds(CX, CY, Size),
    within_bounds(NX, NY, Size),
    piece_at(Board, CX, CY, Piece),
    player_piece(CurrentPlayer, Piece),
    (CX = NX; CY = NY), % Ensure jump is horizontal or vertical
    clear_path_jump(Board, CX, CY, NX, NY, CurrentPlayer), % Ensure no empty or enemy pieces in between
    empty_or_opponent(Board, NX, NY, CurrentPlayer).

% valid_move(+Board, +CurrentPlayer, +CX, +CY, +NX, +NY, +MoveType)
% Checks if a transform move is valid
valid_move(Board, CurrentPlayer, X, Y, KX, KY, transform) :-
    length(Board, Size),
    within_bounds(X, Y, Size),
    within_bounds(KX, KY, Size),
    piece_at(Board, X, Y, Piece),
    player_piece(CurrentPlayer, Piece),
    Piece \= kw, Piece \= kb, % Ensure the piece is not already a king
    piece_at(Board, KX, KY, King),
    player_piece(CurrentPlayer, King),
    (King = kw; King = kb),
    (adjacent(X, Y, KX, KY); clear_path(Board, X, Y, KX, KY)),
    count_kings(Board, CurrentPlayer, KingCount),
    KingCount < 2. % Ensure there are less than 2 kings

% game_over(+GameState, -Winner)
% Check if the game is over and determine the winner
% Strategy:
% 1. Count the total number of kings on the board using count_kings/2.
% 2. Count the number of kings for the white player using count_kings/3.
% 3. Count the number of kings for the black player using count_kings/3.
% 4. Determine if the game is over by evaluating the game_over_conditions/6 predicate with the board, current player, and king counts.
game_over(game_state(Board, CurrentPlayer, _, _), Winner) :-
    count_kings(Board, KingCount),
    count_kings(Board, white, WhiteKingCount),
    count_kings(Board, black, BlackKingCount),
    game_over_conditions(Board, CurrentPlayer, KingCount, WhiteKingCount, BlackKingCount, Winner).

% game_over_conditions(+Board, +CurrentPlayer, +KingCount, +WhiteKingCount, +BlackKingCount, -Winner)
game_over_conditions(Board, CurrentPlayer, _, _, _, Winner) :- % Check if a king is in the opposite corner
    king_in_opposite_corner(Board, CurrentPlayer),
    Winner = CurrentPlayer, !.
game_over_conditions(_, _, _, WhiteKingCount, BlackKingCount, Winner) :- % Check if white has one king and black has none
    WhiteKingCount =:= 1,
    BlackKingCount =:= 0,
    Winner = white, !.
game_over_conditions(_, _, _, WhiteKingCount, BlackKingCount, Winner) :- % Check if black has one king and white has none
    WhiteKingCount =:= 0,
    BlackKingCount =:= 1,
    Winner = black, !.
game_over_conditions(_, _, _, WhiteKingCount, BlackKingCount, Winner) :- % Check if white has two kings and black has none
    WhiteKingCount =:= 2,
    BlackKingCount =:= 0,
    Winner = white, !.
game_over_conditions(_, _, _, WhiteKingCount, BlackKingCount, Winner) :- % Check if black has two kings and white has none
    BlackKingCount =:= 2,
    WhiteKingCount =:= 0,
    Winner = black, !.
    
% king_in_opposite_corner(+Board, +Player)
% Check if a king is in the opposite corner
king_in_opposite_corner(Board, white) :-
    piece_at(Board, 1, 1, kw).
king_in_opposite_corner(Board, black) :-
    length(Board, Size),
    piece_at(Board, Size, Size, kb).

% choose_move(+GameState, +Level, -Move)
% Choose a move based on the difficulty level
% Strategy:
% 1. For easy difficulty (Level 1), select a random valid move.
% 2. For medium difficulty (Level 2), use a greedy algorithm to select the best move.
% 3. For hard difficulty (Level 3), use the minimax algorithm to select the best move.
choose_move(GameState, 1, Move) :- % Choose a move for the easy difficulty level
    valid_moves(GameState, Moves),
    random_member(Move, Moves).
choose_move(GameState, 2, BestMove) :- % Choose a move for the medium difficulty level
    valid_moves(GameState, Moves),
    write('Greedy difficulty selected.'), nl,
    choose_greedy_move(GameState, BestMove).
choose_move(GameState, 3, Move) :- % Choose a move for the hard difficulty level
    valid_moves(GameState, Moves),
    choose_minimax_move_if_valid(Moves, GameState, Move).

% game_cycle_computer(+GameState, +Level)
% Game cycle for Player vs Computer
game_cycle_computer(GameState, Level) :-
    game_over(GameState, Winner), !,
    display_game(GameState),
    congratulate_winner(Winner), nl, nl.
game_cycle_computer(game_state(Board, white, HumanPlayer, Computer), Level) :-
    display_game(game_state(Board, white, HumanPlayer, Computer)),
    make_move(game_state(Board, white, HumanPlayer, Computer), NewGameState),
    game_cycle_computer(NewGameState, Level).
game_cycle_computer(game_state(Board, black, HumanPlayer, Computer), Level) :-
    display_game(game_state(Board, black, HumanPlayer, Computer)),
    write('Computer (black) is thinking...'), nl, nl,
    choose_move(game_state(Board, black, HumanPlayer, Computer), Level, Move),
    move(game_state(Board, black, HumanPlayer, Computer), Move, NewGameState),
    game_cycle_computer(NewGameState, Level).
    
% game_cycle_computer_vs_computer(+GameState, +WhiteLevel, +BlackLevel)
% Game cycle for Computer vs Computer
game_cycle_computer_vs_computer(GameState, WhiteLevel, BlackLevel) :-
    game_over(GameState, Winner), !,
    display_game(GameState),
    congratulate_winner(Winner), nl, nl.
game_cycle_computer_vs_computer(game_state(Board, white, WhitePlayer, BlackPlayer), WhiteLevel, BlackLevel) :-
    display_game(game_state(Board, white, WhitePlayer, BlackPlayer)),
    write('Computer (white) is thinking...'), nl, nl,
    choose_move(game_state(Board, white, WhitePlayer, BlackPlayer), WhiteLevel, Move),
    move(game_state(Board, white, WhitePlayer, BlackPlayer), Move, NewGameState),
    game_cycle_computer_vs_computer(NewGameState, WhiteLevel, BlackLevel).
game_cycle_computer_vs_computer(game_state(Board, black, WhitePlayer, BlackPlayer), WhiteLevel, BlackLevel) :-
    display_game(game_state(Board, black, WhitePlayer, BlackPlayer)),
    write('Computer (black) is thinking...'), nl, nl,
    choose_move(game_state(Board, black, WhitePlayer, BlackPlayer), BlackLevel, Move),
    move(game_state(Board, black, WhitePlayer, BlackPlayer), Move, NewGameState),
    game_cycle_computer_vs_computer(NewGameState, WhiteLevel, BlackLevel).

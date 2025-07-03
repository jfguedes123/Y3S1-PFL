%%% board.pl - Board representation and display %%% 

% initial_state(+GameConfig, -GameState)
% Initializes the game state with the given configuration
initial_state([WhitePlayer, BlackPlayer, Size], game_state(Board, white, WhitePlayer, BlackPlayer)) :-
    initial_board(Size, Board).

% initial_board(+Size, -Board)
% Initialize the board based on the size
initial_board(8, [ [kb, b, b, b, e, e, e, e],
                   [b, b, b, b, e, e, e, e],
                   [b, b, e, e, e, e, e, e],
                   [b, b, e, e, e, e, e, e],
                   [e, e, e, e, e, e, w, w],
                   [e, e, e, e, e, e, w, w],
                   [e, e, e, e, w, w, w, w],
                   [e, e, e, e, w, w, w, kw] ]).


initial_board(81, [ [e, kw, e, e, e, e, e, e],
                   [e, e, e, e, b, e, e, e],
                   [e, e, e, e, e, e, e, e],
                   [e, e, e, w, e, e, e, e],
                   [e, kb, e, e, e, e, e, w],
                   [e, e, b, e, e, e, w, w],
                   [e, e, e, e, w, w, w, w],
                   [e, e, e, e, e, w, w, e] ]).

initial_board(82, [ [e, kw, kb, e, e, e, e, e],
                   [e, e, e, e, b, e, e, e],
                   [e, e, e, e, e, e, e, e],
                   [e, e, e, w, e, e, e, e],
                   [e, b, e, e, e, e, e, w],
                   [e, e, b, e, e, e, w, w],
                   [e, e, e, e, w, w, w, w],
                   [e, e, e, e, e, w, w, e] ]).


initial_board(12, [ [kb, b, b, b, e, e, e, e, e, e, e, e],
                    [b, b, b, b, e, e, e, e, e, e, e, e],
                    [b, b, e, e, e, e, e, e, e, e, e, e],
                    [b, b, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, w, w],
                    [e, e, e, e, e, e, e, e, e, e, w, w],
                    [e, e, e, e, e, e, e, e, w, w, w, w],
                    [e, e, e, e, e, e, e, e, w, w, w, kw] ]).

initial_board(121, [ [e, kw, b, b, e, e, e, e, e, e, e, e],
                    [b, b, b, b, e, e, e, e, e, e, e, e],
                    [b, e, e, e, e, e, e, e, e, e, e, e],
                    [b, b, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, w, w],
                    [e, e, e, e, e, e, e, e, e, e, w, w],
                    [e, e, e, e, e, e, e, e, w, w, e, w],
                    [e, e, e, e, e, e, e, e, e, e, kb, e] ]).

initial_board(122, [ [e, kw, b, b, e, e, e, e, e, e, e, e],
                    [b, kb, b, b, e, e, e, e, e, e, e, e],
                    [b, e, e, e, e, e, e, e, e, e, e, e],
                    [b, b, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, w, w],
                    [e, e, e, e, e, e, e, e, e, e, w, w],
                    [e, e, e, e, e, e, e, e, w, w, e, w],
                    [e, e, e, e, e, e, e, e, e, e, kb, e] ]).

initial_board(16, [ [kb, b, b, b, e, e, e, e, e, e, e, e, e, e, e, e],
                    [b, b, b, b, e, e, e, e, e, e, e, e, e, e, e, e],
                    [b, b, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [b, b, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, w, w],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, w, w],
                    [e, e, e, e, e, e, e, e, e, e, e, e, w, w, w, w],
                    [e, e, e, e, e, e, e, e, e, e, e, e, w, w, w, kw] ]).

initial_board(161, [ [e, kw, kb, b, e, e, e, e, e, e, e, e, e, e, e, e],
                    [b, b, e, b, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, b, e, e, b, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, b, e, e, e, e, e, e, e, w, e, e, e],
                    [e, e, e, e, e, e, e, w, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, w, e, e],
                    [e, e, e, e, e, e, w, e, e, e, e, e, e, e, w, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, w, w, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, w, kw] ]).

initial_board(162, [ [e, kb, kw, b, e, e, e, e, e, e, e, e, e, e, e, e],
                    [b, e, b, b, e, e, e, e, e, e, e, e, e, e, e, e],
                    [b, b, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [b, b, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, w, w],
                    [e, e, e, e, e, e, e, e, e, e, e, e, e, e, w, w],
                    [e, e, e, e, e, e, e, e, e, e, e, e, w, w, w, w],
                    [e, e, e, e, e, e, e, e, e, e, e, e, w, w, w, kw] ]).

% display_game(+GameState)
% Displays the game state for the player
% Strategy: 
% 1. Determine the board size using the length/2 predicate.
% 2. Display the column headers by calling display_column_headers/1 with the board size.
% 3. Print a separator line using display_separator/1 to visually separate the column headers from the board rows.
% 4. Display each row of the board by calling display_rows/2 with the board and its size.
% 5. Display the current player's information using the format/2 predicate, showing the player's name and color (white or black).
display_game(game_state(Board, white, WhitePlayer, _)) :-
    length(Board, Size),
    display_column_headers(Size), nl,
    write('   '), display_separator(Size),
    display_rows(Board, Size),
    format('Current player: ~w (white)', [WhitePlayer]), nl.
display_game(game_state(Board, black, _, BlackPlayer)) :-
    length(Board, Size),
    display_column_headers(Size), nl,
    write('   '), display_separator(Size),
    display_rows(Board, Size),
    format('Current player: ~w (black)', [BlackPlayer]), nl.

% display_column_headers(+Size)
% Displays the column headers for the board
display_column_headers(Size) :-
    write('     '),
    display_column_numbers(1, Size).

% display_column_numbers(+Current, +Size)
% Recursively displays the column numbers for the board
display_column_numbers(Current, Size) :-
    Current =< Size,
    format('~w   ', [Current]),
    Next is Current + 1,
    display_column_numbers(Next, Size).
display_column_numbers(Current, Size) :-
    Current > Size.

% display_separator(+Size)
% Recursively displays the separator line for the board
display_separator(Size) :-
    Size > 0,
    write('+---'),
    Next is Size - 1,
    display_separator(Next).
display_separator(0) :-
    write('+'), nl.

% display_rows(+Board, +Size)
% Displays all rows of the board
display_rows(Board, Size) :-
    display_rows(Board, Size, Size).
display_rows(_, 0, _) :- !.
display_rows(Board, RowNum, Size) :-
    nth1(RowNum, Board, Row),
    format('~|~t~w~2+ |', [RowNum]),
    display_cells(Row),
    write('   '), display_separator(Size),
    NextRowNum is RowNum - 1,
    display_rows(Board, NextRowNum, Size).

% display_cells(+Cells)
% Displays all cells 
display_cells([]) :- nl.
display_cells([Cell|Cells]) :-
    display_cell(Cell),
    write('|'),
    display_cells(Cells).

% display_cell(+Cell)
display_cell(kb) :- write(' B '). % Displays a black king cell
display_cell(kw) :- write(' W '). % Displays a white king cell
display_cell(b)  :- write(' b '). % Displays a black piece cell
display_cell(w)  :- write(' w '). % Displays a white piece cell
display_cell(e)  :- write('   '). % Displays an empty cell

% player_piece(+Player, +Piece)
% Verify piece ownership 
player_piece(white, w).
player_piece(white, kw).
player_piece(black, b).
player_piece(black, kb).

% piece_value(+Piece, -Value)
% Define the value of each piece
piece_value(w, 1).
piece_value(kw, 3).
piece_value(b, 1).
piece_value(kb, 3).

% opponent_piece(+Player, +Piece)
% Verify opponent piece
opponent_piece(white, b).
opponent_piece(white, kb).
opponent_piece(black, w).
opponent_piece(black, kw).

% opponent(+Player, -Opponent)
% Define the opponent
opponent(white, black).
opponent(black, white).

% king_piece(+Player, ?Piece)
% Define the king piece 
king_piece(white, kw).
king_piece(black, kb).

% switch_player(+CurrentPlayer, -NextPlayer)
% Switch the current player to the next player
switch_player(white, black).
switch_player(black, white).
:- consult(main).
:- consult(board).
:- consult(utils).
:- consult(test).

% play/0
% Entry point for the game
play :- menu.

% menu/0
% Displays the main menu and handles user choice
menu :-
    nl,
    write('======================================='), nl,
    write('||          Welcome to Replica!       ||'), nl,
    write('======================================='), nl,
    write('||  1. Player VS Player               ||'), nl,
    write('||  2. Player VS Computer             ||'), nl,
    write('||  3. Computer VS Computer           ||'), nl,
    write('||  4. Rules                          ||'), nl,
    write('||  5. Exit                           ||'), nl,
    write('======================================='), nl,
    write('Please enter your choice (1, 2, 3, 4, or 5): '), nl,
    read(Choice),
    handle_choice(Choice).

% handle_choice/1
% Handles the user's menu choice
handle_choice(1) :-
    write('Enter name for white player: '), nl,
    read(WhitePlayer),
    write('Enter name for black player: '), nl,
    read(BlackPlayer),
    write('Choose board size (8 for 8x8, 12 for 12x12, 16 for 16x16): '), nl,
    read(BoardSize),
    initial_state([WhitePlayer, BlackPlayer, BoardSize], GameState),
    game_cycle(GameState).

handle_choice(2) :-
    write('Enter name for human player: '), nl,
    read(PlayerName),
    write('Choose board size (8 for 8x8, 12 for 12x12, 16 for 16x16): '), nl,
    read(BoardSize),
    write('Choose computer level (1 for easy, 2 for medium, 3 for hard): '), nl,
    read(Level),
    initial_state([PlayerName, 'Computer', BoardSize], GameState),
    game_cycle_computer(GameState, Level).

handle_choice(3) :-
    write('Choose board size (8 for 8x8, 12 for 12x12, 16 for 16x16): '), nl,
    read(BoardSize),
    write('Choose computer level for white (1 for easy, 2 for medium, 3 for hard): '), nl,
    read(WhiteLevel),
    write('Choose computer level for black (1 for easy, 2 for medium, 3 for hard): '), nl,
    read(BlackLevel),
    initial_state(['Computer_1', 'Computer_2', BoardSize], GameState),
    game_cycle_computer_vs_computer(GameState, WhiteLevel, BlackLevel).

handle_choice(4) :-
    display_rules,
    menu.

handle_choice(5) :-
    write('Goodbye!'), nl.

handle_choice(_) :-
    write('Invalid choice. Please try again.'), nl,
    menu.

% display_rules/0
% Displays the game rules
display_rules :-
    nl,
    write('======================================='), nl,
    write('||               Rules                ||'), nl,
    write('======================================='), nl,
    write('|| Replica is a two player game played||'), nl,
    write('|| using a chessboard and 12 black    ||'), nl,
    write('|| and 12 white flippable checkers.   ||'), nl,
    write('|| Players setup the game by placing  ||'), nl,
    write('|| the checkers on the board in       ||'), nl,
    write('|| opposite corners (with a 2x2       ||'), nl,
    write('|| square in the corner, flanked by a ||'), nl,
    write('|| 2x2 square on each side). The      ||'), nl,
    write('|| pieces in the very corner start    ||'), nl,
    write('|| the game flipped over (indicating  ||'), nl,
    write('|| a king). Players make one move per ||'), nl,
    write('|| turn, starting with White.         ||'), nl,
    write('||                                    ||'), nl,
    write('|| On each turn, players either step, ||'), nl,
    write('|| jump, or transform. All moves      ||'), nl,
    write('|| (even captures) must go "forward"  ||'), nl,
    write('|| (1 of the 3 directions towards the ||'), nl,
    write('|| opponent corner). For steps and    ||'), nl,
    write('|| for jumps, if there is already a   ||'), nl,
    write('|| piece on the destination square,   ||'), nl,
    write('|| it is captured by replacement. For ||'), nl,
    write('|| a step, the piece moves forward    ||'), nl,
    write('|| one square. For a jump, the piece  ||'), nl,
    write('|| moves in a straight line forward   ||'), nl,
    write('|| over friendly pieces until it      ||'), nl,
    write('|| reaches a square not occupied by a ||'), nl,
    write('|| friendly piece. For a transform, a ||'), nl,
    write('|| friendly non-king piece in         ||'), nl,
    write('|| line-of-sight of a friendly king   ||'), nl,
    write('|| gets flipped (this creates another ||'), nl,
    write('|| friendly king). Only enemy pieces  ||'), nl,
    write('|| block line of sight.               ||'), nl,
    write('||                                    ||'), nl,
    write('|| The game is over if a player wins  ||'), nl,
    write('|| by getting any friendly king into  ||'), nl,
    write('|| the opposite corner, or wins by    ||'), nl,
    write('|| capturing any enemy king.          ||'), nl,
    write('======================================='), nl,
    nl.
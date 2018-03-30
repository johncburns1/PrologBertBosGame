%John Burns
%Annie Ibarra
%
%bertsgame

%blue/red
blue_square(blue).
red_square(red).

%click_state
%asks prolog whether an element is a click or no_click
click_state(click, click) :- !.
click_state(no_click, no_click) :- !.
click_state(_X, Y) :- Y.

%click_state for the permutations
click_state2(click).
click_state2(no_click).

%flip rule
%flips an element from red to blue
%or from blue to red
flip_state(red, blue) :- !.
flip_state(blue, red) :- !.
flip_state(_X, Y) :- Y.

%flips even when flip_state(red, red) or flip_state(blue, blue)
red() :- flip_state(red, blue).
blue() :- flip_state(blue, red).

%color_state
%asks prolog what color an element is
color_state(X, Y) :- blue_square(X), Y = X.
color_state(X, Y) :- red_square(X), Y = X.

%Building the board section
%length of list
%this tells prolog the length to make the list
list_length(N, List) :- length(List, N).

%generate grid
%generates a list of N lists all N elements in length
generate_grid(N, Board) :-
  list_length(N, Board),
  maplist(list_length(N), Board).

%fill_row
%fills a row with blue values
fill_row([]).
fill_row([H|T]) :-
  blue_square(H),
  fill_row(T).

%fill board
%uses fill_row to fill every row in the grid with blue values
fill_board([]).
fill_board([H|T]) :-
  fill_row(H),
  fill_board(T).

%generate_board
%generates the board itself (NxN with all blue values)
generate_board(N, Board):-
  generate_grid(N, Board),
  fill_board(Board).

%generate_click_perm
%generates a list of all possible click combinations for the first row
%use the cut to keep N from iterating below 1
generate_click_perm(0, []) :- !.
generate_click_perm(N, [H|ClickState]) :-
  click_state2(H),
  M is N-1, %iterate N using another variable
  generate_click_perm(M, ClickState).

%generate_clicks
%base case is 2 empty lists
%generates list from the colors in a given row
generate_clicks([], []) :- !.
generate_clicks([blue|T1], [H2|T2]) :-
  click_state(click, H2),
  generate_clicks(T1, T2).

generate_clicks([], []) :- !.
generate_clicks([red|T1], [H2|T2]) :-
  click_state(no_click, H2),
  generate_clicks(T1, T2).

%traverse recurses down the puzzle to the bottom
%It will then return all of the necessary clicks to complete the puzzle
%base case
traverse(Clicks, [H1], [Clicks]) :-
  flipRow(Clicks, H1, X),
  red_test(X).

%recursive case
traverse(FirstRowClicks, [H1, H2|T1], [Y, Clicks|ClicksList]) :-
  Y = FirstRowClicks,
  flipRow(FirstRowClicks, H1, Temp),
  flip_next_row(FirstRowClicks, H2, X),
  generate_clicks(Temp, Clicks),
  traverse(Clicks, [X|T1], [Clicks|ClicksList]).

%completion test
%asks whether or not all of the elements in the last row of the puzzle
%are red
red_test([]).
red_test([H|T]) :-
  red_square(H),
  red_test(T).

%flip_below flips the element below a flipped element
%base case is 2 empty lists
%passed a list of clicks from the row before it
flip_next_row([], [], []) :- !.
flip_next_row([click|Clicks], [H1|T1], [H2|T2]) :-
  flip_state(H1, H2),
  flip_next_row(Clicks, T1, T2).

%for no_click cases
flip_next_row([], [], []) :- !.
flip_next_row([no_click|Clicks], [H1|T1], [H2|T2]) :-
  color_state(H1, H2),
  flip_next_row(Clicks, T1, T2).

%flipRow
%flip the first 2 elements and then call the recursive flipRow2 to
%recurse through the rest of the list
flipRow([click|Clicks], [H1, H2|T1], [H3, H4|T2]) :-
  flip_state(H1, X),
  flip_state(H2, Y),
  flipRow2(Clicks, [X, Y|T1], [H3, H4|T2]).

%flip the first 2 elements and then call the recursive flipRow2 to
%recurse through the rest of the list
flipRow([no_click|Clicks], [H1, H2|T1], [H3, H4|T2]) :-
  color_state(H1, X),
  color_state(H2, Y),
  flipRow2(Clicks, [X, Y|T1], [H3, H4|T2]).

%flipping when element to be flipped is not an element that is not the 1st or last
flipRow2([click|Clicks], [H1, H2, H3|T1], [H4, H5, H6|T2]) :-
  flip_state(H1, H4),
  flip_state(H2, X),
  flip_state(H3, Y),
  flipRow2(Clicks, [X, Y|T1], [H5, H6|T2]).

%no_click when @ an element that is not the 1st or last
flipRow2([no_click|Clicks], [H1, H2, H3|T1], [H4, H5, H6|T2]) :-
  color_state(H1, H4),
  color_state(H2, X),
  color_state(H3, Y),
  flipRow2(Clicks, [X, Y|T1], [H5, H6|T2]).

%base case for flipRow2
%this accounts for the last 2 elements of a list
flipRow2([click], [H1, H2], [H3, H4]) :-
  flip_state(H1, H3),
  flip_state(H2, H4).

%no_click base case
%makes the values in the head of the resulting list the same color_state
flipRow2([no_click], [H1, H2], [H3, H4]) :-
  color_state(H1, H3),
  color_state(H2, H4).

%bertsgame
%this will tie everything together and solve the puzzle
bertsgame(N, Clicks) :-
  generate_board(N, Board),
  generate_click_perm(N, X),
  traverse(X, Board, Clicks).

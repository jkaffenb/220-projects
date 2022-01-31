% -*- mode: prolog -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: crypt.pl
%
% NAME: Jack Kaffenbarger
%
% DATE: 9/26/19
%
% DESCRIPTION: Prolog program supporting the discovery of solutions to
% crytarithmetic puzzles involving sums of digit lists.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% We did something like this in class...

is_digit(X) :-
    member(X, [0,1,2,3,4,5,6,7,8,9]).

all_digits([]).
all_digits([V|Rest]) :-
    is_digit(V), all_digits(Rest).

% note we wrote this as a warmup.  Here is an alterate solution
% for unique:
unique(List) :-
    list_to_ord_set(List, Set), length(List, L1), length(Set, L2),
    L1 = L2.

% You might need to write and use:
% Convert a Dlist with all bound variables to an integer

dlist_to_num([], 0).
dlist_to_num([H|T], Number) :-
    dlist_to_num(T, Numberother),
    length(T, Multiplier),
    Number is H * 10^Multiplier + Numberother.

% Converting a list of Digit lists to a list of integers is so easy,
% its provided here!
dlists_to_nums([],[]).
dlists_to_nums([Dlist|Rest], [Num|Others]) :-
    dlist_to_num(Dlist, Num),
    dlists_to_nums(Rest, Others).

% Helper function that gets the sum of a list
sum([],0).
sum([H|T],Result) :- sum(T,R1), Result is R1 + H.

% Both helper functions make sure that the first variable is not 0
nest_list_not_zero([]).
nest_list_not_zero([H|T]) :- first_not_zero(H), nest_list_not_zero(T).

first_not_zero([H|_]) :- H \= 0.

% Appends all of the letters into a set, makes, sure the first letter of each
% word is not a 0, and then checks to see if the two sides match. If not,
% the function backtracks.
solve_sum(ListList,Sum) :-
    append(ListList, Sum, Unique),
    flatten(Unique, Flatunique),
    list_to_ord_set(Flatunique, Set),
    all_digits(Set),
    unique(Set),
    nest_list_not_zero(ListList),
    first_not_zero(Sum),
    dlists_to_nums(ListList, Left_numbers_list),
    dlist_to_num(Sum, Right_sum),
    sum(Left_numbers_list, Left_sum),
    Left_sum = Right_sum.

% Helper functions that print the list in an easily readable form
print_list([]).
print_list([H|T]) :- write(H), print_list(T).

print_nest_list([]).
print_nest_list([A]) :- print_list(A), write(' '), write(=), write(' ').
print_nest_list([H|T]) :- print_list(H), write(' ' + ' '), print_nest_list(T), !.


write_solution(ListList, Sum) :-
    % Writes a solved puzzle to the screen in an easily readable form:
    % e.g. write_solution([[2,8,1,7],[0,3,6,8]],[0,3,1,8,5])
    % produces: 2817 + 0368 = 03185  (followed by a newline)
    print_nest_list(ListList),
    print_list(Sum).

solve_and_write(ListList,Sum) :-
    % Does just what it says.
    solve_sum(ListList,Sum),
    write_solution(ListList,Sum).


% Example usage:
% solve_and_write([[S,E,N,D],[M,O,R,E]],[M,O,N,E,Y])

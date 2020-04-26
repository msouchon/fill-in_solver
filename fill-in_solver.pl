%%
% Author: Marcus Souchon <marcus_s@live.com.au> (Solver)
%         Peter Schachte <schachte@unimelb.edu.au> (File IO)
% Purpose: Solve Fillin Puzzles
%
% Fillin Puzzles consist of a grid of squares most of which are empty into
% which characters can be inputted. A list of character strings are provided
% each of which must appear once in the list, vertically or horizontally
% (note: character strings of length 1 are not included in the list but are
% always permitted).
%
% The grid also consists of blank squares, represented by '#' which cannot be
% filled in. A properly constructed puzzle may have multiple solutions.
%
% This file solves Fillin Puzzles, given two files: A Puzzle file which
% contains '#' symbols for blanks and '_' for fillable slots, and rows
% seperated by newlines. Also a word file ,which contatins a list of
% character strings, each seperated by a newline of which appear in the
% solved puzzle. Both these files are plaintext.
%
% The provided started code by Peter Schachte provides the file io for this
% program, reading each plaintext file into a list of lists. Each list within
% this represents a line with each character being an element within it.
%
% The puzzle solver then accepts these processed input files and provides the
% puzzle solutions. The process for creating these solutions is as such:
%
% First the '_' characters are given associated logical varaibles, consistent
% horizontally and vertically.
%
% Slots are collected by extracting the lists of characters between '#'.
%
% Slots of a unique length are filled from the word list first, as they only
% have one solution.
%
% The remaining slots are filled in, in order of non logical variables in the
% slots descending. This means that slots that contain the fewest logical
% varaibles are filled first.
%
% The solved puzzle is then outputted to a file in a similiar form as its
% imput, only solved.

% Ensure neccesary libraries are loaded for the puzzle solver.
:- ensure_loaded(library(clpfd)).
:- ensure_loaded(library(pairs)).

%%
% Reads in a PuzzleFile and a WordListFile, formatted as plain text with rows
% or words seperated by newline characters. Using these it solves the puzzle
% and prints the solution in the same format. Requires the first two arguments
% to be instantiated.
main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

%%
% FILE IO
%%

%%
% Opens the file, reads the entire content of a file and saves each line
% to a list, then closes the file. Content is a list of these lines.
% Requires Filename to be instantiated.
read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

%%
% Reads the lines from a stream and saves each line to a list in Content.
% Requires Stream to be instantiated. 
read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).

%%
% Reads one line from the Stream to Line, if the line is at the end of the
% file Last is true, otherwise false. Requires Stream to be instantiated.
read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

%%
% Prints the puzzle to a file. Requires both arguments to be instantiated.
print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

%%
% Prints a row from a stream. Requires Stream to be instantiated.
print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

%%
% Prints a character from a stream, if it is a variable it is printed as '_',
% following the puzzle format. Requires Stream to be instantiated.
put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

%%
% Holds when the puzzle is valid, ie it is square.
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(same_length(Row), Rows).

%%
% SOLVER
%%

%%
% Holds when the words from WordList fill PuzzleRows to form FilledRows.
% PuzzleRows must be a list of lists containing '_' or '#' and WordList
% must be a list of words, such that a word is a list of characters.
% FilledRows is of the same format as PuzzleRows but with characters instead
% of '_'.
solve_puzzle(PuzzleRows, WordList, FilledRows) :-

	% Get slots from given puzzle rows
	get_slots(PuzzleRows, FilledRows, WordSlots),

	% Get unique slots, sort remaing slots by rairity ascending
	process_slots(WordSlots, UniqueSlots, RemainingSlots),

	% Map unique slots to words
	fill_slots(UniqueSlots, WordList),
	fill_slots(RemainingSlots, WordList).

%%
% Extracts slots from puzzle rows, while also building the same variables
% in both filled rows and word slots. Requires the first two arguments to
% be instantiated.
get_slots(PuzzleRows, FilledRows, WordSlots) :-

	% Create new logical variables with the same shape as the puzzle
	% with the same symbols.
	same_length(PuzzleRows, FilledRows),
	maplist(same_length, PuzzleRows, FilledRows),
	maplist(is_same, PuzzleRows, FilledRows),

	% Get columns
	transpose(FilledRows, FilledColumns),

	% Find slots on the rows, accounting for hash symbols
	maplist(split_list_hash, FilledRows, SlotRowsNested),
	remove_nesting(SlotRowsNested, SlotRows),
	
	% Find slots on the columns, accounting for hash symbols
	maplist(split_list_hash, FilledColumns, SlotColumnsNested),
	remove_nesting(SlotColumnsNested, SlotColumns),

	% Create list of all slots, remove 1 letter slots as they are
	% not given words
	append(SlotRows, SlotColumns, Slots),	
	exclude(length_flipped(1), Slots, WordSlots).

%%
% Seperates given Slots into Slots which have a unique length and the
% remaining slots. Unique slots are in order of descending length.
% Remaining slots are in order of rairity ascending than length descending.
% Requires 'Slots' to be instantiated.
process_slots(Slots, UniqueSlots, RemSlotsSorted) :-
	% Add a prefix for length to sort slots by decending length
	map_list_to_pairs(length, Slots, NSlots),
	sort(0, @>=, NSlots, NSlotsS),

	% Group the slots by length and remove prefix
	group_pairs_by_key(NSlotsS, NSlotsSG),
	pairs_values(NSlotsSG, SlotsSG),
	
	% Add a prefix for group length and extract unique and remaining slots
	map_list_to_pairs(length, SlotsSG, SlotsSNG),
	partition(prefix_is_1, SlotsSNG, UniqueSlotsNG, RemSlotsSNG),

	% Remove group prefix for unique slots and remove group nesting
	pairs_values(UniqueSlotsNG, UniqueSlotsG),
	remove_nesting(UniqueSlotsG, UniqueSlots),

	% Sort remaining slots by group size ascending
	sort(0, @=<, RemSlotsSNG, RemSlotsSNGS),
	
	% Remove group prefix and group nesting on remaining slots
	pairs_values(RemSlotsSNGS, RemSlotsSGS),
	remove_nesting(RemSlotsSGS, RemSlotsSorted).

% Counts the number of elements in a list which are not variables. Argument
% 'List' must be instantiated.
count_non_vars(List, Count) :-
	partition(nonvar, List, NonVarList, _),
	length(NonVarList, Count).

% Sorts a list of slots by the number of elements which are not variables in
% descending order. Agrument 'Slots' must be instantiated.
sort_slots_by_non_vars(Slots, Sorted) :-
	map_list_to_pairs(count_non_vars, Slots, NSlots),
	sort(0, @>=, NSlots, NSlotsS),
	pairs_values(NSlotsS, Sorted).

%%
% Given a list of slots and a list of words, fills those slots. At each point
% it tries to fill the slot with the most instantiated elements. Requires both
% arguments.
fill_slots([], _).
fill_slots(Slots, WordList) :-
	sort_slots_by_non_vars(Slots, [S|Ss]),
	find_slot(S, WordList, NewWordList),
	fill_slots(Ss, NewWordList).

%%
% Used by fill_slots to individually fill a slot, then remove the used word
% from the wordlist. Requires both 'Slot' and 'Wordlist' to be instantiated.
find_slot(Slot, WordList, NewWordList) :-
	member(Slot, WordList),
	select(Slot, WordList, NewWordList).

%%
% HELPER PREDICATES
%%

%%
% Checks two lists if they are they same, but considers the character '_'
% from the first list to be a variable. Requires at least the first list.
is_same([], []).
is_same([Symbol|L1], [Symbol|L2]) :-
	Symbol \== '_',
	is_same(L1, L2).
is_same(['_'|L1], [_|L2]) :-
	is_same(L1, L2).

%%
% Seperates a list of elements with interspaced '#' symbols to a list of lists
% each containing the elements between the '#' symbols. Requires the first
% argument to be instantiated. Begins by skipping starting hashes then getting
% all elements before the next hash and placing them into a list. This is
% then repeated to build the list of lists.
split_list_hash([], []).
split_list_hash([X|Xs], Yss) :-
	(   X == '#'
	->  split_list_hash(Xs, Yss)
	;   Yss = [[X|Ys]|YssRest],
	    split_list_hash_get_next(Xs, Ys, YssRest)
	).

%%
% A helper function for the above that gets all the elements before the next
% hash, then calls the main function to keep building the list of lists.
split_list_hash_get_next([], [], []).
split_list_hash_get_next([X|Xs], Ys, Yss) :-
	(   X == '#'
	->  Ys = [],
	    split_list_hash(Xs, Yss)
	;   Ys = [X|YsRest],
	    split_list_hash_get_next(Xs, YsRest, Yss)
	).

%%
% Removes nesting in a list of lists by joining internal lists together
% to form one list.
remove_nesting([], []).
remove_nesting([X|Xs], Ys) :-
	remove_nesting(Xs, A),
	append(X, A, Ys).

%%
% The standard length predicate with reversed inputs used when needed to
% be partially applied in higher order functions. Functions as length
% otherwise.
length_flipped(Num, Xs) :- length(Xs, Num).

%%
% Returns true if the prefix of the list is equal to 1.
prefix_is_1(N-_) :- N =:= 1.


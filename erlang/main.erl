-module(main).
-export([start/1]).

-import(generate_squares, [get_squares/1]).

-import(lists, [nth/2, seq/2]).
-import(math, [pow/2]).


% Another solution would be optimisation by rotation.
% For each Square in the grid place a single 0.
% Rotate these Squares so that there are the least amount of 0s.

% 0s can be viewed as obscuring the 1s,
% This means the grid can be interpreted in 3 dimensions.
% Where each 0 obscures an array of potential 0s and 1s that could take its place in a different configuration of squares.

% Rotations have cascading effects on others:
% The effects of these rotations appear to be highly chaotic to the grid, but they can be modelled as state transitions:

% When a square is rotated; if the cell in the square transitions 1 -> 1: then it doesn't change, 
% which means that its dependent squares (ones that overlap with it), neccessarily also do not change.

% When a square is rotated; if the cell in the square transitions 1 -> 0: then all the squares that have that cell as a corner and already contain a 0 can update themselves.
% 


% cd("C:/Users/Kier/Desktop/OEIS/A227133/Second Approach").

convertToBitlist(Permutation, GridSize) -> [Bit || <<Bit:1>> <= <<Permutation:GridSize>>].
convertAllToBitlists(Permutations, GridSize) -> [[Bit || <<Bit:1>> <= <<Permutation:GridSize>>] || Permutation <- Permutations].

start_clock() -> statistics(runtime).

stop_clock() ->
    {_, Time1} = statistics(runtime),
    io:format("It took ~p seconds.~n", [Time1 / 1000]).

start(GridLength) ->
	GridSize = GridLength * GridLength,
	Squares = convertAllToBitlists(generate_squares:get_squares(GridLength), GridSize),
	io:fwrite("Squares: ~p~n", [Squares]),

	%start_clock(),

	% {Popcount, Solution} = solve(Squares, GridSize, GridSize),
	% io:fwrite("F(~p) = ~p.~nSolution: ~p~n", [GridLength, Popcount, convertToBitlist(Solution, GridSize)]),
	
	SquareMap = construct_square_map(Squares, lists:duplicate(GridSize, 0)),
	io:fwrite("SquareMap: ~p~n", [SquareMap]).

	%stop_clock().


[3,3,3,3
,3,5,5,3
,3,5,5,3
,3,3,3,3]
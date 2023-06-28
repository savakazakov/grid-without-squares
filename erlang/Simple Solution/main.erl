-module(main).
-export([solve/1]).

-import(generate_squares, [get_squares/1]).


% OEIS\A227133 can be modelled as a search for an integer with the greatest popcount, subject to: it not containing any squares.
% A square can simply be defined by an integer whose binary is entirely 0 bits, except four 1 bits in the corners of the square.
% These squares can be bitwise ANDed with a grid to check if they are present.


%%-----------------------------%%
% Solution Generation Functions %
%%-----------------------------%%

% Generate all lexographically next permutations of a given popcount
% This initial popcount is input as the first Permutation: (1 bsl Popcount) - 1
gospers_hack(GridSize, Permutation, Squares) when (Permutation < (1 bsl GridSize)) -> 
	C = Permutation band (-1 * Permutation),
	R = Permutation + C,

	NextPermutation = (trunc(((R bxor Permutation) bsr 2) / C) bor R),

	% Check each of the pre-calculated squares
	case validGrid(NextPermutation, Squares) of
		true -> convertToBitlist(NextPermutation, GridSize);
		false -> gospers_hack(GridSize, NextPermutation, Squares)
	end;

% There were no permutations that satiated all squares:
gospers_hack(_, _, _) -> no_solution.


isSquare(Value, Square) -> (Value band Square) == Square.
validGrid(Value, [Square | []]) -> not isSquare(Value, Square);
validGrid(Value, [Square | OtherSquares]) ->
	case isSquare(Value, Square) of
		true -> false;
		_ -> validGrid(Value, OtherSquares)
	end.


%%-----------------%%
% Utility Functions %
%%-----------------%%

start_clock() -> statistics(runtime).
stop_clock() ->
    {_, Time1} = statistics(runtime),
    io:format("It took ~p seconds.~n", [Time1 / 1000]).


printGrid(Grid, GridLength) -> [io:fwrite("~p~n", [lists:sublist(Grid, N, GridLength)]) || N <- lists:seq(1, length(Grid), GridLength)].
convertToBitlist(Value, GridSize) -> [Bit || <<Bit:1>> <= <<Value:GridSize>>].


%%-------------------%%
% Interface Functions %
%%-------------------%%

solve(GridLength) ->
	GridSize = GridLength * GridLength,
	Squares = generate_squares:get_squares(GridLength),

	start_clock(),

	{Solution, Popcount} = solve(GridSize, GridSize, Squares),

	io:format("F(~p) = ~p.~n", [GridLength, Popcount]),
	printGrid(Solution, GridLength),

	stop_clock().

% Main solution loop
solve(GridSize, Popcount, Squares) ->
	case gospers_hack(GridSize, (1 bsl Popcount) - 1, Squares) of
		no_solution -> solve(GridSize, Popcount - 1, Squares);
		Solution -> {Solution, Popcount}
	end.
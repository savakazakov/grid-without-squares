-module(main).
-export([start/0, solve/1]).

-import(squares, [get_squares/1, get_heat_map/2, get_dependency_maps/2, apply_dependency_map/3, get_peaks_of_heatmap/1, clear_bit/2, convert_to_bitlist/2, convert_all_to_bitlist/2]).
-import(lists, [sum/1]).


% OEIS\A227133 Problem Description: 
% Given a square grid with side n consisting of n^2 cells (or points), 
% a(n) is the maximum number of points that can be painted so that no four of the painted ones form a square with sides parallel to the grid.
	

% Algorithm:
% Since there are only two possible tiles to consider, the grid can be viewed as a single binary number, of length GridLength * GridLength.
% The value that is being searched for is the equivalent to the popcount of this binary number.

% The Heatmap Collapse Algorithm (HCA) builds a grid of cells, where each cell is the number of times that cell is used as a corner to a possible square.
% This is referred to as a heatmap; since it displays the most used cells.
% The cells that are the most used are also the ones that should be set to 0; to make the most efficient use of the 0 tiles.

% After the initial heatmap is computed:
% The indicies of the cells with the greatest values are stored in a list.
% For each non-symmetrical cell the dependency_map of that cell is applied to the heatmap, creating an updated heatmap.
% This can be modelled as a recursive function, that branches for each option - since it is possible there are multiple cells that share the greatest value in the heatmap.
% The recursion ignores cells of value equal to or less than 0.
% The recursion stops if its depth + 1 is equal to, or less than, the best known depth; since it is impossible to create a grid with a better score.
% The recursion depth is equal to the popcount of the integer.


% This solution does not contain any optimisations.

%%-------------------%%
% Interface Functions %
%%-------------------%%

start() -> solve(4).

solve(GridLength) ->
	start_clock(),

	GridSize = GridLength * GridLength,
	Squares = squares:get_squares(GridLength), % A List of all possible squares as integers.
	SquaresAsBitLists = squares:convert_all_to_bitlist(Squares, GridSize), % Each element is a single bit.

	InitialGrid = trunc(math:pow(2, GridSize) - 1), % An integer with GridSize number of set bits.
	InitialHeatMap = squares:get_heat_map(SquaresAsBitLists, GridSize), % A list expressing how many squares each cell is part of.
	DependencyMaps = squares:get_dependency_maps(SquaresAsBitLists, GridSize), % A list of GridSize number of sublists, the specific squares that cell is part of.

	SearchFunction = 
		fun Search([{Grid, Heatmap} | Grids]) ->
			case is_valid_grid(Grid, Squares) of
				true -> Grid;
				false -> 
					% Generate all children:
					Peaks = squares:get_peaks_of_heatmap(Heatmap),
					ChildDependencyMaps = [{lists:nth(N, DependencyMaps), N} || {_, N} <- Peaks],
					ChildGridsAndHeatmaps = [{squares:clear_bit(Grid, GridSize - N + 1), squares:apply_dependency_map(Heatmap, DependencyMap, GridSize - N + 1)} || {DependencyMap, N} <- ChildDependencyMaps],
					
					% Recurse with children:
					Search(Grids ++ ChildGridsAndHeatmaps)
			end
		end,

	Solution = SearchFunction([{InitialGrid, InitialHeatMap}]), 

	io:fwrite("Solution Found.~nF(~p) = ~p.~n", [GridLength, popcount(Solution, GridSize)]),
	print_grid(convert_to_bitlist(Solution, GridSize), GridLength),

	stop_clock().


%%-----------------%%
% Utility Functions %
%%-----------------%%

start_clock() -> statistics(runtime).
stop_clock() ->
    {_, Time1} = statistics(runtime),
    io:format("It took ~p seconds.~n", [Time1 / 1000]).


print_grid(Grid, GridLength) -> [io:fwrite("~p~n", [lists:sublist(Grid, N, GridLength)]) || N <- lists:seq(1, length(Grid), GridLength)].
popcount(Value, GridSize) -> lists:sum(squares:convert_to_bitlist(Value, GridSize)).

is_square(Value, Square) -> (Value band Square) == Square.
is_valid_grid(Value, [Square | []]) -> not is_square(Value, Square);
is_valid_grid(Value, [Square | OtherSquares]) ->
	case is_square(Value, Square) of
		true -> false;
		_ -> is_valid_grid(Value, OtherSquares)
	end.
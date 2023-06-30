-module(main).
-export([solve/1]).

-import(generate_squares, [get_squares/1, get_heat_map/2, get_dependency_map/2, apply_dependency_map/2, convertToBitlist/2, convertAllToBitlist/2]).


% OEIS\A227133 Problem Description: 
% Given a square grid with side n consisting of n^2 cells (or points), 
% a(n) is the maximum number of points that can be painted so that no four of the painted ones form a square with sides parallel to the grid.
	

% Algorithm:
% Since there are only two possible tiles to consider, the grid can be viewed as a single binary number, of length GridLength * GridLength.

% The Heatmap Collapse Algorithm (HCA) builds a grid of cells, where each cell is the number of times that cell is used as a corner to a possible square.
% This is referred to as a heatmap; since it displays the most used cells.
% The cells that are the most used are also the ones that should be set to 0; to make the most efficient use of the 0 tiles.

% After the initial heatmap is computed:
% The indicies of the cells with the greatest values are stored in a list.
% For each non-symmetrical cell the dependency_map of that cell is applied to the heatmap, creating an updated heatmap.
% This can be modelled as a recursive function, that branches for each option - since it is possible there are multiple cells that share the greatest value in the heatmap.
% The recursion ignores cells of value equal to or less than 0.
% The recursion stops if its depth + 1 is equal to, or less than, the best known depth; since it is impossible to create a grid with a better score.



%%-------------------%%
% Interface Functions %
%%-------------------%%

solve(GridLength) ->
	GridSize = GridLength * GridLength,
	Squares = generate_squares:get_squares(GridLength),
	SquaresAsBitLists = generate_squares:convertAllToBitlist(Squares, GridSize),

	InitialHeatMap = generate_squares:get_heat_map(SquaresAsBitLists, GridSize),
	DependencyMap = generate_squares:get_dependency_map(SquaresAsBitLists, GridSize),

	% Due to the size & number of variables that are not modified during recursion an internal function is used for searching:
	% This internal function can still use the above Maps & Squares.

	SearchFunction = fun Search(Depth) ->
						case Depth of
							1 ->
								io:fwrite("HeatMap: ~p~n", [InitialHeatMap]),
								io:fwrite("DependencyMap: ~p~n", [DependencyMap]),
								[];
							_ -> ok	
						end
					end,

	Solution = SearchFunction(1).
	%printGrid(Solution, GridLength).


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

% start_clock() -> statistics(runtime).
% stop_clock() ->
%     {_, Time1} = statistics(runtime),
%     io:format("It took ~p seconds.~n", [Time1 / 1000]).


printGrid(Grid, GridLength) -> [io:fwrite("~p~n", [lists:sublist(Grid, N, GridLength)]) || N <- lists:seq(1, length(Grid), GridLength)].
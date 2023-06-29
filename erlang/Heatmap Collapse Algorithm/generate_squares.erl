-module(generate_squares).
-export([get_squares/1, get_heat_map/1, get_dependency_map/2]).

-import(lists, [nth/2, zip/2, seq/2, duplicate/2]).
-import(math, [pow/2]).



% Iterate across all the points in the grid; with a scale between 2 and the gridlength:
get_squares(GridLength) ->
	GridSize = GridLength * GridLength,
	[convertToBitlist(construct_Square(Index, Scale, GridLength), GridSize) || Index <- lists:seq(1, GridSize), Scale <- lists:seq(2, GridLength), validSquare(Index, Scale, GridLength)].


% The binary of this integer will be all 0s except for 4 ones in the corners of the square:
construct_Square(Index, Scale, GridLength) ->
	trunc(math:pow(2, Index - 1)) + 
	trunc(math:pow(2, Index - 1 + Scale - 1)) + 
	trunc(math:pow(2, Index - 1 + (GridLength * (Scale - 1)))) + 
	trunc(math:pow(2, Index - 1 + (GridLength * (Scale - 1)) + Scale - 1)).


get_heat_map(Squares) -> get_heat_map(Squares, []).
get_heat_map([Square |[]], Map) -> add_list(Square, Map);
get_heat_map([Square | Squares], Map) -> get_heat_map(Squares, add_list(Square, Map)).


% Get a single Integer for each cell in the grid, subtracting that integer from the heatmap will reduce all vertices of all squares that interact with that cell:
get_dependency_map(Squares, GridSize) -> [combine_squares(SharedSquares, GridSize) || SharedSquares <- [lists_with_set_N(Squares, N) || N <- lists:seq(1, GridSize)]].
combine_squares(Squares, GridSize) -> lists:foldl(fun bor_list/2, lists:duplicate(GridSize, 0), Squares).


%%-----------------%%
% Utility Functions %
%%-----------------%%

validSquare(Index, Scale, GridLength) -> (not rowCrossing(Index, Scale, GridLength)) and (squareWithinBounds(Index, Scale, GridLength)).
currentRow(Index, GridLength) -> ((Index - 1) + GridLength - ((Index - 1) rem GridLength)) / GridLength. % -1 since this requires 0-based indexing
rowCrossing(Current, Scale, GridLength) -> abs(currentRow(Current, GridLength) - currentRow(Current + Scale -1, GridLength)) > 0.
squareWithinBounds(Current, Scale, GridWidth) -> (Current + (Scale - 1) + (GridWidth * (Scale - 1)) =< GridWidth * GridWidth).
convertToBitlist(Value, GridSize) -> [Bit || <<Bit:1>> <= <<Value:GridSize>>].

bor_list(List1, List2) -> [X bor Y || {X, Y} <- lists:zip(List1, List2)].
add_list(List1, List2) -> [X + Y || {X, Y} <- lists:zip(List1, List2)].

lists_with_set_N(Lists, N) -> 
	Predicate = fun(List) -> 
					case lists:nth(N, List) == 1 of
						true -> true;
						_ -> false
					end
				end,
	lists:filter(Predicate, Lists).
-module(squares).
-export([get_squares/1, get_heat_map/2, get_dependency_maps/2, apply_dependency_map/3, get_peaks_of_heatmap/1, get_shared_squares/2, clear_bit/2, convert_to_bitlist/2, convert_all_to_bitlist/2]).

-import(lists, [nth/2, zip/2, seq/2, duplicate/2, max/1]).
-import(math, [pow/2]).


% A Square can be defined by a single integer.
% This integer will, when expressed as a binary, be consist of entirely cleared bits, 
% Except for 4 set bits in indices corresponding to the corners of the square.

% A heatmap refers to adding all of these squares together, hence creating a super-imposed grid.
% The values in each cell of the grid refer to the number of times that cell is a corner in a square.
% This can be used to determine the cells that are most used & hence would be most efficient to clear.

% These squares can also be combined together via bitwise OR.
% If all of the squares that contain a 1 in a cell are OR'd together a dependency_map is created,
% "Dependency" refers to the fact that all of squares in the map use that cell; there is one map per cell.
% If a cell in the grid is set to 0, the heatmap can be updated by simply subtracting that dependency_map from the heatmap.



%%---------------------%%
% Interfacing Functions %
%%---------------------%%

% Iterate across all the points in the grid; with a scale between 2 and the gridlength:
get_squares(GridLength) ->
	GridSize = GridLength * GridLength,
	[construct_Square(Index, Scale, GridLength) || Index <- lists:seq(1, GridSize), Scale <- lists:seq(2, GridLength), validSquare(Index, Scale, GridLength)].


% The binary of this integer will have 4 set bits, one in each corner of the square, top-left as Index:
construct_Square(Index, Scale, GridLength) ->
	trunc(math:pow(2, Index - 1)) + 
	trunc(math:pow(2, Index - 1 + Scale - 1)) + 
	trunc(math:pow(2, Index - 1 + (GridLength * (Scale - 1)))) + 
	trunc(math:pow(2, Index - 1 + (GridLength * (Scale - 1)) + Scale - 1)).


get_heat_map(Squares, GridSize) when is_integer(GridSize) -> get_heat_map(Squares, lists:duplicate(GridSize, 0));
get_heat_map([Square |[]], Map) -> elementwise_add_lists(Square, Map);
get_heat_map([Square | Squares], Map) -> get_heat_map(Squares, elementwise_add_lists(Square, Map)).


% Build these mythical, magical maps:
get_dependency_maps(Squares, GridSize) -> [combine_squares(SharedSquares, GridSize) || SharedSquares <- [lists_with_set_N(Squares, N) || N <- lists:seq(1, GridSize)]].

get_shared_squares(Squares, GridSize) -> [lists_with_set_N(Squares, N) || N <- lists:seq(1, GridSize)].


% Bitwise OR all of the squares together:
combine_squares(Squares, GridSize) -> lists:foldl(fun elementwise_bor_lists/2, lists:duplicate(GridSize, 0), Squares).
 
% Simply subtract the selected Dependency Map from the Heat Map and set the value at N to 0:
apply_dependency_map(HeatMap, DependencyMap, Index) ->
	Value = lists:nth(Index, HeatMap),
	Mask = lists:duplicate(length(HeatMap) - Index, 0) ++ [Value] ++ lists:duplicate(Index - 1, 0),
	elementwise_sub_lists(elementwise_sub_lists(HeatMap, DependencyMap), Mask).

% There are lots of ways to optimise this function
get_peaks_of_heatmap(HeatMap) -> [{Value, Index} || {Value, Index} <- lists:zip(HeatMap, lists:seq(1, length(HeatMap))), Value == lists:max(HeatMap)].


%%-----------------%%
% Utility Functions %
%%-----------------%%

% Functions for get_square:
validSquare(Index, Scale, GridLength) -> (not rowCrossing(Index, Scale, GridLength)) and (squareWithinBounds(Index, Scale, GridLength)).
squareWithinBounds(Current, Scale, GridWidth) -> (Current + (Scale - 1) + (GridWidth * (Scale - 1)) =< GridWidth * GridWidth).
rowCrossing(Current, Scale, GridLength) -> abs(currentRow(Current, GridLength) - currentRow(Current + Scale -1, GridLength)) > 0.
currentRow(Index, GridLength) -> ((Index - 1) + GridLength - ((Index - 1) rem GridLength)) / GridLength. % -1 since this requires 0-based indexing

clear_bit(Grid, N) -> Grid - trunc(math:pow(2, N - 1)).

convert_to_bitlist(Value, GridSize) -> [Bit || <<Bit:1>> <= <<Value:GridSize>>].
convert_all_to_bitlist(Values, GridSize) -> [[Bit || <<Bit:1>> <= <<Value:GridSize>>] || Value <- Values].

elementwise_bor_lists(List1, List2) -> [X bor Y || {X, Y} <- lists:zip(List1, List2)].
elementwise_sub_lists(List1, List2) -> [X - Y || {X, Y} <- lists:zip(List1, List2)].
elementwise_add_lists(List1, List2) -> [X + Y || {X, Y} <- lists:zip(List1, List2)].

% Filter Lists, return a sublist of lists that have their Nth element equal to 1: 
lists_with_set_N(Lists, N) -> 
	Predicate = fun(List) -> lists:nth(N, List) == 1 end,
	lists:filter(Predicate, Lists).
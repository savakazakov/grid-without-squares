-module(generate_squares).
-export([get_squares/1]).

-import(lists, [zip/2, seq/2]).
-import(math, [pow/2]).



% Iterate across all the points in the grid; with a scale between 2 and the gridlength:
get_squares(GridLength) ->
	GridSize = GridLength * GridLength,
	[construct_Square(Index, Scale, GridLength) || Index <- lists:seq(1, GridSize), Scale <- lists:seq(2, GridLength), validSquare(Index, Scale, GridLength)].


% The binary of this integer will be all 0s except for 4 ones in the corners of the square:
construct_Square(Index, Scale, GridLength) ->
	trunc(math:pow(2, Index - 1)) + 
	trunc(math:pow(2, Index - 1 + Scale - 1)) + 
	trunc(math:pow(2, Index - 1 + (GridLength * (Scale - 1)))) + 
	trunc(math:pow(2, Index - 1 + (GridLength * (Scale - 1)) + Scale - 1)).


%%-----------------%%
% Utility Functions %
%%-----------------%%

validSquare(Index, Scale, GridLength) -> (not rowCrossing(Index, Scale, GridLength)) and (squareWithinBounds(Index, Scale, GridLength)).
currentRow(Index, GridLength) -> ((Index - 1) + GridLength - ((Index - 1) rem GridLength)) / GridLength. % -1 since this requires 0-based indexing
rowCrossing(Current, Scale, GridLength) -> abs(currentRow(Current, GridLength) - currentRow(Current + Scale -1, GridLength)) > 0.
squareWithinBounds(Current, Scale, GridWidth) -> (Current + (Scale - 1) + (GridWidth * (Scale - 1)) =< GridWidth * GridWidth).
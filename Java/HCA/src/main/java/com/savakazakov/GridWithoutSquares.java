package com.savakazakov;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.HashMap;
import java.util.HashSet;

/**
 * A class that attempts to tackle the Grid without squares problem.
 * @Note For full explanation of the problem please refer to the README.md file.
 * @Note I'm assuming that the element of interest (EOI) is "O". I.e., no 4 "O"s can to form a square.
 * @Note An "O" corresponds to a boolean value of true.
 */
public class GridWithoutSquares
{
    public static final int size = 3;
    public static final int seqSize = 7;
    public static final int times = 10;
    public static int[][] symmetryIndices = genSymmetryIndices(size);
    public static final boolean printAll = false;
    public static final boolean measurePerformance = false;

    public static void main(String[] args)
    {
        // 1.	Generate cellToSquares

        Map<Integer, Set<Integer>> heatmap = genHeatmapLookup(size);

        // Testing heatmap lookup.
        for (Set<Integer> s : heatmap.values())
        {
            String str = "[";

            for (Integer i : s)
            {
                str += i + ", ";
            }

            str = str.substring(0, str.length() - 2);
            System.out.println(str += "]");
        }

        // 2.	Generate PrevKnownGrids - ( popcount, grid ) (This is an optimisation, do this later.)
        // 3.	Set the grid to all 1s.
        // 4.	Init recursion: (currentPopcount, currentGrid, currentCell)
        //      a.	If (depth > currBest) – depth is really popcount
        //          i.	Return;
        // 5.	If currGrid == one of the PrevKnownGrids -> return with answer from PrevKnownGrids.
        // 6.	Check if it is a valid solution.
        //      a.	Yes
        //          i.	Return ( popcount, grid )
        //      b.	No
        //          i.	From HeatMap – get cells with highest squareOverlapFactor and remove the ones that are symmetrically the same.
        //          ii.	Once you have that list of cells. Branch to each.
        // 7.	Compare results from the branches
        // 8.	If all branching is complete: return with the best result.





        /* if (measurePerformance)
        {
            List<boolean[]> solutions = null;
    
            // Time the method.
            long startTime = System.nanoTime();
    
            for (int i = 0; i < times; i++)
            {
                solutions = genSol(size);
            }
    
            solutions = genSol(size);
            printSolution(solutions.get(0));
            
            long endTime = System.nanoTime();
    
            long total = (endTime - startTime) / times;
    
            System.out.println("Testing: N = " + size + ", " + times + " times");
            System.out.println("Average performance: " + total / 1000000 + " ms, (" + total / 1000 + " us)");
        }
        else
        {
            gridWithoutSquaresSequence(seqSize);
        } */
    }

    /**
     * Print the sequence of solutions to the Grid Without Sqaures problem.
     * @param maxSize - The upper limit of the grid size to be printed.
     */
    public static void gridWithoutSquaresSequence(int maxSize)
    {
        /* int numOfEOI = 0;

        for (int i = 2; i <= maxSize; i++)
        {
            // Recalculate the indices for the new size.
            symmetryIndices = genSymmetryIndices(i);

            // Generate the solutions.
            List<boolean[]> solutions = genSol(i);


            boolean[] sol = solutions.get(0);

            // Reset the numOfEOI.
            numOfEOI = 0;

            // Get the number of EOI.
            for (int j = 0; j < sol.length; j++)
            {
                if (sol[j])
                    numOfEOI++;
            }

            // Print the ratio.
            System.out.println("Ratio: " + numOfEOI + " / " + i * i + " (" + (double) numOfEOI / (i * i) + ")");

            if (printAll)
            {
                // Print all solutions.
                for ( boolean[] solution : solutions)
                {
                    printSolution(solution);
                }
            }
            else
            {
                // Print the first solution.
                printSolution(sol);
            }

            // Print how many unique solutions there are.
            if (solutions.size() > 1)
                System.out.println(" + " + (solutions.size() - 1) + " more unique solutions");
        } */
    }

    /**
     * Generate a list of 1D flattened grids, which are solutions for the 
     * "Grid Without Squares" problem for a given size. This integrates the validation
     * of the solution in the generation process.
     * 
     * @param size - The size of the solutions. E.g. size = 5 would give solutions for a 5 x 5 grid.
     * @return - The list containing all valid unique solutions for the give size.
     * @see chkUnique(boolean[], boolean[]) for a definition of unique.
     */
    // public static List<boolean[]> genSol(int size)
    // {
    //     Map<Integer, List<Square>> cellToSquares = genCellToSquaresMap(size);
    //     List<boolean[]> sol = new ArrayList<>();
    //     MutableInteger maxElm = new MutableInteger(0);

    //     genSolTR(0, 0, maxElm, new boolean[size * size], sol, cellToSquares);

    //     return sol;
    // }

    /**
     * An almost Tail Recursive algorithm, invoked by the genSol method (Middle recursion in reality).
     * @param ctr - The current index in the 1D flattened accumulator grid.
     * @param curElm - The current number of EOIs.
     * @param maxElm - The current maximum number of EOIs for a complete solution.
     * @param cand - The accumulator candidate.
     * @param sol - The list of solutions with maxElm EOIs.
     * @param cellToSquares - ADT to hold the mapping between cells and the squares it belongs to.
     */
    public static void genSolTR(int ctr, int curElm, MutableInteger maxElm, boolean[] cand, List<boolean[]> sol, Map<Integer, List<Square>> cellToSquares)
    {
        // Optimisation: Check if it possible to get to the current maxElm.
        if (curElm + cand.length - ctr < maxElm.val)
            return;

        // Recursion end condiditons.
        if (ctr == cand.length)
        {
            if (curElm > maxElm.val)
            {
                sol.clear();
                sol.add(cand.clone());
                maxElm.val = curElm;
            }
            else if (curElm == maxElm.val)
            {
                if (sol.stream().allMatch(x -> chkUnique(symmetryIndices, x, cand)))
                    sol.add(cand.clone());
            }

            return;
        }
        
        // Check if it is possible to place the EOI in cell[ctr / size][ctr % size].
        if (cellToSquares.get(ctr).stream().allMatch(square -> square.verticesCount < 3))
        {
            // First, set the cell to the EOI.
            cand[ctr] = true;
            // Update all squares, which have this cell as a vertex.
            cellToSquares.get(ctr).forEach(square -> square.verticesCount++);
            // Move on to the next cell.
            genSolTR(++ctr, ++curElm, maxElm, cand, sol, cellToSquares);

            // Second, check if a solution would be optimal with this cell as not an EOI.
            // Reset to the state before the recursive call.
            curElm--;
            cand[--ctr] = false;
            // Update the squares.
            cellToSquares.get(ctr).forEach(square -> square.verticesCount--);
        }

        // If it isn't possible or we already attemped the to have an EOI in this cell,
        // try to set the cell as not an EOI.
        genSolTR(++ctr, curElm, maxElm, cand, sol, cellToSquares);
    }

    /**
     * TODO Rework the Javadoc.
     * Generate the ADT to hold the mapping between cells and the squares it belongs to.
     * I.e. for all cells of the grid build a list of squares it is a vertex of.
     * 
     * Cells are indexed from 0 to @param size * @param size - 1, 
     * from top left corner to bottom right.
     * Squares are indexed from 0 to s.
     * This formula is derived from the following series: 1 ^ 2 + 2 ^ 2 + 3 ^ 2 + ... + (n - 1) ^ 2,
     * because there are (n - 1) ^ 2 squares of size 2 in an n by n grid, (n - 2) ^ 2 squares of size 3
     * and so on.
     * 
     * The exact vertices are unimportant for this application, therefore, only their count is stored.
     * They can be retrieved at the end using the squareID. This massively simplifies the ADT and the Square class.
     * 
     * @param size - The size of the grid.
     * @return - A mapping between the cell ID and the list of square IDs.
     */
    public static Map<Integer, Set<Integer>> genHeatmapLookup(int size)
    {
        Map<Integer, Set<Integer>> heatmap = new HashMap<>();
        int totalCells = size * size;

        // Iterate trough the cells.
        for (int i = 0; i < totalCells; i++)
        {
            Set<Integer> curSet = new HashSet<>();
            heatmap.put(i, curSet);

            // Add the entire row, column and both diagonal this cell is a part of.
            for (int j = 0; j < size; j++)
            {
                // Row.
                curSet.add(i / size + j);

                // Column.
                curSet.add(i % size + j * size);

                // Add the diagonal perpendicular to the main diagonal.
                int numOfIdicesInMainDiag = size - i % size + i / size;
                // int numOfIdicesInAntiDiag = size - (size - 1 - i % size - i / size);
                int numOfIdicesInAntiDiag = 1 + i % size + i / size;

                int xStart = i / size, yStart = i % size;
                
                while (xStart != 0 || yStart != 0)
                {
                    xStart--;
                    yStart--;
                }

                for (int k = 0; k < numOfIdicesInMainDiag; k++)
                {
                    curSet.add(xStart + yStart * size);
                }


                int mainDiag = (i % size) + (size + 1) * j;

                if (mainDiag >= 0 && mainDiag < totalCells) {
                    int mainDiagRow = mainDiag / size;
                    int mainDiagCol = mainDiag % size;

                    if (mainDiagRow == mainDiagCol) {
                        curSet.add(mainDiag);
                    }
                }

                // // Add the diagonal perpendicular to the main diagonal and the diagonal perpendicular to the anti-diagonal.
                // int mainDiag = (i % size - i / size) + (size + 1) * j;

                // if (mainDiag >= 0 && mainDiag < totalCells)
                // {
                //     curSet.add(mainDiag);

                //     // The anti-diagonal is just a horizontal reflection of the main.
                //     // curSet.add((mainDiag / size) * size + size - 1 - mainDiag % size);

                //     // curSet.add((j + 1) * size - i & size - i / size - 1);
                //     // (j + 1) * size - (antiDiagonalStart + 1)
                //     // antiDiagonalStart + (size - 1) * (j + 1)
                // }
            }

            // Finally remove the cell itself.
            curSet.remove(i);
        }

        return heatmap;
    }

    public static int[][] genHeatmap(int size)
    {
        // n-1 for the outer layer where n is the size
        // and +2 two for each layer inwards
        // If the min index of the row or the column is x then this is the layer.
        // num of layers is ceil (size / 2)
        return null;
    }

    /**
     * Print the solution as on the console. Where "O" is the EOI.
     * I.e. no 4 elements are the vertices of a square.
     * @param solution - The 1D flattened grid that stores the solution.
     */
    public static void printSolution(boolean[] sol)
    {
        // Get the length of the 
        int size = (int) Math.sqrt(sol.length);

        // Initialise the strings to be used in the printing.
        String line = "+" + "-+".repeat(size);
        String middleLine = "";

        for (int i = 0; i < size; i++)
        {
            System.out.println(line);
            
            for (int j = 0; j < size; j++)
                middleLine += "|" + (sol[i * size + j] ? "O" : "X"); 

            System.out.println(middleLine + "|");
            middleLine = "";
        }

        System.out.println(line + "\n");
    }

    /**
     * Generate the symmetrical indices under the 8 symmetries of a square for a 1D flattened 
     * @param size - The size of the grid.
     * @return - A 2D array of integers, where the integer at each cell is the translated index
     * of the symmetry (first dimension) and the normal flattened index (second dimension).
     */
    public static int[][] genSymmetryIndices(int size)
    {
        int[][] symmetryIndices = new int[8][size * size];

        for (int i = 0; i < size * size; i++)
        {
            // Identical Symmetry (0 degrees rotation)
            symmetryIndices[0][i] = i;

            // 90 Degrees Rotation (Counterclockwise)
            symmetryIndices[1][i] = (size - 1 - i % size) * size + i / size;

            // 180 Degrees Rotation
            symmetryIndices[2][i] = size * size - i - 1;

            // 270 Degrees Rotation
            symmetryIndices[3][i] = (i % size) * size + size - 1 - i / size;

            // Horizontal Mirror Image (Reflection)
            symmetryIndices[4][i] = (i / size) * size + size - 1 - (i % size);

            // Vertical Mirror Image (Reflection)
            symmetryIndices[5][i] = (size - 1 - i / size) * size + i % size;

            // Main Diagonal Reflection
            symmetryIndices[6][i] = (i % size) * size + i / size;

            // Anti-Diagonal Reflection
            symmetryIndices[7][i] = (size - 1 - i % size) * size + size - 1 - i / size;

        }

        return symmetryIndices;
    }

    /**
     * Check if two 1D flattened grids are unique.
     * I.e. check if the first is not a symmetrically equivalent solution, given the 8 symmetries of a square.
     * @param symmetryIndices - The precalculated indices for the 8 symmetries.
     * @param sol - The proposed solution.
     * @param cand - The candidate.
     * @return - True if unique and false otherwise.
     */
    public static boolean chkUnique(int[][] symmetryIndices, boolean[] sol, boolean[] cand)
    {
        List<Boolean> symmetries = new ArrayList<>(List.of(false, false, false, false, false, false, false, false));

        for (int i = 0; i < sol.length; i++)
        {
            for (int j = 0; j < 8; j++)
            {
                if (sol[i] != cand[symmetryIndices[j][i]])
                    symmetries.set(j, true);
            }

            // If all symmetries are satisfied, exit immediately.
            if (!symmetries.contains(false))
                return true;
        }

        return false;
    }

    /**
     * Class wrap for a square in a 2D grid. This is needed because multiple cell (4 to be exact)
     * can be the vertices of a square and they should all point to the same square, hence the
     * need for an object.
     * @implNote The squareID is not strictly needed, however, it helps retrieve the vertices of 
     * square that have all 4 vertices set as the EOI. A MutableInteger could be
     * used instead.
     */
    public static class Square
    {
        public int squareID;
        public int verticesCount;

        public Square(int squareID, int verticesCount)
        {
            this.squareID = squareID;
            this.verticesCount = verticesCount;
        }
    }

    /**
     * Class wrap for an integer so that recursion could save its result through stack pops.
     */
    public static class MutableInteger
    {
        public MutableInteger(int val)
        {
            this.val = val;
        }

        public int val;
    }
}
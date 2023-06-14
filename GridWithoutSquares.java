import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Arrays;
import java.util.HashMap;

/**
 * A class that attempts to tackle the Grid without squares problem.
 * @Note For full explanation of the problem please refer to the README.md file.
 * @Note I'm assuming that the element of interest is "O". I.e., no 4 "O"s can to form a square.
 * @Note An "O" corresponds to a boolean value of true.
 * 
 * TODO: Make sure all methods are safe and I do parameter checking.
 */
public class GridWithoutSquares
{
    public static void main(String[] args)
    {
        // Map<Integer, List<Square>> squares = genCellToSquaresMap(3);

        // genSol(3);

        // boolean[][] first = new boolean[][]{ {true, true, true},
        //                                      {false, true, true},
        //                                      {true, true, false} };

        // boolean[] first = new boolean[]{false, true, true, true, true, true, true, true, true};
        // boolean[] second = new boolean[]{true, true, true, true, true, true, true, true, false};

        // System.out.println(chkUnique(first, second));

        // return;

        int size = 5;

        // Time the method.
        long startTime = System.nanoTime();
        genSol(size);
        genSol(size);
        genSol(size);
        genSol(size);
        genSol(size);
        genSol(size);
        genSol(size);
        genSol(size);
        genSol(size);
        List<boolean[]> solutions = genSol(size);
        long endTime = System.nanoTime();

        long total = ((endTime - startTime) / 10) / 1000000;

        // for (boolean[] s : solutions)
        // {
        //     printSolution(oneDimToTwoDim(s));
        // }
        
        System.out.println(solutions.size());
        System.out.println("Performance: " + total);
    }

    /**
     * TODO: Javadoc this.
     * @param oneDim
     * @param size
     * @return
     */
    public static boolean[][] oneDimToTwoDim(boolean[] oneDim)
    {
        int size = (int) Math.sqrt(oneDim.length);
        boolean[][] twoDim = new boolean[size][size];

        for (int i = 0; i < size; i++)
        {
            for (int j = 0; j < size; j++)
            {
                twoDim[i][j] = oneDim[i * size + j];
            }
        }

        return twoDim;
    }

    /**
     * Checks if the candidate is a valid solution.
     * This uses recursion with a subset of the candidate,
     * hence the extra parameters.
     * @param cand - The 2D array holding the candidate.
     * @param size - The length up to which the candidate should be checked. (Default = cand.length)
     * @param x - The starting position on the first axis. (Default = 0)
     * @param y - The starting position on the second axis. (Default = 0)
     * @return - True if candidate is square free and false otherwise.
     */
    public static boolean chkCand(boolean[][] cand, int size, int x, int y)
    {
        // The terminating condition.
        if (size == 1)
            return true;
        
        for (int i = 1; i < size; i++)
        {
            // Check only the squares with the (x, y) starting position (along the diagonal of the candidate).
            if (cand[x][y] && cand[x + i][y] && cand[x][y + i] && cand[x + i][y + i])
                return false;
            // Go recursively over every other possible starting position.
            if (!(chkCand(cand, size - i, x + i, y) && chkCand(cand, size - i, x, y + i) && chkCand(cand, size - i, x + i, y + i)))
                return false;
        }

        return true;
    }

    /**
     * Generate all possible permutations of all possible solutions
     * @param size - The size of the candidate 2D array.
     * @return - A 3D array of candidates.
     */
    public static boolean[][][] genCands(int size)
    {
        boolean[][][] cands = new boolean[(int) Math.pow(2, size * size)][size][size];
        int position = 0;

        for (int i = 0; i < Math.pow(2, size * size); i++)
        {
            cands[i] = new boolean[size][size];
            position = 0;

            for (int j = 0; j < size; j++)
            {
                for (int k = 0; k < size; k++)
                {
                    cands[i][j][k] = (i >> position & 1) == 1 ? true : false;
                    position++;
                }
            }
        }

        return cands;
    }

    /**
     * Generate all possible permutations with 
     * repetitions with a specific number of elements.
     * @param size - The size of the candidate 2D array.
     * @param numOfElm - The desired number of Elm.
     * @return - A 3D array of candidates.
     */
    public static List<boolean[][]> genCandsN(int size, int numOfElm)
    {
        List<boolean[][]> cands = new ArrayList<>();
        genCandsNTR(0, 0, numOfElm, new boolean[size][size], cands);

        return cands;
    }
    
    /**
     * An almost Tail Recursive algorithm, invoked of the genCands method.
     * @param ctr - The current index in the 2D array.
     * @param numOfElmCtr - The current number of elements.
     * @param numOfElm - The required number of elements.
     * @param cand - The accumulator candidate.
     * @param cands - The ADT to hold the candidates with numOfElm elements.
     */
    public static void genCandsNTR(int ctr, int numOfElmCtr, int numOfElm, boolean[][] cand, List<boolean[][]> cands)
    {
        // Check if this candidate has the required number of elements.
        if (numOfElmCtr == numOfElm)
        {
            cands.add(deepCopy(cand));
            return;
        }
        // Check if it is about to go out of bounds of the candidate.
        else if (ctr >= Math.pow(cand.length, 2))
            return;

        // The beauty of the recursion is the fact that it makes each cell of the array
        // first true and then false for the solution of the rest of the candidate.
        cand[ctr / cand.length][ctr % cand.length] = true;
        genCandsNTR(++ctr, ++numOfElmCtr, numOfElm, cand, cands);
        cand[(ctr - 1) / cand.length][(ctr - 1) % cand.length] = false;
        genCandsNTR(ctr, --numOfElmCtr, numOfElm, cand, cands);
    }

    /**
     * Attempt to integrate the validity of the solution with the generation process.
     * TODO: Finish this comment.
     */
    public static List<boolean[]> genSol(int size)
    {
        Map<Integer, List<Square>> cellToSquares = genCellToSquaresMap(size);
        List<boolean[]> sol = new ArrayList<>();
        MutableInteger maxElm = new MutableInteger(0);

        genSolTR(0, 0, maxElm, new boolean[size * size], sol, cellToSquares);

        return sol;
    }

    /**
     * TODO: Finish this.
     */
    public static void genSolTR(int ctr, int curElm, MutableInteger maxElm, boolean[] cand, List<boolean[]> sol, Map<Integer, List<Square>> cellToSquares)
    {
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
                if (sol.stream().allMatch(x -> chkUnique(x, cand)))
                    sol.add(cand.clone());
            }

            return;
        }
        
        // Check if it is possible to place the element of interest in cell[ctr / size][ctr % size]. 
        if (cellToSquares.get(ctr).stream().allMatch(square -> square.verticesCount < 3))
        {
            cand[ctr] = true;

            // Update the ADT. TODO better comment.
            cellToSquares.get(ctr).forEach(square -> square.verticesCount++);
            genSolTR(++ctr, ++curElm, maxElm, cand, sol, cellToSquares);

            // Reset to the state before the recursive call.
            curElm--;
            cand[--ctr] = false;
            // Update the ADT. TODO better comment.
            cellToSquares.get(ctr).forEach(square -> square.verticesCount--);
        }

        // Go ahead with 
        genSolTR(++ctr, curElm, maxElm, cand, sol, cellToSquares);
    }

    /**
     * Generate the ADT to hold the mapping between cells and the squares it belongs to.
     * I.e. for all cells of the grid build a list of squares it is a vertex of.
     * 
     * Cells are indexed from 0 to @param size * @param size - 1, 
     * from top left corner to bottom right.
     * Squares are indexed from 0 to @param size * (@param size − 1) * (2@param size − 1) / 6.
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
    public static Map<Integer, List<Square>> genCellToSquaresMap(int size)
    {
        Map<Integer, List<Square>> squares = new HashMap<>();
        int itr = 0;

        // It is more effecient to iterate through the squares, because
        // for each square we add all 4 cell IDs. (the vertices)
        for (int i = 2; i <= size; i++)
        {
            for (int j = 0; j <= size - i; j++)
            {
                for (int k = 0; k <= size - i; k++)
                {
                    // Create an instance of the current square.
                    Square curSquare = new Square(itr++, 0);

                    // Add the top left cell of the square.
                    if (!squares.containsKey(size * j + k))
                        squares.put(size * j + k, new ArrayList<>(List.of(curSquare)));
                    else
                        squares.get(size * j + k).add(curSquare);

                    // Add the top right cell of the square.
                    if (!squares.containsKey(size * j + k + i - 1))
                        squares.put(size * j + k + i - 1, new ArrayList<>(List.of(curSquare)));
                    else
                        squares.get(size * j + k + i - 1).add(curSquare);

                    // Add the bottom left cell of the square.
                    if (!squares.containsKey(size * (j + i - 1) + k))
                        squares.put(size * (j + i - 1) + k, new ArrayList<>(List.of(curSquare)));
                    else
                        squares.get(size * (j + i - 1) + k).add(curSquare);
                    
                    // Add the bottom right cell of the square.
                    if (!squares.containsKey(size * (j + i - 1) + k + i - 1))
                        squares.put(size * (j + i - 1) + k + i - 1, new ArrayList<>(List.of(curSquare)));
                    else
                        squares.get(size * (j + i - 1) + k + i - 1).add(curSquare);
                }
            }
        }

        return squares;
    }

    /**
     * Print the solution as on the console. Where "O" is the element of interest.
     * I.e. no 4 elements are the vertices of a square.
     * TODO: This should be rewritten to accept 1D flattened array.
     * @param solution - The 2D array that stores the solution.
     */
    public static void printSolution(boolean[][] sol)
    {
        // Initialise the strings to be used in the printing.
        String line = "+" + "-+".repeat(sol.length);
        String middleLine = "";

        for (int i = 0; i < sol.length; i++)
        {
            System.out.println(line);
            
            for (int j = 0; j < sol.length; j++)
                middleLine += "|" + (sol[i][j] ? "O" : "X"); 

            System.out.println(middleLine + "|");
            middleLine = "";
        }

        System.out.println(line + "\n");
    }

    /**
     * @Deprecated
     * Perform a deep copy of a 2D array.
     * This is required since Java only does shallow cloning.
     * @param original - The 2D array to be deep copied.
     * @return - The copy of the 2D array.
     */
    @Deprecated
    public static boolean[][] deepCopy(boolean[][] original)
    {
        if (original == null)
            return null;

        final boolean[][] result = new boolean[original.length][];

        for (int i = 0; i < original.length; i++)
            result[i] = Arrays.copyOf(original[i], original[i].length);

        return result;
    }

    /**
     * @Deprecated
     * Check if two 2D arrays are not identical.
     * @Note This assumes the 2D arrays are of the same dimensions.
     * @param sol - The proposed solution.
     * @param cand - The candidate.
     * @return - True if not identical and false otherwise.
     */
    @Deprecated
    public static boolean chkIdent(boolean[][] sol, boolean[][] cand)
    {        
        for (int i = 0; i < sol.length; i++)
        {
            for (int j = 0; j < sol.length; j++)
            {
                if (sol[i][j] != cand[i][j])
                    return true;
            }
        }

        return false;
    }

    /**
     * Method overload of chkIdent(boolean[][], boolean[][]).
     * Check if two 1D flattened arrays are not identical.
     * @Note This assumes the 1D arrays are of the same dimensions.
     * @param sol - The proposed solution.
     * @param cand - The candidate.
     * @return - True if not identical and false otherwise.
     */
    public static boolean chkIdent(boolean[] sol, boolean[] cand)
    {
        for (int i = 0; i < sol.length; i++)
        {
            if (sol[i] != cand[i])
                return true;
        }

        return false;
    }

    /**
     * @Deprecated
     * Check if two 2D arrays are not identical, i.e., if the second is flipped horizontally.
     * @Note This assumes the 2D arrays are of the same dimensions.
     * @param sol - The proposed solution.
     * @param cand - The candidate.
     * @return - True if not identical and false otherwise.
     */
    @Deprecated
    public static boolean chkIdentHor(boolean[][] sol, boolean[][] cand)
    {        
        for (int i = 0; i < sol.length; i++)
        {
            for (int j = 0; j < sol.length; j++)
            {
                if (sol[i][j] != cand[sol.length - i - 1][j])
                    return true;
            }
        }

        return false;
    }

    /**
     * Method overload of chkIdentHor(boolean[][], boolean[][]).
     * Check if two 1D flattened arrays not identical, i.e., if the second is flipped horizontally.
     * @Note This assumes the 1D arrays are of the same dimensions.
     * @param sol - The proposed solution.
     * @param cand - The candidate.
     * @return - True if not identical and false otherwise.
     */
    public static boolean chkIdentHor(boolean[] sol, boolean[] cand)
    {
        int size = (int) Math.sqrt(sol.length);

        for (int i = 0; i < sol.length; i++)
        {
            if (sol[i] != cand[size - 1 - (i % size) + (i / size) * size])
                return true;
        }

        return false;
    }

    /**
     * @Deprecated
     * Check if two 2D arrays are not identical if the second is flipped vertically.
     * @Note This assumes the 2D arrays are of the same dimensions.
     * @param sol - The proposed solution.
     * @param cand - The candidate.
     * @return - True if not identical and false otherwise.
     */
    @Deprecated
    public static boolean chkIdentVert(boolean[][] sol, boolean[][] cand)
    {        
        for (int i = 0; i < sol.length; i++)
        {
            for (int j = 0; j < sol.length; j++)
            {
                if (sol[i][j] != cand[i][sol.length - j - 1])
                    return true;
            }
        }

        return false;
    }

    /**
     * Method overload of chkIdentVert(boolean[][], boolean[][]).
     * Check if two 1D flattened arrays not identical, i.e., if the second is flipped vertically.
     * @Note This assumes the 1D arrays are of the same dimensions.
     * @param sol - The proposed solution.
     * @param cand - The candidate.
     * @return - True if not identical and false otherwise.
     */
    public static boolean chkIdentVert(boolean[] sol, boolean[] cand)
    {
        int size = (int) Math.sqrt(sol.length);

        for (int i = 0; i < sol.length; i++)
        {
            for (int j = 0; j < sol.length; j++)
            {
                if (sol[i] != cand[(size - 1 - i / size) * size + (i % size)])
                    return true;
            }
        }

        return false;
    }

    /**
     * @Deprecated
     * Check if two 2D arrays are not identical if the second is flipped along the diagonal.
     * @Note This assumes the 2D arrays are of the same dimensions.
     * @param sol - The proposed solution.
     * @param cand - The candidate.
     * @return - True if not identical and false otherwise.
     */
    @Deprecated
    public static boolean chkIdentDiag(boolean[][] sol, boolean[][] cand)
    {        
        for (int i = 0; i < sol.length; i++)
        {
            for (int j = 0; j < sol.length; j++)
            {
                if (sol[i][j] != cand[sol.length - i - 1][sol.length - j - 1])
                    return true;
            }
        }

        return false;
    }

    /**
     * Method overload of chkIdentDiag(boolean[][], boolean[][]).
     * Check if two 1D flattened arrays not identical, i.e., if the second flipped along the diagonal.
     * @Note This assumes the 1D arrays are of the same dimensions.
     * @param sol - The proposed solution.
     * @param cand - The candidate.
     * @return - True if not identical and false otherwise.
     */
    public static boolean chkIdentDiag(boolean[] sol, boolean[] cand)
    {        
        for (int i = 0; i < sol.length; i++)
        {
            if (sol[i] != cand[sol.length - i - 1])
                return true;
        }

        return false;
    }

    /**
     * @Deprecated
     * Check if two 2D arrays are unique.
     * This means the second is not a flipped or rotated version of the first.
     * @Note This assumes the 2D arrays are of the same dimensions.
     * @param sol - The proposed solution.
     * @param cand - The candidate.
     * @return - True if unique and false otherwise.
     */
    @Deprecated
    public static boolean chkUnique(boolean[][] sol, boolean[][] cand)
    {
        return (chkIdent(sol, cand) && chkIdentHor(sol, cand) && chkIdentVert(sol, cand) && chkIdentDiag(sol, cand));
    }

    /**
     * Check if two 1D arrays are unique.
     * This means the second is not a flipped or rotated version of the first.
     * @Note This assumes the 1D arrays are of the same dimensions.
     * @param sol - The proposed solution.
     * @param cand - The candidate.
     * @return - True if unique and false otherwise.
     */
    public static boolean chkUnique(boolean[] sol, boolean[] cand)
    {
        return (chkIdent(sol, cand) && chkIdentHor(sol, cand) && chkIdentVert(sol, cand) && chkIdentDiag(sol, cand));
    }

    /**
     * @Deprecated
     * @param cands - The list of 2D arrays (candidates), which contain
     * duplicate and invalid solutions.
     * @return - The list with unique and valid solutions.
     */
    @Deprecated
    public static List<boolean[][]> pruneCands(List<boolean[][]> cands)
    {
        List<boolean[][]> candsCopy = new ArrayList<>(cands);

        // Remove invalid solutions.
        for (boolean[][] c : cands)
        {
            if (!chkCand(c, c.length, 0, 0))
                candsCopy.remove(c);
        }

        if (candsCopy.isEmpty())
            return candsCopy;

        // Remove duplicate solutions.
        for (int i = 0; i < cands.size() - 1; i++)
        {
            for (int j = i + 1; j < cands.size(); j++)
            {
                if (!chkUnique(cands.get(i), cands.get(j)))
                    candsCopy.remove(cands.get(j));
            }
        }

        return candsCopy;
    }

    /**
     * Class wrap for a square in a 2D grid. This is needed because multiple cell (4 to be exact)
     * can be the vertices of a square and they should all point to the same square, hence the
     * need for an object.
     * @implNote The squareID is not strictly needed, however, it helps retrieve the vertices of 
     * square that have all 4 vertices set as the element of interest. A MutableInteger could be
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
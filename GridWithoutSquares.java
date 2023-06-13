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
        //                                      {true, true, true},
        //                                      {true, true, false} };

        // boolean[] second = new boolean[]{true, true, true, false, true, true, true, true, false};

        int size = 3;

        List<boolean[]> solutions = genSol(size);

        for (boolean[] s : solutions)
        {
            printSolution(oneDimToTwoDim(s, size));
        }

        // printSolution(oneDimToTwoDim(second, 3));
    }

    /**
     * TODO: Javadoc this.
     * @param oneDim
     * @param size
     * @return
     */
    public static boolean[][] oneDimToTwoDim(boolean[] oneDim, int size)
    {
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
        MaxElm maxElm = new MaxElm();
        maxElm.val = 0;

        genSolTR(0, 0, maxElm, new boolean[size * size], sol, cellToSquares);

        return sol;
    }

    /**
     * TODO: Finish this.
     */
    public static void genSolTR(int ctr, int curElm, MaxElm maxElm, boolean[] cand, List<boolean[]> sol, Map<Integer, List<Square>> cellToSquares)
    {
        // Recursion end condiditons.
        if (ctr >= cand.length)
        {
            if (curElm > maxElm.val)
            {
                sol.clear();
                sol.add(cand.clone());
                maxElm.val = curElm;
            }
            else if (curElm == maxElm.val)
                sol.add(cand.clone());

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
     * TODO: Finish this comment.
     * For all cells of the grid build a list of squares it is a vertex of.
     * TODO: explain the ecoding of the cells and the encoding of the squares.
     * explain how I don't care about the vertices itself and I don't need to store them.
     * Also I can get them at the end using the formula, it massively simplifies the ADT.
     * I only care if some square has all its verticies set to the element of interest.
     * TODO: Explain the formula to find all of the squares.
     * n * (n−1) * (2n−1) / 6 - the formula.
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
     * Print the solution as on the console. Where "O" is the symbol of interest or element.
     * I.e. no 4 elements are the vertices of a square.
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
            {
                middleLine += "|" + (sol[i][j] ? "O" : "X"); 
            }

            System.out.println(middleLine + "|");
            middleLine = "";
        }

        System.out.println(line);
    }

    /**
     * Perform a deep copy of a 2D array.
     * This is required since Java only does shallow cloning.
     * @param original - The 2D array to be deep copied.
     * @return - The copy of the 2D array.
     */
    public static boolean[][] deepCopy(boolean[][] original)
    {
        if (original == null)
            return null;

        final boolean[][] result = new boolean[original.length][];

        for (int i = 0; i < original.length; i++)
        {
            result[i] = Arrays.copyOf(original[i], original[i].length);
        }

        return result;
    }

    /**
     * Check if two 2D arrays are not identical.
     * @Note This assumes the 2D arrays are of the same dimensions.
     * @param sol - The proposed solution.
     * @param cand - The candidate.
     * @return - True if not identical and false otherwise.
     */
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
     * Check if two 2D arrays are not identical if the second is flipped horizontally.
     * @Note This assumes the 2D arrays are of the same dimensions.
     * @param sol - The proposed solution.
     * @param cand - The candidate.
     * @return - True if not identical and false otherwise.
     */
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
     * Check if two 2D arrays are not identical if the second is flipped vertically.
     * @Note This assumes the 2D arrays are of the same dimensions.
     * @param sol - The proposed solution.
     * @param cand - The candidate.
     * @return - True if not identical and false otherwise.
     */
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
     * Check if two 2D arrays are not identical if the second is flipped along the diagonal.
     * @Note This assumes the 2D arrays are of the same dimensions.
     * @param sol - The proposed solution.
     * @param cand - The candidate.
     * @return - True if not identical and false otherwise.
     */
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
     * Check if two 2D arrays are unique.
     * This means the second is not a flipped or rotated version of the first.
     * @Note This assumes the 2D arrays are of the same dimensions.
     * @param sol - The proposed solution.
     * @param cand - The candidate.
     * @return - True if unique and false otherwise.
     */
    public static boolean chkUnique(boolean[][] sol, boolean[][] cand)
    {
        return (chkIdent(sol, cand) && chkIdentHor(sol, cand) && chkIdentVert(sol, cand) && chkIdentDiag(sol, cand));
    }

    /**
     * TODO: Finish this.
     * @param cands
     * @return
     */
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
     * TODO: Javadoc this.
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

    public static class MaxElm
    {
        public int val;
    }
}
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

public class TestPrecalcUniquenessCheck
{
    public static final int size = 5;
    public static int[][] rotIndices = rotIndices(size);
    public static final int times = 10000000;

    public static void main(String[] args)
    {
        int size = 5;
        int[][] rotIdxs = rotIndices(size);
        
        for (int i = 0; i < 3; i++)
        {
            for (int j = 0; j < size * size; j++)
            {
                System.out.println("[" + i + "][" + j + "] = " + rotIdxs[i][j]);
            }
        }

        // Generate 5 random 5x5 grids.
        boolean[][] grids = new boolean[size][size * size];

        for (int i = 0; i < size; i++)
        {
            for (int j = 0; j < size * size; j++)
            {
                if (Math.random() < .5)
                    grids[i][j] = false;
                else
                    grids[i][j] = true;

                // System.out.println("[" + i + "][" + j + "] = " + rotIdxs[i][j]);
            }
        }

        // Check uniqueness "times" times for different methods.

        try
        {
            measurePerformance(times, grids, "chkUnique");
            measurePerformance(times, grids, "precalcChkUnique");
            measurePerformance(times, grids, "precalcChkUniqueEnhanced");
            measurePerformance(times, grids, "precalcChkUniqueFinal");

        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }

    /**
     * TODO: Finish this.
     * @param times
     * @param grids
     * @param methodName
     */
    public static void measurePerformance(int times, boolean[][] grids, String methodName) throws Exception
    {
        Class<?>[] argTypes = new Class[] { boolean[].class, boolean[].class };
        Class<?> c = Class.forName("TestPrecalcUniquenessCheck");
        Method method = c.getDeclaredMethod(methodName, argTypes);

        long startTime = System.nanoTime();
        
        for (int i = 0; i < times; i++)
        {
            for (int j = 0; j < size; j++)
            {
                method.invoke(null, grids[0], grids[j]);
            }
        }

        long totalNew = (System.nanoTime() - startTime);

        System.out.println("Testing " + methodName + ": N = " + size + ", " + times + " times");
        System.out.println("Performance: " + totalNew / 1000000 + " ms, (" + totalNew / 1000 + " us)");
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
     * Check if two 1D arrays are unique.
     * This means the second is not a flipped or rotated version of the first.
     * @Note This assumes the 1D arrays are of the same dimensions.
     * @param sol - The proposed solution.
     * @param cand - The candidate.
     * @return - True if unique and false otherwise.
     */
    public static boolean chkUnique(boolean[] sol, boolean[] cand)
    {
        return chkIdent(sol, cand) && chkIdentHor(sol, cand) && chkIdentVert(sol, cand) && chkIdentDiag(sol, cand);
    }

    /**
     * TODO: Finish this.
     * 0 i hor
     * 1 is vert
     * 2 is the diag
     * @param size
     * @return
     */
    public static int[][] rotIndices(int size)
    {
        // int flattenedSize = size * size;
        int[][] rotIndicesLookup = new int[3][size * size];

        for (int i = 0; i < size * size; i++)
        {
            rotIndicesLookup[0][i] = size - 1 - (i % size) + (i / size) * size;
            rotIndicesLookup[1][i] = (size - 1 - i / size) * size + (i % size);
            rotIndicesLookup[2][i] = size * size - i - 1;
        }

        return rotIndicesLookup;
    }

    /**
     * TODO: Finish this.
     * @param sol
     * @param cand
     * @return
     */
    public static boolean precalcChkUnique(boolean[] sol, boolean[] cand)
    {
        int[][] rotIndices = rotIndices((int) Math.sqrt(sol.length));

        boolean horTrue = false, vertTrue = false, diagTrue = false;

        for (int i = 0; i < sol.length; i++)
        {
            if (sol[i] != cand[rotIndices[0][i]])
                horTrue = true;

            if (sol[i] != cand[rotIndices[1][i]])
                vertTrue = true;

            if (sol[i] != cand[rotIndices[2][i]])
                diagTrue = true;

            if (horTrue && vertTrue && diagTrue)
                return true;
        }

        return false;
    }

    /**
     * TODO: Finish this.
     * TODO: There should be a slightly more optimal way to do this.
     * I.e. avoid the checks for an identity that has already returned true.
     * @param sol
     * @param cand
     * @return
     */
    public static boolean precalcChkUniqueEnhanced(boolean[] sol, boolean[] cand)
    {
        boolean horTrue = false, vertTrue = false, diagTrue = false;

        for (int i = 0; i < sol.length; i++)
        {
            if (!horTrue && sol[i] != cand[rotIndices[0][i]])
                horTrue = true;

            if (!vertTrue && sol[i] != cand[rotIndices[1][i]])
                vertTrue = true;

            if (!diagTrue && sol[i] != cand[rotIndices[2][i]])
                diagTrue = true;

            if (horTrue && vertTrue && diagTrue)
                return true;
        }

        return false;
    }

    public static boolean precalcChkUniqueFinal(boolean[] sol, boolean[] cand)
    {
        boolean horTrue = false, vertTrue = false, diagTrue = false;
        int[] indices = {0, 1, 2};
        int ctr = 3;

        for (int i = 0; i < sol.length; i++)
        {
            for (int j = 0; j < indices.length; j++)
            {
                if (indices[j] != -1 && sol[i] != cand[rotIndices[j][i]])
                {
                    indices[j] = -1;
                    ctr--;

                    if (ctr == 0)
                        return true;
                }
            }
        }

        return false;
    }
}

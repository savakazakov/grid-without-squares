public class TestPrecalcUniquenessCheck
{
    public static final int size = 5;
    public static int[][] rotIndices = rotIndices(size);

    public static void main(String[] args)
    {
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
            }
        }

        // Check uniqueness 1000000 times.
        int times = 1000000;

        long startTimeOld = System.nanoTime();
        
        for (int i = 0; i < times; i++)
        {
            for (int j = 0; j < size; j++)
            {
                chkUnique(grids[0], grids[j]);
            }
        }

        long endTimeOld = System.nanoTime();

        long totalOld = (endTimeOld - startTimeOld);

        System.out.println("Testing Old: N = " + size + ", " + times + " times");
        System.out.println("Performance: " + totalOld / 1000000 + " ms, (" + totalOld / 1000 + " us)");

        long startTimeNew = System.nanoTime();
        
        for (int i = 0; i < times; i++)
        {
            for (int j = 0; j < size; j++)
            {
                chkUniqueNew(grids[0], grids[j]);
            }
        }

        long endTimeNew = System.nanoTime();

        long totalNew = (endTimeNew - startTimeNew);

        System.out.println("Testing New: N = " + size + ", " + times + " times");
        System.out.println("Performance: " + totalNew / 1000000 + " ms, (" + totalNew / 1000 + " us)");

        // Compare times.
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
        int[][] rotIndicesLookup = new int[3][size * size];

        for (int i = 0; i < size * size; i++)
        {
            rotIndicesLookup[0][i] = size - 1 - (i % size) + (i / size) * size;
            rotIndicesLookup[1][i] = (size - 1 - i / size) * size + (i % size);
            rotIndicesLookup[2][i] = size * size - i - 1;
        }

        return rotIndicesLookup;
    }

    public static boolean chkUniqueNew(boolean[] sol, boolean[] cand)
    {
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
}
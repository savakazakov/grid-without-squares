package com.savakazakov;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;

import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.Blackhole;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

//...........................................................................

// Modes
/*
 * Mode.Throughput, as stated in its Javadoc, measures the raw throughput by
 * continuously calling the benchmark method in a time-bound iteration, and
 * counting how many times we executed the method.
 *
 * We are using the special annotation to select the units to measure in,
 * although you can use the default.
 */
// @BenchmarkMode(Mode.Throughput)

//...........................................................................

/*
 * Mode.AverageTime measures the average execution time, and it does it
 * in the way similar to Mode.Throughput.
 *
 * Some might say it is the reciprocal throughput, and it really is.
 * There are workloads where measuring times is more convenient though.
 */
// @BenchmarkMode(Mode.AverageTime)

//...........................................................................

/*
 * Mode.SampleTime samples the execution time. With this mode, we are
 * still running the method in a time-bound iteration, but instead of
 * measuring the total time, we measure the time spent in *some* of
 * the benchmark method calls.
 *
 * This allows us to infer the distributions, percentiles, etc.
 *
 * JMH also tries to auto-adjust sampling frequency: if the method
 * is long enough, you will end up capturing all the samples.
 */
// @BenchmarkMode(Mode.SampleTime)

//...........................................................................

/*
 * Mode.SingleShotTime measures the single method invocation time. As the Javadoc
 * suggests, we do only the single benchmark method invocation. The iteration
 * time is meaningless in this mode: as soon as benchmark method stops, the
 * iteration is over.
 *
 * This mode is useful to do cold startup tests, when you specifically
 * do not want to call the benchmark method continuously.
 */
// @BenchmarkMode(Mode.SingleShotTime)

//...........................................................................

/*
 * We can also ask for multiple benchmark modes at once. All the tests
 * above can be replaced with just a single test like this:
 */
// @BenchmarkMode({Mode.Throughput, Mode.AverageTime, Mode.SampleTime, Mode.SingleShotTime})

//...........................................................................

/*
 * We can also ask for multiple benchmark modes at once. All the tests
 * above can be replaced with just a single test like this:
 */
// @BenchmarkMode(Mode.All)

//...........................................................................

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
@Fork(value = 2, jvmArgs = {"-Xms2G", "-Xmx2G"}, warmup = 0)
// @Warmup(iterations = 2)
// @Measurement(iterations = 3)
public class TestPrecalcUniquenessCheck
{
    // Testing configurations:
    public static final int size = 5;
    public static int[][] rotIndices = rotIndices(size);
    public static final int numOfGrids = 10000;
    public static final int times = 1000;

    public static void main(String[] args)
    {
        // Generate 5 random 5x5 grids.
        boolean[][] grids = new boolean[numOfGrids][size * size];

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

        // Check uniqueness "times" times for different methods.
        try
        {
            measurePerformance(times, "runUniquenessTest", "chkUnique", grids);
            // measurePerformance(times, "differentMethodSignature", "Print this");
            
            measurePerformance(times, "runUniquenessTest", "precalcChkUnique", grids);
            measurePerformance(times, "runUniquenessTest", "precalcChkUniqueEnhanced", grids);
            measurePerformance(times, "runUniquenessTest", "precalcChkUniqueFinal", grids);
            // measurePerformance(1, "differentMethodSignature", "Print this!");

        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }

    /**
     * Testing framework for evaluating the performance of different method implementations. 
     * @param times - The number of times to run the test.
     * @param grids - 
     * @param methodName
     */
    public static void measurePerformance(int times, String methodName, Object... args) throws Exception
    {
        Class<?>[] argTypes = new Class[args.length];
        for (int i = 0; i < args.length; i++)
            argTypes[i] = args[i].getClass();

        Class<?> c = Class.forName("TestPrecalcUniquenessCheck");
        Method method = c.getDeclaredMethod(methodName, argTypes);

        long startTime = System.nanoTime();
        long timeElapsed = 0;
        
        for (int i = 0; i < times; i++)
            timeElapsed += (Long) method.invoke(null, args);

        long total = (System.nanoTime() - startTime);

        System.out.println("Testing " + methodName + ": N = " + size + ", " + times + " times, " + numOfGrids + " number of grids.");
        System.out.println("Performance: " + total / 1000000 + " ms, (" + total / 1000 + " us)");

        System.out.println("Testing timeElapsed " + methodName + ": N = " + size + ", " + times + " times, " + numOfGrids + " number of grids.");
        System.out.println("Performance: " + timeElapsed / 1000000 + " ms, (" + timeElapsed / 1000 + " us)");
    }

    public static long runUniquenessTest(String uniquenessMethodName, boolean[][] grids) throws Exception
    {
        Class<?> c = Class.forName("TestPrecalcUniquenessCheck");
        Method method = c.getDeclaredMethod(uniquenessMethodName, boolean[].class, boolean[].class);

        long startTime = System.nanoTime();

        for (int i = 0; i < numOfGrids; i++)
        {
            method.invoke(null, grids[0], grids[i]);
        }

        return System.nanoTime() - startTime;
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
     * Pre-calculate the indices for the horizontally, vertically and
     * diagonally symmetrical grids with the specified size.
     * @Note - 
     * @param size - The size of the grid.
     * @return - A 2D array, which holds 3 arrays of length size * size.
     * @Note - Index 0 is the horizontally reflected index.
     * @Note - Index 1 is the vertically reflected index.
     * @Note - Index 2 is the diagonally reflected index.
     */
    public static int[][] rotIndices(int size)
    {
        int[][] rotIndicesLookup = new int[8][size * size];

        for (int i = 0; i < size * size; i++)
        {
            rotIndicesLookup[0][i] = size - 1 - (i % size) + (i / size) * size;
            rotIndicesLookup[1][i] = (size - 1 - i / size) * size + (i % size);
            rotIndicesLookup[2][i] = size * size - i - 1;
        }

        return rotIndicesLookup;
    }

    /**
     * Pre-calculated version of chkUnique
     * @see chkUnique
     * @see rot
     * @param sol
     * @param cand
     * @return
     */
    public static boolean precalcChkUnique(boolean[] sol, boolean[] cand)
    {
        boolean identTrue = false, horTrue = false, vertTrue = false, diagTrue = false;

        for (int i = 0; i < sol.length; i++)
        {
            if (sol[i] != cand[i])
                identTrue = true;

            if (sol[i] != cand[rotIndices[0][i]])
                horTrue = true;

            if (sol[i] != cand[rotIndices[1][i]])
                vertTrue = true;

            if (sol[i] != cand[rotIndices[2][i]])
                diagTrue = true;

            if (identTrue && horTrue && vertTrue && diagTrue)
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
        boolean identTrue = false, horTrue = false, vertTrue = false, diagTrue = false;

        for (int i = 0; i < sol.length; i++)
        {
            if (!identTrue && sol[i] != cand[i])
                identTrue = true;

            if (!horTrue && sol[i] != cand[rotIndices[0][i]])
                horTrue = true;

            if (!vertTrue && sol[i] != cand[rotIndices[1][i]])
                vertTrue = true;

            if (!diagTrue && sol[i] != cand[rotIndices[2][i]])
                diagTrue = true;

            if (identTrue && horTrue && vertTrue && diagTrue)
                return true;
        }

        return false;
    }

    public static boolean precalcChkUniqueFinal(boolean[] sol, boolean[] cand)
    {
        boolean identTrue = false;
        int[] indices = {0, 1, 2};
        int ctr = 4;

        for (int i = 0; i < sol.length; i++)
        {
            for (int j = 0; j < indices.length; j++)
            {
                if (!identTrue && sol[i] != cand[i])
                {
                    identTrue = true;
                    ctr--;

                    if (ctr == 0)
                        return true;
                }

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

    public static void differentMethodSignature(String str)
    {
        System.out.println(str);
    }
}

package com.savakazakov;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;

import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.Blackhole;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 2)
@Measurement(iterations = 3)
@Threads(4)
@Fork(value = 2/* , jvmArgs = {"-Xms2G", "-Xmx2G"} */)
@State(Scope.Benchmark)
public class PrecalcUniquenessBenchmark
{
    // Testing configurations.
    public int size = 5;
    public int[][] rotIndices;
    public boolean[][] grids;
    public int numOfGrids = 1000000;

    @Setup(Level.Trial)
    public void up()
    {
        // Set the seed value.
        long seed = 12345;

        // Create a Random object with the specified seed
        Random random = new Random(seed);

        rotIndices = rotIndices(size);

        // Generate [numOfGrids] random [size] x [size] grids.
        grids = new boolean[numOfGrids][size * size];

        for (int i = 0; i < size; i++)
        {
            for (int j = 0; j < size * size; j++)
            {
                if (random.nextDouble() < .5)
                    grids[i][j] = false;
                else
                    grids[i][j] = true;
            }
        }
    }

    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    public int runChkUnique()
    {
        int count = 0;

        for (int i = 0; i < numOfGrids; i++)
        {
            if(chkUnique(grids[0], grids[i]))
                count++;
        }

        return count;
    }

    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    public int runPrecalcChkUnique()
    {
        int count = 0;
        
        for (int i = 0; i < numOfGrids; i++)
        {
            if(precalcChkUnique(grids[0], grids[i]))
                count++;
        }

        return count;
    }

    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    public int runPrecalcChkUniqueEnhanced()
    {
        int count = 0;
        
        for (int i = 0; i < numOfGrids; i++)
        {
            if(precalcChkUniqueEnhanced(grids[0], grids[i]))
                count++;
        }

        return count;
    }

    @Benchmark
    @BenchmarkMode(Mode.AverageTime)
    public int runPrecalcChkUniqueFinal()
    {
        int count = 0;
        
        for (int i = 0; i < numOfGrids; i++)
        {
            if(precalcChkUniqueFinal(grids[0], grids[i]))
                count++;
        }

        return count;
    }

    public static void main(String[] args) throws RunnerException
    {
        Options opt = new OptionsBuilder()
                .include(PrecalcUniquenessBenchmark.class.getSimpleName())
                .forks(1)
                .build();

        new Runner(opt).run();
    }

    public static boolean chkIdent(boolean[] sol, boolean[] cand)
    {
        for (int i = 0; i < sol.length; i++)
        {
            if (sol[i] != cand[i])
                return true;
        }

        return false;
    }

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

    public static boolean chkIdentDiag(boolean[] sol, boolean[] cand)
    {        
        for (int i = 0; i < sol.length; i++)
        {
            if (sol[i] != cand[sol.length - i - 1])
                return true;
        }

        return false;
    }

    public static boolean chkUnique(boolean[] sol, boolean[] cand)
    {
        return chkIdent(sol, cand) && chkIdentHor(sol, cand) && chkIdentVert(sol, cand) && chkIdentDiag(sol, cand);
    }

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

    public boolean precalcChkUnique(boolean[] sol, boolean[] cand)
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

    public boolean precalcChkUniqueEnhanced(boolean[] sol, boolean[] cand)
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

    public boolean precalcChkUniqueFinal(boolean[] sol, boolean[] cand)
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
}

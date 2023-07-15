/*
 * Reference: https://github.com/openjdk/jmh/blob/master/jmh-samples/src/main/java/org/openjdk/jmh/samples/JMHSample_01_HelloWorld.java
 * https://jenkov.com/tutorials/java-performance/jmh.html
 */

package com.savakazakov;

import java.util.concurrent.TimeUnit;

import org.openjdk.jmh.annotations.*;
// import org.openjdk.jmh.infra.Blackhole;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

/////////////////////////////////////////////////////////////////////////////

// MODES
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

/////////////////////////////////////////////////////////////////////////////

// STATES
/*
 * Most of the time, you need to maintain some state while the benchmark is
 * running. Since JMH is heavily used to build concurrent benchmarks, we
 * opted for an explicit notion of state-bearing objects.
 *
 * Below are two state objects. Their class names are not essential, it
 * matters they are marked with @State. These objects will be instantiated
 * on demand, and reused during the entire benchmark trial.
 *
 * The important property is that state is always instantiated by one of
 * those benchmark threads which will then have the access to that state.
 * That means you can initialize the fields as if you do that in worker
 * threads (ThreadLocals are yours, etc).
 */

/*
 * State Class Requirements
 * A JMH state class must obey the following rules:
 *      1. The class must be declared public
 *      2. If the class is a nested class, it must be declared static (e.g. public static class ...)
 *      3. The class must have a public no-arg constructor (no parameters to the constructor).
 * When these rules are obeyed you can annotate the class with the @State annotation to make JMH recognize it as a state class.
 */

// Thread - Each thread running the benchmark will create its own instance of the state object.
// @State(Scope.Thread)

// Group - Each thread group running the benchmark will create its own instance of the state object.
// @State(Scope.Thread)

// Benchmark - All threads running the benchmark share the same state object.
// @State(Scope.Benchmark)

//...........................................................................

/*
 * Benchmark methods can reference the states, and JMH will inject the
 * appropriate states while calling these methods. You can have no states at
 * all, or have only one state, or have multiple states referenced. This
 * makes building multi-threaded benchmark a breeze.
 *
 * Fortunately, in many cases you just need a single state object.
 * In that case, we can mark the benchmark instance itself to be
 * the @State. Then, we can reference its own fields as any
 * Java program does.
 */
// @State(Scope.Thread)
// public class [Benchmark_Class_Name] { ...

//...........................................................................

/*
 * Since @State objects are kept around during the lifetime of the
 * benchmark, it helps to have the methods which do state housekeeping.
 * These are usual fixture methods, you are probably familiar with them from
 * JUnit and TestNG.
 *
 * Fixture methods make sense only on @State objects, and JMH will fail to
 * compile the test otherwise.
 *
 * As with the State, fixture methods are only called by those benchmark
 * threads which are using the state. That means you can operate in the
 * thread-local context, and (not) use synchronization as if you are
 * executing in the context of benchmark thread.
 *
 * Note: fixture methods can also work with static fields, although the
 * semantics of these operations fall back out of State scope, and obey
 * usual Java rules (i.e. one static field per class).
 *
 * Ok, let's prepare our benchmark:
 */
// The following works if the benchmark class is the state itself.
// @Setup
// public void prepare()
// {
//     x = Math.PI;
// }

// @TearDown
// public void check()
// {
//     assert x > Math.PI : "Nothing changed?";
// }

//...........................................................................

// Normally the @Setup and @TearDown methods are methods of the @State class.

// Example with a nested state.
// @State(Scope.Thread)
// public static class MyState
// {
//     @Setup(Level.Trial)
//     public void doSetup()
//     {
//         sum = 0;
//         System.out.println("Do Setup");
//     }

//     @TearDown(Level.Trial)
//     public void doTearDown()
//     {
//         System.out.println("Do TearDown");
//     }

//     public int a = 1;
//     public int b = 2;
//     public int sum ;
// }

//...........................................................................

/*
 * Fixture methods have different levels to control when they should be run.
 * There are at least three Levels available to the user. The value you set instruct 
 * JMH about when the method should be called. The possible values are:
 *      1. Level.Trial - The method is called once for each time for each full run of the benchmark.
 *         A full run means a full "fork" including all warmup and benchmark iterations.
 *         (the sequence of iterations)
 *      2. Level.Iteration - The method is called once for each iteration of the benchmark.
 *      (the sequence of invocations)
 *      3. Level.Invocation - The method is called once for each call to the benchmark method.
 *      (WARNING: read the Javadoc before using)
 */

/////////////////////////////////////////////////////////////////////////////

/*
 * The downfall of many benchmarks is Dead-Code Elimination (DCE): compilers
 * are smart enough to deduce some computations are redundant and eliminate
 * them completely. If the eliminated part was our benchmarked code, we are
 * in trouble.
 *
 * Fortunately, JMH provides the essential infrastructure to fight this
 * where appropriate: returning the result of the computation will ask JMH
 * to deal with the result to limit dead-code elimination (returned results
 * are implicitly consumed by Blackholes).
 *
 * Should your benchmark require returning multiple results, you have to
 * consider two options (detailed below).
 *
 * NOTE: If you are only producing a single result, it is more readable to
 * use the implicit return. Do not make your benchmark
 * code less readable with explicit Blackholes!
 * 
 * Option A:
 *
 * Merge multiple results into one and return it.
 * This is OK when is computation is relatively heavyweight, and merging
 * the results does not offset the results much.
 */
// @Benchmark
// public double measureRight_1()
// {
//     return compute(x1) + compute(x2);
// }

/*
 * Option B:
 *
 * Use explicit Blackhole objects, and sink the values there.
 * (Background: Blackhole is just another @State object, bundled with JMH).
 */
// @Benchmark
// public void measureRight_2(Blackhole bh)
// {
//     bh.consume(compute(x1));
//     bh.consume(compute(x2));
// }

/////////////////////////////////////////////////////////////////////////////

/*
 * The flip side of dead-code elimination is constant-folding.
 *
 * If JVM realizes the result of the computation is the same no matter what,
 * it can cleverly optimize it. In our case, that means we can move the
 * computation outside of the internal JMH loop.
 *
 * This can be prevented by always reading the inputs from non-final
 * instance fields of @State objects, computing the result based on those
 * values, and follow the rules to prevent DCE.
 */

// IDEs will say "Oh, you can convert this field to local variable". Don't. Trust. Them.
// (While this is normally fine advice, it does not work in the context of measuring correctly.)
// private double x = Math.PI;

// IDEs will probably also say "Look, it could be final". Don't. Trust. Them. Either.
// (While this is normally fine advice, it does not work in the context of measuring correctly.)
// private final double wrongX = Math.PI;

// @Benchmark
// public double measureWrong_1()
// {
//     // This is wrong: the source is predictable, and computation is foldable.
//     return compute(Math.PI);
// }

// @Benchmark
// public double measureWrong_2()
// {
//     // This is wrong: the source is predictable, and computation is foldable.
//     return compute(wrongX);
// }

// @Benchmark
// public double measureRight()
// {
//     // This is correct: the source is not predictable.
//     return compute(x);
// }

/////////////////////////////////////////////////////////////////////////////

// MAIN
/*
 * public static void main(String[] args) throws RunnerException
 * {
 *      Options opt = new OptionsBuilder()
 *              .include([Name_Of_Benchmark_Class].class.getSimpleName())
 *              .forks(1)
 *              .build();
 *      new Runner(opt).run();
 * }
 */

/////////////////////////////////////////////////////////////////////////////

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
@Threads(12)
@Fork(value = 3/* , jvmArgs = {"-Xms2G", "-Xmx2G"} */)
@Warmup(iterations = 2, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
public class TemplateBenchmark
{
    public static void main(String[] args) throws RunnerException
    {
        Options opt = new OptionsBuilder()
                .include(PrecalcUniquenessBenchmark.class.getSimpleName())
                // .threads(4)
                .forks(1)
                // .jvmArgs("-ea")
                // .shouldFailOnError(false) // switch to "true" to fail the complete run.
                .build();

        new Runner(opt).run();
    }

    // @Benchmark
    // @BenchmarkMode(Mode.AverageTime)
    // @Fork(value = 2/* , jvmArgs = {"-Xms2G", "-Xmx2G"} */)
    public static void blank()
    {
        // Intentionally left empty.
    }
}

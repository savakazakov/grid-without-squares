package com.savakazakov;

import org.openjdk.jmh.annotations.Benchmark;

public class MyBenchmark
{
    public static void main(String[] args)
    {
        testMethod();
    }

    @Benchmark
    public static void testMethod()
    {
        // This is a demo/sample template for building your JMH benchmarks. Edit as needed.
        // Put your benchmark code here.
        int a = 1;
        int b = 2;
        int sum = a + b;
        System.out.println("YEAH BABY!");
    }

}

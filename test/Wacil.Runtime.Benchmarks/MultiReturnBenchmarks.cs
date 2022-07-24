namespace Wacil.Runtime.Benchmarks;

using System;
using BenchmarkDotNet.Attributes;

public class MultiReturnBenchmark {
    public int ReturningFourThingsViaAllOut(int argument0, out int return1, out int return2, out int return3) {
        return1 = argument0 + 1;
        return2 = argument0 + 2;
        return3 = argument0 + 3;
        return argument0 + 4;
    }
    
    [Benchmark]
    public int CallReturningFourThingsViaAllOut() {
        int return1;
        int return2;
        int return3;
        return ReturningFourThingsViaAllOut(1, out return1, out return2, out return3) + return1 + return2 + return3;
    }

    public int ReturningFourThingsViaStructOut(int argument0, out ValueTuple<int, int, int> remaining) {
        remaining.Item1 = argument0 + 1;
        remaining.Item2 = argument0 + 2;
        remaining.Item3 = argument0 + 3;
        return argument0 + 4;
    }

    [Benchmark]
    public int CallReturningFourThingsViaStructOut() {
        ValueTuple<int, int, int> remaining;
        return ReturningFourThingsViaStructOut(1, out remaining) + remaining.Item1 + remaining.Item2 + remaining.Item3;
    }

    public (int, int, int, int) ReturningFourThingsAsStruct(int arg) {
        return new(arg + 4, arg + 1, arg + 2, arg + 3);
    }

    [Benchmark]
    public int CallReturningFourThingsAsStruct() {
        var results = ReturningFourThingsAsStruct(1);
        return results.Item1 + results.Item2 + results.Item3 + results.Item4;
    }
}

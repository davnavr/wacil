namespace Wacil.Runtime.Benchmarks;

using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Reflection;
using BenchmarkDotNet.Attributes;

public class DelegateBenchmarks {
    public delegate int MyDelegate(int a, int b, int c);

    public record FuncRef(object Environment, MethodInfo Method);

    [Benchmark]
    [ArgumentsSource(nameof(Closures))]
    public int AlwaysConvertAndInvoke(MulticastDelegate clo) {
        MyDelegate converted = Unsafe.As<MyDelegate>(Delegate.CreateDelegate(typeof(MyDelegate), clo.Target, clo.Method));
        return converted(1, 2, 3);
    }

    [Benchmark]
    [ArgumentsSource(nameof(Closures))]
    public int TryConvertAndInvoke(MulticastDelegate clo) {
        MyDelegate converted = clo as MyDelegate;
        converted ??= Unsafe.As<MyDelegate>(Delegate.CreateDelegate(typeof(MyDelegate), clo.Target, clo.Method));
        return converted(1, 2, 3);
    }

    [Benchmark]
    [ArgumentsSource(nameof(Closures))]
    public int DynamicallyInvoke(MulticastDelegate clo) {
        return (int)clo.DynamicInvoke(new[] { (object)1, (object)2, (object)3 });
    }

    [Benchmark]
    [ArgumentsSource(nameof(FunctionReferences))]
    public int InvokeWithFuncRef(FuncRef pair) {
        MyDelegate converted = Unsafe.As<MyDelegate>(Delegate.CreateDelegate(typeof(MyDelegate), pair.Environment, pair.Method));
        return converted(1, 2, 3);
    }

    static int AddAllNumbers(int a, int b, int c) => a + b + c;

    public IEnumerable<MulticastDelegate> Closures() {
        var o = new Tuple<int>(5);

        Func<int, int, int, int> funcStaticCall = AddAllNumbers;
        Func<int, int, int, int> funcClosedOver = (int a, int b, int c) => o.Item1 + a + b + c;
        MyDelegate exactStaticCall = AddAllNumbers;
        MyDelegate exactClosedOver = (int a, int b, int c) => o.Item1 + a + b + c;

        return new MulticastDelegate[] {
            funcStaticCall,
            funcClosedOver,
            exactStaticCall,
            exactClosedOver,
        };
    }

    public IEnumerable<FuncRef> FunctionReferences() {
        var o = new Tuple<int>(5);
        MyDelegate exactClosedOver = (int a, int b, int c) => o.Item1 + a + b + c;
        MyDelegate exactStaticCall = AddAllNumbers;

        return new FuncRef[] {
            new FuncRef(exactClosedOver.Target, exactClosedOver.Method),
            new FuncRef(null, exactStaticCall.Method),
        };
    }
}

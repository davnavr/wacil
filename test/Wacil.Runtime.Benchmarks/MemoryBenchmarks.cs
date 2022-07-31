namespace Wacil.Runtime.Benchmarks {
    using BenchmarkDotNet.Attributes;
    using Wacil.Runtime;

    public class MemoryBenchmarks {
        private readonly int index;
        private readonly int data;

        private readonly ArrayMemory array = new ArrayMemory(new Limits(1));
    
        private readonly SegmentedMemory segmented = new SegmentedMemory(new Limits(1));
    
        private readonly UnmanagedMemory unmanaged = new UnmanagedMemory(new Limits(1));

        public MemoryBenchmarks() {
            var rng = new System.Random(42);
            data = rng.Next();
            index = (int)rng.Next(0, MemoryHelpers.PageSize / 4) * 4;
        }

        [Benchmark]
        public int ArrayReadInt32() => MemoryHelpers.ReadInt32<ArrayMemory>(index, array, 0, 2);

        [Benchmark]
        public void ArrayWriteInt32() => MemoryHelpers.Write<ArrayMemory>(index, data, array, 0, 2);

        [Benchmark]
        public int SegmentedReadInt32() => MemoryHelpers.ReadInt32<SegmentedMemory>(index, segmented, 0, 2);

        [Benchmark]
        public void SegmentedWriteInt32() => MemoryHelpers.Write<SegmentedMemory>(index, data, segmented, 0, 2);

        [Benchmark]
        public int UnmanagedReadInt32() => MemoryHelpers.ReadInt32<UnmanagedMemory>(index, unmanaged, 0, 2);

        [Benchmark]
        public void UnmanagedWriteInt32() => MemoryHelpers.Write<UnmanagedMemory>(index, data, unmanaged, 0, 2);
    }
}

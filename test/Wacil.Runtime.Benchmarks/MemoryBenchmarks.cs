namespace Wacil.Runtime.Benchmarks {
    using BenchmarkDotNet.Attributes;
    using Wacil.Runtime;

    public class MemoryBenchmarks {
        private readonly uint address;
        private readonly int data;

        private readonly Memory memory = new(1);

        public MemoryBenchmarks() {
            var rng = new System.Random(42);
            data = rng.Next();
            address = (uint)rng.Next(0, Memory.PageSize / 4) * 4;
        }

        [Benchmark]
        public int ReadInt32() => Memory.ReadInt32(address, memory, 0, 2);

        [Benchmark]
        public void WriteInt32() => Memory.WriteInt32(data, address, memory, 0, 2);
    }
}

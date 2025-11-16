namespace Implementations.CSharp;

internal static class Generator
{
    public static int Run()
    {
        Directory.CreateDirectory(Cid.CidsDirectory);
        var examples = Directory.GetFiles(Cid.ExamplesDirectory).Where(File.Exists).OrderBy(path => path).ToList();
        foreach (var example in examples)
        {
            var content = File.ReadAllBytes(example);
            var cid = Cid.Compute(content);
            var destination = Path.Combine(Cid.CidsDirectory, cid);
            File.WriteAllBytes(destination, content);
            Console.WriteLine($"Wrote {Path.GetFileName(destination)} from {Path.GetFileName(example)}");
        }

        return 0;
    }
}

namespace Implementations.CSharp;

internal static class Checker
{
    public static int Run()
    {
        if (!Directory.Exists(Cid.CidsDirectory))
        {
            Console.Error.WriteLine($"CID directory not found at '{Cid.CidsDirectory}'.");
            return 1;
        }

        var mismatches = new List<(string Path, string Expected)>();
        var files = Directory.GetFiles(Cid.CidsDirectory).Where(File.Exists).OrderBy(path => path).ToList();
        foreach (var file in files)
        {
            var name = Path.GetFileName(file);
            var bytes = File.ReadAllBytes(file);
            var expected = Cid.Compute(bytes);
            if (!string.Equals(name, expected, StringComparison.Ordinal))
            {
                mismatches.Add((name, expected));
            }
        }

        if (mismatches.Count == 0)
        {
            Console.WriteLine($"All {files.Count} CID files match their contents.");
            return 0;
        }

        Console.WriteLine("Found CID mismatches:");
        foreach (var mismatch in mismatches)
        {
            Console.WriteLine($"- {mismatch.Path} should be {mismatch.Expected}");
        }

        return 1;
    }
}

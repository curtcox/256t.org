namespace Implementations.CSharp;

internal static class Checker
{
    public static int Run()
    {
        return RunAsync().GetAwaiter().GetResult();
    }

    private static async Task<int> RunAsync()
    {
        if (!Directory.Exists(Cid.CidsDirectory))
        {
            Console.Error.WriteLine($"CID directory not found at '{Cid.CidsDirectory}'.");
            return 1;
        }

        var mismatches = new List<(string Name, string Expected)>();
        var downloadFailures = new List<(string Cid, string Error)>();
        var files = Directory.GetFiles(Cid.CidsDirectory).Where(File.Exists).OrderBy(path => path).ToList();
        var baseUrl = "https://256t.org";

        foreach (var file in files)
        {
            var name = Path.GetFileName(file);
            var localContent = File.ReadAllBytes(file);
            var expected = Cid.Compute(localContent);
            
            // Check local CID file
            if (!string.Equals(name, expected, StringComparison.Ordinal))
            {
                mismatches.Add((name, expected));
            }
            
            // Check downloaded content
            try
            {
                var result = await Cid.DownloadAsync(baseUrl, name);
                if (!result.IsValid)
                {
                    downloadFailures.Add((name, result.Computed));
                }
                else if (!result.Content.SequenceEqual(localContent))
                {
                    downloadFailures.Add((name, "content mismatch with local file"));
                }
            }
            catch (Exception ex)
            {
                downloadFailures.Add((name, ex.Message));
            }
        }

        var hasErrors = false;

        if (mismatches.Count > 0)
        {
            Console.WriteLine("Found CID mismatches:");
            foreach (var mismatch in mismatches)
            {
                Console.WriteLine($"- {mismatch.Name} should be {mismatch.Expected}");
            }
            hasErrors = true;
        }

        if (downloadFailures.Count > 0)
        {
            Console.Error.WriteLine("Found download validation failures:");
            foreach (var failure in downloadFailures)
            {
                Console.Error.WriteLine($"- {failure.Cid}: {failure.Error}");
            }
            hasErrors = true;
        }

        if (hasErrors)
        {
            return 1;
        }

        Console.WriteLine($"All {files.Count} CID files match their contents.");
        Console.WriteLine($"All {files.Count} downloaded CIDs are valid.");
        return 0;
    }
}

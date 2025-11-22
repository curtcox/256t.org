using System.Security.Cryptography;

namespace Implementations.CSharp;

internal static class Cid
{
    public static string BaseDirectory { get; } = Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", ".."));

    public static string ExamplesDirectory => Path.Combine(BaseDirectory, "examples");

    public static string CidsDirectory => Path.Combine(BaseDirectory, "cids");

    public static string Compute(byte[] content)
    {
        var prefix = EncodeLength(content.Length);
        var suffix = content.Length <= 64 ? ToBase64Url(content) : ToBase64Url(Hash(content));
        return prefix + suffix;
    }

    private static string EncodeLength(int length)
    {
        var buffer = new byte[6];
        var remaining = (ulong)length;

        for (var i = 0; i < buffer.Length; i++)
        {
            buffer[buffer.Length - 1 - i] = (byte)(remaining >> (8 * i));
        }
        return ToBase64Url(buffer);
    }

    private static byte[] Hash(byte[] content)
    {
        using var sha = SHA512.Create();
        return sha.ComputeHash(content);
    }

    private static string ToBase64Url(byte[] data)
    {
        var encoded = Convert.ToBase64String(data);
        return encoded.TrimEnd('=').Replace('+', '-').Replace('/', '_');
    }

    public record DownloadResult(byte[] Content, string Computed, bool IsValid);

    public static async Task<DownloadResult> DownloadAsync(string baseUrl, string cid)
    {
        var url = baseUrl.TrimEnd('/') + '/' + cid;
        using var client = new HttpClient();
        client.Timeout = TimeSpan.FromSeconds(10);
        
        var response = await client.GetAsync(url);
        if (!response.IsSuccessStatusCode)
        {
            throw new HttpRequestException($"HTTP {(int)response.StatusCode}: {response.ReasonPhrase}");
        }
        
        var content = await response.Content.ReadAsByteArrayAsync();
        var computed = Compute(content);
        var isValid = computed == cid;
        
        return new DownloadResult(content, computed, isValid);
    }
}

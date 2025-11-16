namespace Implementations.CSharp;

internal static class Program
{
    private static readonly string[] ValidCommands = new[] { "check", "generate" };

    private static int Main(string[] args)
    {
        var command = args.FirstOrDefault()?.ToLowerInvariant() ?? "check";
        return command switch
        {
            "check" => Checker.Run(),
            "generate" => Generator.Run(),
            _ => UnknownCommand(command)
        };
    }

    private static int UnknownCommand(string command)
    {
        Console.Error.WriteLine($"Unknown command '{command}'. Valid commands: {string.Join(", ", ValidCommands)}.");
        return 1;
    }
}

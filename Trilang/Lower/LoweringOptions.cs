namespace Trilang.Lower;

public record LoweringOptions(IEnumerable<string> Directives)
{
    public static readonly LoweringOptions Default = new LoweringOptions([]);
}
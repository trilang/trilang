namespace Trilang;

public record struct SourceSpan(SourcePosition Start, SourcePosition End)
{
    public override string ToString()
        => $"{Start} - {End}";

    public SourceSpan Combine(SourceSpan other)
        => new SourceSpan(Start, other.End);

    public SourceSpan Combine(SourcePosition other)
        => new SourceSpan(Start, other);
}
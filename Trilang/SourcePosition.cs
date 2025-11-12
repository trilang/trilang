namespace Trilang;

public readonly record struct SourcePosition(int Index, int Line, int Column)
{
    public SourcePosition() : this(0, 1, 1)
    {
    }

    public override string ToString()
        => $"({Index}:{Line}:{Column})";

    public SourcePosition Add(int length)
        => new SourcePosition(Index + length, Line, Column + length);

    public SourcePosition AddLine(int length = 1)
        => new SourcePosition(Index + length, Line + length, 1);

    public SourceSpan ToSpan(SourcePosition end)
        => new SourceSpan(this, end);

    public SourceSpan ToSpan(int length)
        => new SourceSpan(this, Add(length));

    public SourceSpan ToSpan()
        => new SourceSpan(this, this);
}
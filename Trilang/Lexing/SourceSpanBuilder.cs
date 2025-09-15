namespace Trilang.Lexing;

internal sealed class SourceSpanBuilder
{
    private SourcePosition position;

    public SourceSpanBuilder()
        => position = new SourcePosition();

    public void Add(int length)
        => position = position.Add(length);

    public void NewLine(int length = 1)
        => position = position.AddLine(length);

    public SourceSpan Build(int length)
    {
        var span = position.ToSpan(length);
        position = span.End;

        return span;
    }

    public SourceSpan Build()
        => position.ToSpan();

    public int Index
        => position.Index;
}
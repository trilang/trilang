namespace Trilang.Parsing;

public class VisitorContext<TResult>
{
    public void MarkAsFinished(TResult? result = default)
    {
        IsFinished = true;
        Result = result;
    }

    public bool IsFinished { get; private set; }

    public TResult? Result { get; private set; }
}
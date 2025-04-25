namespace Trilang.Semantics;

public class SemanticAnalysisException : Exception
{
    public SemanticAnalysisException()
    {
    }

    public SemanticAnalysisException(string message) : base(message)
    {
    }

    public SemanticAnalysisException(string message, Exception inner) : base(message, inner)
    {
    }
}
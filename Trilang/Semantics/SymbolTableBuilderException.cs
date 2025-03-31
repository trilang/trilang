namespace Trilang.Semantics;

public class SymbolTableBuilderException : Exception
{
    public SymbolTableBuilderException()
    {
    }

    public SymbolTableBuilderException(string message) : base(message)
    {
    }

    public SymbolTableBuilderException(string message, Exception inner) : base(message, inner)
    {
    }
}
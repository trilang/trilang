namespace Trilang.Semantics;

public class TypeCheckerException : Exception
{
    public TypeCheckerException()
    {
    }

    public TypeCheckerException(string message) : base(message)
    {
    }

    public TypeCheckerException(string message, Exception inner) : base(message, inner)
    {
    }
}
namespace Trilang.IntermediateRepresentation;

public class IrException : Exception
{
    public IrException()
    {
    }

    public IrException(string message) : base(message)
    {
    }

    public IrException(string message, Exception inner) : base(message, inner)
    {
    }
}
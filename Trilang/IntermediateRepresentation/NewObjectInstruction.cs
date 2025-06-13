using Trilang.Metadata;

namespace Trilang.IntermediateRepresentation;

public class NewObjectInstruction : IInstruction, IEquatable<NewObjectInstruction>
{
    public NewObjectInstruction(
        Register result,
        ConstructorMetadata constructor,
        IReadOnlyList<Register> arguments)
    {
        Result = result;
        Constructor = constructor;
        Arguments = arguments;
    }

    public static bool operator ==(NewObjectInstruction? left, NewObjectInstruction? right)
        => Equals(left, right);

    public static bool operator !=(NewObjectInstruction? left, NewObjectInstruction? right)
        => !Equals(left, right);

    public bool Equals(NewObjectInstruction? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Result.Equals(other.Result) &&
               Constructor.Equals(other.Constructor) &&
               Arguments.SequenceEqual(other.Arguments);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((NewObjectInstruction)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Result, Constructor, Arguments);

    public Register Result { get; }

    public ConstructorMetadata Constructor { get; }

    public IReadOnlyList<Register> Arguments { get; }
}
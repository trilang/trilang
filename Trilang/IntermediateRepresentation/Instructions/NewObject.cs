using Trilang.Metadata;

namespace Trilang.IntermediateRepresentation.Instructions;

public record NewObject(
    Register Result,
    ConstructorMetadata Constructor,
    IReadOnlyList<Register> Arguments) : IInstruction
{
    public virtual bool Equals(NewObject? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Result.Equals(other.Result) &&
               Constructor.Equals(other.Constructor) &&
               Arguments.SequenceEqual(other.Arguments);
    }

    public override int GetHashCode()
        => HashCode.Combine(Result, Constructor, Arguments);

    public override string ToString()
        => $"new\t{Constructor.DeclaringType}, {string.Join(", ", Arguments)}";
}
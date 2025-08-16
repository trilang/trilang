namespace Trilang.IntermediateRepresentation.Instructions;

public record Call(Register Result, Register Function, IReadOnlyList<Register> Parameters, bool IsStatic) : IInstruction
{
    public virtual bool Equals(Call? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Result.Equals(other.Result) &&
               Function.Equals(other.Function) &&
               Parameters.SequenceEqual(other.Parameters) &&
               IsStatic == other.IsStatic;
    }

    public override int GetHashCode()
        => HashCode.Combine(Result, Function, Parameters);

    public override string ToString()
        => Parameters.Count == 0
            ? $"call\t{Result}, {Function}"
            : $"call\t{Result}, {Function}, ({string.Join(", ", Parameters)})";
}
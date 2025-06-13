namespace Trilang.IntermediateRepresentation;

public class Block : IEquatable<Block>
{
    private readonly List<IInstruction> instructions;

    public Block(string label) : this(label, [])
    {
    }

    public Block(string label, IEnumerable<IInstruction> instructions)
    {
        Label = label;
        this.instructions = [..instructions];
    }

    public static bool operator ==(Block? left, Block? right)
        => Equals(left, right);

    public static bool operator !=(Block? left, Block? right)
        => !Equals(left, right);

    public bool Equals(Block? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Label == other.Label &&
               instructions.SequenceEqual(other.instructions) &&
               Equals(Previous, other.Previous) &&
               Equals(Next, other.Next);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((Block)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(instructions, Label);

    public void AddInstruction(IInstruction instruction)
        => instructions.Add(instruction);

    public string Label { get; }

    public IReadOnlyList<IInstruction> Instructions
        => instructions;

    // TODO: multiple prev/next blocks
    public Block? Previous { get; set; }

    public Block? Next { get; set; }
}
namespace Trilang.IntermediateRepresentation;

public class Block : IEquatable<Block>
{
    private readonly List<IInstruction> instructions;
    private readonly List<Block> previous;
    private readonly List<Block> next;

    public Block(string label)
        : this(label, [])
    {
    }

    public Block(string label, IEnumerable<IInstruction> instructions)
        : this(label, instructions, [])
    {
    }

    public Block(string label, IEnumerable<IInstruction> instructions, IEnumerable<Block> next)
    {
        Label = label;
        this.instructions = [..instructions];
        previous = [];
        this.next = [];

        foreach (var block in next)
            AddNext(block);
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
               Next.SequenceEqual(other.Next);
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

    public void AddPrevious(Block block)
    {
        previous.Add(block);
        block.next.Add(this);
    }

    public void AddNext(Block block)
    {
        next.Add(block);
        block.previous.Add(this);
    }

    public string Label { get; }

    public IReadOnlyList<IInstruction> Instructions
        => instructions;

    public IReadOnlyList<Block> Previous
        => previous;

    public IReadOnlyList<Block> Next
        => next;
}
using System.Diagnostics;
using System.Text;
using Trilang.IntermediateRepresentation.Instructions;

namespace Trilang.IntermediateRepresentation;

[DebuggerDisplay("{Label}")]
public class Block : IEquatable<Block>
{
    private readonly List<IInstruction> instructions;
    private readonly Dictionary<string, (Register Register, bool IsDefinition)> assignments;
    private readonly HashSet<Block> previous;
    private readonly HashSet<Block> next;

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
        assignments = [];
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
               instructions.SequenceEqual(other.instructions);
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

    public override string ToString()
    {
        var sb = new StringBuilder();
        sb.Append($"{Label}");
        sb.Append(':');

        if (previous.Count > 0)
        {
            sb.Append(" <- ");

            foreach (var block in previous)
                sb.Append(block.Label).Append(", ");

            sb.Remove(sb.Length - 2, 2);
        }

        sb.AppendLine();

        foreach (var instruction in instructions)
            sb.Append('\t').AppendLine(instruction.ToString());

        return sb.ToString();
    }

    public void AddInstruction(IInstruction instruction)
        => instructions.Add(instruction);

    public void InsertInstruction(int index, IInstruction instruction)
        => instructions.Insert(index, instruction);

    public void ReplaceInstruction(int index, IInstruction instruction)
        => instructions[index] = instruction;

    public void AddAssignment(string variable, Register value, bool isDefinition)
        => assignments[variable] = (value, isDefinition);

    public Register? GetAssignment(string variable)
        => assignments.TryGetValue(variable, out var value) ? value.Register : null;

    public Register? FindAssignment(string name)
    {
        var q = new Queue<Block>();
        var visited = new HashSet<Block>();
        q.Enqueue(this);

        while (q.TryDequeue(out var block))
        {
            if (!visited.Add(block))
                continue;

            var assignment = block.GetAssignment(name);
            if (assignment is not null)
                return assignment;

            foreach (var previousBlock in block.Previous)
                if (!visited.Contains(previousBlock))
                    q.Enqueue(previousBlock);
        }

        return null;
    }

    public string? FindVariable(Register register)
    {
        var q = new Queue<Block>();
        var visited = new HashSet<Block>();
        q.Enqueue(this);

        while (q.TryDequeue(out var block))
        {
            if (!visited.Add(block))
                continue;

            var assignment = block.assignments.FirstOrDefault(a => a.Value.Register == register).Key;
            if (assignment is not null)
                return assignment;

            foreach (var previousBlock in block.Previous)
                if (!visited.Contains(previousBlock))
                    q.Enqueue(previousBlock);
        }

        return null;
    }

    public void AddPrevious(Block block)
    {
        if (!previous.Add(block))
            return;

        block.next.Add(this);
    }

    public void AddNext(Block block)
    {
        if (!next.Add(block))
            return;

        block.previous.Add(this);
    }

    public string Label { get; }

    public IReadOnlyList<IInstruction> Instructions
        => instructions;

    public IReadOnlyCollection<Block> Previous
        => previous;

    public IReadOnlyCollection<Block> Next
        => next;

    public IReadOnlyDictionary<string, (Register Register, bool IsDefinition)> Assignments
        => assignments;

    public int Order { get; set; }
}
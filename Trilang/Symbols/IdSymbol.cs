using Trilang.Semantics.Model;

namespace Trilang.Symbols;

public class IdSymbol : IEquatable<IdSymbol>
{
    private readonly List<ISemanticNode> nodes;

    public IdSymbol(string name, ISemanticNode? node)
    {
        nodes = [];

        Name = name;

        if (node is not null)
            nodes.Add(node);
    }

    public static bool operator ==(IdSymbol? left, IdSymbol? right)
        => Equals(left, right);

    public static bool operator !=(IdSymbol? left, IdSymbol? right)
        => !Equals(left, right);

    public bool Equals(IdSymbol? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name &&
               nodes.SequenceEqual(other.nodes);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((IdSymbol)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, nodes);

    public override string ToString()
        => $"Id: {Name}";

    public void AddNode(ISemanticNode node)
        => nodes.Add(node);

    public string Name { get; }

    public IReadOnlyList<ISemanticNode> Nodes => nodes;
}
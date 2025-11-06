using Trilang.Semantics.Model;

namespace Trilang.Symbols;

public class IdSymbol : IEquatable<IdSymbol>
{
    public IdSymbol(string name, ISemanticNode? node)
    {
        Name = name;
        Node = node;
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
               Equals(Node, other.Node);
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
        => HashCode.Combine(Name, Node);

    public override string ToString()
        => $"Id: {Name}";

    public string Name { get; }

    public ISemanticNode? Node { get; }
}
using Trilang.Parsing.Ast;

namespace Trilang.Symbols;

public abstract class Symbol<T> : IEquatable<Symbol<T>>
    where T : ISyntaxNode
{
    protected Symbol(SymbolKind kind, string name, T node)
    {
        Kind = kind;
        Name = name;
        Node = node;
    }

    public static bool operator ==(Symbol<T>? left, Symbol<T>? right)
        => Equals(left, right);

    public static bool operator !=(Symbol<T>? left, Symbol<T>? right)
        => !Equals(left, right);

    public bool Equals(Symbol<T>? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Kind == other.Kind &&
               Name == other.Name &&
               Node.Equals(other.Node);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((Symbol<T>)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine((int)Kind, Name, Node);

    public SymbolKind Kind { get; }

    public string Name { get; }

    public T Node { get; }

    // public object? Reference { get; }
}
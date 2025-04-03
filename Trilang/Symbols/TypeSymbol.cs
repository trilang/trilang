using Trilang.Parsing.Ast;

namespace Trilang.Symbols;

public class TypeSymbol : Symbol<ISyntaxNode?>, IEquatable<TypeSymbol>
{
    // TODO:
    public TypeSymbol(string name, bool isArray, ISyntaxNode? node)
        : base(SymbolKind.Type, name, node)
    {
        IsArray = isArray;
    }

    public static bool operator ==(TypeSymbol? left, TypeSymbol? right)
        => Equals(left, right);

    public static bool operator !=(TypeSymbol? left, TypeSymbol? right)
        => !Equals(left, right);

    public bool Equals(TypeSymbol? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return base.Equals(other) && IsArray == other.IsArray;
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((TypeSymbol)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(base.GetHashCode(), IsArray);

    public bool IsArray { get; }
}
using Trilang.Parsing.Ast;

namespace Trilang.Symbols;

public class TypeSymbol : Symbol<ISyntaxNode?>, IEquatable<TypeSymbol>
{
    public TypeSymbol(TypeSymbolKind typeKind, string name, ISyntaxNode? node)
        : base(SymbolKind.Type, name, node)
        => TypeKind = typeKind;

    public static TypeSymbol Type(string name, TypeDeclarationNode node)
        => new TypeSymbol(TypeSymbolKind.Type, name, node);

    public static TypeSymbol Array(string name)
        => new TypeSymbol(TypeSymbolKind.Array, name, null);

    public static TypeSymbol Alias(string name, TypeAliasDeclarationNode node)
        => new TypeSymbol(TypeSymbolKind.Alias, name, node);

    public static TypeSymbol Alias(string name, FunctionTypeDeclarationNode node)
        => new TypeSymbol(TypeSymbolKind.Alias, name, node);

    public static TypeSymbol Function(string name, FunctionTypeDeclarationNode node)
        => new TypeSymbol(TypeSymbolKind.Function, name, node);

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

        return base.Equals(other) && TypeKind == other.TypeKind;
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
        => HashCode.Combine(base.GetHashCode(), Kind);

    public TypeSymbolKind TypeKind { get; }

    public bool IsType => TypeKind == TypeSymbolKind.Type;

    public bool IsArray => TypeKind == TypeSymbolKind.Array;

    public bool IsAlias => TypeKind == TypeSymbolKind.Alias;

    public bool IsFunction => TypeKind == TypeSymbolKind.Function;
}
using Trilang.Parsing.Ast;

namespace Trilang.Symbols;

public class TypeSymbol : ISymbol, IEquatable<TypeSymbol>
{
    private TypeSymbol(TypeSymbolKind typeKind, string name, ISyntaxNode node)
    {
        TypeKind = typeKind;
        Name = name;
        Node = node;
    }

    public static TypeSymbol Type(TypeDeclarationNode node)
        => new TypeSymbol(TypeSymbolKind.Type, node.Name, node);

    public static TypeSymbol Array(ArrayTypeNode node)
        => new TypeSymbol(TypeSymbolKind.Array, node.Name, node);

    public static TypeSymbol Alias(TypeAliasDeclarationNode node)
        => new TypeSymbol(TypeSymbolKind.Alias, node.FullName, node);

    public static TypeSymbol FunctionType(FunctionTypeNode node)
        => new TypeSymbol(TypeSymbolKind.Function, node.Name, node);

    public static TypeSymbol Interface(InterfaceNode node)
        => new TypeSymbol(TypeSymbolKind.Interface, node.Name, node);

    public static TypeSymbol DiscriminatedUnion(DiscriminatedUnionNode node)
        => new TypeSymbol(TypeSymbolKind.DiscriminatedUnion, node.Name, node);

    public static TypeSymbol Tuple(TupleTypeNode node)
        => new TypeSymbol(TypeSymbolKind.Tuple, node.Name, node);

    public static TypeSymbol OpenGenericType(TypeDeclarationNode node)
        => new TypeSymbol(TypeSymbolKind.OpenGenericType, node.FullName, node);

    public static TypeSymbol GenericType(GenericTypeNode node)
        => new TypeSymbol(TypeSymbolKind.GenericType, node.Name, node);

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

        return TypeKind == other.TypeKind &&
               Name == other.Name &&
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

        return Equals((TypeSymbol)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine((int)TypeKind, Name, Node);

    public override string ToString()
        => $"{TypeKind}: {Name}";

    public TypeSymbolKind TypeKind { get; }

    public string Name { get; }

    public ISyntaxNode Node { get; }

    public bool IsType => TypeKind == TypeSymbolKind.Type;

    public bool IsArray => TypeKind == TypeSymbolKind.Array;

    public bool IsAlias => TypeKind == TypeSymbolKind.Alias;

    public bool IsFunction => TypeKind == TypeSymbolKind.Function;

    public bool IsInterface => TypeKind == TypeSymbolKind.Interface;

    public bool IsDiscriminatedUnion => TypeKind == TypeSymbolKind.DiscriminatedUnion;

    public bool IsTuple => TypeKind == TypeSymbolKind.Tuple;

    public bool IsGenericType => TypeKind == TypeSymbolKind.OpenGenericType;

    public bool IsClosedGenericType => TypeKind == TypeSymbolKind.GenericType;
}
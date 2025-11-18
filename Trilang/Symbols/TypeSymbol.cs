using Trilang.Semantics.Model;

namespace Trilang.Symbols;

public class TypeSymbol : IEquatable<TypeSymbol>
{
    private TypeSymbol(TypeSymbolKind typeKind, string name, ISemanticNode node)
    {
        TypeKind = typeKind;
        Name = name;
        Node = node;
    }

    public static TypeSymbol TypeDeclaration(TypeDeclaration node)
        => new TypeSymbol(TypeSymbolKind.TypeDeclaration, node.Name, node);

    public static TypeSymbol Array(ArrayType node)
        => new TypeSymbol(TypeSymbolKind.Array, node.Name, node);

    public static TypeSymbol Alias(AliasDeclaration node)
        => new TypeSymbol(TypeSymbolKind.Alias, node.FullName, node);

    public static TypeSymbol FunctionType(FunctionType node)
        => new TypeSymbol(TypeSymbolKind.Function, node.Name, node);

    public static TypeSymbol Interface(Interface node)
        => new TypeSymbol(TypeSymbolKind.Interface, node.Name, node);

    public static TypeSymbol DiscriminatedUnion(DiscriminatedUnion node)
        => new TypeSymbol(TypeSymbolKind.DiscriminatedUnion, node.Name, node);

    public static TypeSymbol Tuple(TupleType node)
        => new TypeSymbol(TypeSymbolKind.Tuple, node.Name, node);

    public static TypeSymbol GenericTypeDeclaration(TypeDeclaration node)
        => new TypeSymbol(TypeSymbolKind.GenericTypeDeclaration, node.FullName, node);

    public static TypeSymbol GenericType(GenericType node)
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

    public ISemanticNode Node { get; }

    public bool IsTypeDeclaration => TypeKind == TypeSymbolKind.TypeDeclaration;

    public bool IsArray => TypeKind == TypeSymbolKind.Array;

    public bool IsAlias => TypeKind == TypeSymbolKind.Alias;

    public bool IsFunction => TypeKind == TypeSymbolKind.Function;

    public bool IsInterface => TypeKind == TypeSymbolKind.Interface;

    public bool IsDiscriminatedUnion => TypeKind == TypeSymbolKind.DiscriminatedUnion;

    public bool IsTuple => TypeKind == TypeSymbolKind.Tuple;

    public bool IsGenericTypeDeclaration => TypeKind == TypeSymbolKind.GenericTypeDeclaration;

    public bool IsGenericType => TypeKind == TypeSymbolKind.GenericType;
}
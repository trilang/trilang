using Trilang.Parsing.Ast;

namespace Trilang.Symbols;

public class IdSymbol : ISymbol, IEquatable<IdSymbol>
{
    public IdSymbol(FunctionDeclarationNode node) : this(node.Name, node)
    {
    }

    public IdSymbol(VariableDeclarationStatementNode node) : this(node.Name, node)
    {
    }

    public IdSymbol(ParameterNode node) : this(node.Name, node)
    {
    }

    public IdSymbol(FieldDeclarationNode node) : this(node.Name, node)
    {
    }

    public IdSymbol(MethodDeclarationNode node) : this(node.Name, node)
    {
    }

    public IdSymbol(InterfaceFieldNode node) : this(node.Name, node)
    {
    }

    public IdSymbol(InterfaceMethodNode node) : this(node.Name, node)
    {
    }

    public IdSymbol(string name, ISyntaxNode? node)
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

    public string Name { get; }

    public ISyntaxNode? Node { get; }
}
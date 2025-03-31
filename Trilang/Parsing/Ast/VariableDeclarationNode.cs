using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public abstract class VariableDeclarationNode : ISyntaxNode, IEquatable<VariableDeclarationNode>
{
    protected VariableDeclarationNode(string name, string type)
    {
        Name = name;
        Type = type;
    }

    public static bool operator ==(VariableDeclarationNode? left, VariableDeclarationNode? right)
        => Equals(left, right);

    public static bool operator !=(VariableDeclarationNode? left, VariableDeclarationNode? right)
        => !Equals(left, right);

    public bool Equals(VariableDeclarationNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name &&
               Type == other.Type &&
               Equals(TypeMetadata, other.TypeMetadata) &&
               Equals(SymbolTable, other.SymbolTable);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((VariableDeclarationNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, Type);

    public override string ToString()
    {
        var formatter = new CommonFormatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public abstract void Accept(IVisitor visitor);

    public abstract void Accept<TContext>(IVisitor<TContext> visitor, TContext context);

    public ISyntaxNode? Parent { get; set; }

    public string Name { get; }

    public string Type { get; }

    public TypeMetadata? TypeMetadata { get; set; }

    public SymbolTable? SymbolTable { get; set; }
}
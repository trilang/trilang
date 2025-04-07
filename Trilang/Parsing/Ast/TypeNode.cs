using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class TypeNode : ISyntaxNode, IEquatable<TypeNode>
{
    private TypeNode(string name, bool isArray)
    {
        Name = name;
        IsArray = isArray;
    }

    public static TypeNode Create(string name)
        => new TypeNode(name, false);

    public static TypeNode Array(string name)
        => new TypeNode(name, true);

    public static bool operator ==(TypeNode? left, TypeNode? right)
        => Equals(left, right);

    public static bool operator !=(TypeNode? left, TypeNode? right)
        => !Equals(left, right);

    public bool Equals(TypeNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name &&
               IsArray == other.IsArray &&
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

        return Equals((TypeNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, IsArray);

    public override string ToString()
    {
        var formatter = new CommonFormatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.Visit(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.Visit(this, context);

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }

    public string Name { get; }

    public bool IsArray { get; }

    public IMetadata? Metadata { get; set; }
}
using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class MemberAccessExpressionNode : IExpressionNode, IEquatable<MemberAccessExpressionNode>
{
    public MemberAccessExpressionNode(string name)
        => Name = name;

    public static bool operator ==(MemberAccessExpressionNode? left, MemberAccessExpressionNode? right)
        => Equals(left, right);

    public static bool operator !=(MemberAccessExpressionNode? left, MemberAccessExpressionNode? right)
        => !Equals(left, right);

    public bool Equals(MemberAccessExpressionNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name &&
               Equals(ReturnTypeMetadata, other.ReturnTypeMetadata) &&
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

        return Equals((MemberAccessExpressionNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name);

    public override string ToString()
    {
        var formatter = new Formatter();
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

    public ITypeMetadata? ReturnTypeMetadata { get; set; }
}
using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class MemberAccessExpressionNode : IExpressionNode, IEquatable<MemberAccessExpressionNode>
{
    public const string This = "this";
    public const string Field = "field";
    public const string Value = "value";

    public MemberAccessExpressionNode(string name) : this(null, name)
    {
    }

    public MemberAccessExpressionNode(MemberAccessExpressionNode? member, string name)
    {
        Member = member;
        Name = name;
    }

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
               Equals(Member, other.Member) &&
               Equals(ReturnTypeMetadata, other.ReturnTypeMetadata);
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
        => HashCode.Combine(Member, Name);

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

    public MemberAccessExpressionNode? Member { get; }

    public string Name { get; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }

    public bool IsThis
        => Name == This;
}
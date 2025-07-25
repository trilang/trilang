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

    public MemberAccessExpressionNode(IExpressionNode? member, string name)
    {
        Member = member;
        Name = name;

        if (Member is not null)
            Member.Parent = this;
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
        => visitor.VisitMemberAccess(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitMemberAccess(this, context);

    public ISyntaxNode Transform(ITransformer transformer)
        => transformer.TransformMemberAccess(this);

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }

    public IExpressionNode? Member { get; set; }

    public string Name { get; }

    public object? Reference { get; set; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }

    public bool IsThis
        => Name == This;

    public bool IsField
        => Name == Field;

    public bool IsValue
        => Name == Value;
}
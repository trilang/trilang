using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class CallExpressionNode : IExpressionNode, IEquatable<CallExpressionNode>
{
    public CallExpressionNode(MemberAccessExpressionNode member, IReadOnlyList<IExpressionNode> parameters)
    {
        Member = member;
        Parameters = parameters;

        Member.Parent = this;
        foreach (var parameter in parameters)
            parameter.Parent = this;
    }

    public static bool operator ==(CallExpressionNode? left, CallExpressionNode? right)
        => Equals(left, right);

    public static bool operator !=(CallExpressionNode? left, CallExpressionNode? right)
        => !Equals(left, right);

    public bool Equals(CallExpressionNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Member.Equals(other.Member) &&
               Equals(ReturnTypeMetadata, other.ReturnTypeMetadata) &&
               Parameters.SequenceEqual(other.Parameters);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((CallExpressionNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Member, Parameters);

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

    public MemberAccessExpressionNode Member { get; }

    public IReadOnlyList<IExpressionNode> Parameters { get; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }
}
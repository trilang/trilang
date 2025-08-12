using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class CallExpressionNode : IExpressionNode, IEquatable<CallExpressionNode>
{
    public CallExpressionNode(IExpressionNode member, IReadOnlyList<IExpressionNode> parameters)
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
        => visitor.VisitCall(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitCall(this, context);

    public ISyntaxNode Transform(ITransformer transformer)
        => transformer.TransformCall(this);

    public IExpressionNode Clone()
        => new CallExpressionNode(Member.Clone(), Parameters.Select(x => x.Clone()).ToArray())
        {
            SymbolTable = SymbolTable,
        };

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }

    public IExpressionNode Member { get; }

    public IReadOnlyList<IExpressionNode> Parameters { get; }

    public FunctionTypeMetadata? Metadata
        => Member.ReturnTypeMetadata as FunctionTypeMetadata;

    public ITypeMetadata? ReturnTypeMetadata
        => Metadata?.ReturnType;
}
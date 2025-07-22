using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class UnaryExpressionNode : IExpressionNode, IEquatable<UnaryExpressionNode>
{
    public UnaryExpressionNode(UnaryExpressionKind kind, IExpressionNode operand)
    {
        Kind = kind;
        Operand = operand;
        Operand.Parent = this;
    }

    public static bool operator ==(UnaryExpressionNode? left, UnaryExpressionNode? right)
        => Equals(left, right);

    public static bool operator !=(UnaryExpressionNode? left, UnaryExpressionNode? right)
        => !Equals(left, right);

    public bool Equals(UnaryExpressionNode? other)
    {
        if (other is null) return false;

        if (ReferenceEquals(this, other))
            return true;

        return Kind.Equals(other.Kind) &&
               Operand.Equals(other.Operand) &&
               Equals(ReturnTypeMetadata, other.ReturnTypeMetadata);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null) return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((UnaryExpressionNode)obj);
    }

    public override int GetHashCode()
        => Operand.GetHashCode();

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitUnaryExpression(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitUnaryExpression(this, context);

    public ISyntaxNode Transform(ITransformer transformer)
        => transformer.TransformUnaryExpression(this);

    public ISyntaxNode? Parent { get; set; }

    public UnaryExpressionKind Kind { get; }

    public IExpressionNode Operand { get; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }

    public ISymbolTable? SymbolTable { get; set; }
}
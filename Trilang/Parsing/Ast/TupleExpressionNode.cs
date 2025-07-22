using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class TupleExpressionNode : IExpressionNode, IEquatable<TupleExpressionNode>
{
    public TupleExpressionNode(IReadOnlyList<IExpressionNode> expressions)
    {
        if (expressions.Count <= 1)
            throw new ArgumentException("Tuple must have at least 2 elements", nameof(expressions));

        Expressions = expressions;
    }

    public static bool operator ==(TupleExpressionNode? left, TupleExpressionNode? right)
        => Equals(left, right);

    public static bool operator !=(TupleExpressionNode? left, TupleExpressionNode? right)
        => !Equals(left, right);

    public bool Equals(TupleExpressionNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Expressions.SequenceEqual(other.Expressions) &&
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

        return Equals((TupleExpressionNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Expressions);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitTuple(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitTuple(this, context);

    public ISyntaxNode Transform(ITransformer transformer)
        => transformer.TransformTuple(this);

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }

    public IReadOnlyList<IExpressionNode> Expressions { get; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }
}
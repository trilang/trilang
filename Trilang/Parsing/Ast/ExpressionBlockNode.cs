using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class ExpressionBlockNode : IExpressionNode, IEquatable<ExpressionBlockNode>
{
    private readonly List<IStatementNode> statements;

    public ExpressionBlockNode(IEnumerable<IStatementNode> expressions)
        => this.statements = [..expressions];

    public static bool operator ==(ExpressionBlockNode? left, ExpressionBlockNode? right)
        => Equals(left, right);

    public static bool operator !=(ExpressionBlockNode? left, ExpressionBlockNode? right)
        => !Equals(left, right);

    public bool Equals(ExpressionBlockNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return statements.Equals(other.statements);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((ExpressionBlockNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(statements);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitExpressionBlock(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitExpressionBlock(this, context);

    public ISyntaxNode Transform(ITransformer transformer)
        => transformer.TransformExpressionBlock(this);

    public IExpressionNode Clone()
        => throw new NotSupportedException();

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }

    public IReadOnlyList<IStatementNode> Statements
        => statements;

    public ITypeMetadata? ReturnTypeMetadata
        => statements.Count > 0
            ? (statements[^1] as ExpressionStatementNode)?.Expression.ReturnTypeMetadata
            : null;
}
using Trilang.Metadata;
using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class ExpressionBlockNode : IExpressionNode, IBlockNode, IEquatable<ExpressionBlockNode>
{
    private readonly List<IStatementNode> statements;

    public ExpressionBlockNode(IEnumerable<IStatementNode> expressions)
    {
        this.statements = [..expressions];

        foreach (var statement in statements)
            statement.Parent = this;
    }

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

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformExpressionBlock(this);

    public IExpressionNode Clone()
        => throw new NotSupportedException();

    public void Add(IStatementNode declaration)
    {
        declaration.Parent = this;
        statements.Add(declaration);
    }

    public void Insert(int i, IStatementNode declaration)
    {
        declaration.Parent = this;
        statements.Insert(i, declaration);
    }

    public void InsertAfter(IStatementNode declaration, IStatementNode after)
    {
        var index = statements.IndexOf(declaration);
        statements.Insert(index + 1, after);
    }

    public void Replace(IStatementNode oldStatement, IStatementNode newStatement)
    {
        var index = statements.IndexOf(oldStatement);
        statements[index] = newStatement;

        oldStatement.Parent = null;
        newStatement.Parent = this;
    }

    public void Remove(IStatementNode declaration)
        => statements.Remove(declaration);

    public ISyntaxNode? Parent { get; set; }

    public IReadOnlyList<IStatementNode> Statements
        => statements;

    public ITypeMetadata? ReturnTypeMetadata
        => statements.Count > 0
            ? (statements[^1] as ExpressionStatementNode)?.Expression.ReturnTypeMetadata
            : null;
}
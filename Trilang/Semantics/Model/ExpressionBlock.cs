using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class ExpressionBlock : IExpression, IBlock
{
    private readonly List<IStatement> statements;

    public ExpressionBlock(IEnumerable<IStatement> statements)
    {
        this.statements = [..statements];

        foreach (var statement in this.statements)
            statement.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitExpressionBlock(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitExpressionBlock(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformExpressionBlock(this);

    public IExpression Clone()
        => throw new NotSupportedException();

    public void Add(IStatement declaration)
    {
        declaration.Parent = this;
        statements.Add(declaration);
    }

    public void Insert(int i, IStatement declaration)
    {
        declaration.Parent = this;
        statements.Insert(i, declaration);
    }

    public void InsertAfter(IStatement declaration, IStatement after)
    {
        var index = statements.IndexOf(declaration);
        statements.Insert(index + 1, after);
    }

    public void Replace(IStatement oldStatement, IStatement newStatement)
    {
        var index = statements.IndexOf(oldStatement);
        statements[index] = newStatement;

        oldStatement.Parent = null;
        newStatement.Parent = this;
    }

    public void Remove(IStatement declaration)
        => statements.Remove(declaration);

    public ISemanticNode? Parent { get; set; }

    public IReadOnlyList<IStatement> Statements
        => statements;

    public ITypeMetadata? ReturnTypeMetadata
        => statements.Count > 0
            ? (statements[^1] as ExpressionStatement)?.Expression.ReturnTypeMetadata
            : null;
}
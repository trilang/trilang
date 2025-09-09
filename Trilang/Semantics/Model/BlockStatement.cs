namespace Trilang.Semantics.Model;

public class BlockStatement : IStatement, IBlock
{
    private readonly List<IStatement> statements;

    public BlockStatement()
        : this([])
    {
    }

    public BlockStatement(IReadOnlyList<IStatement> statements)
    {
        this.statements = [..statements];

        foreach (var statement in statements)
            statement.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitBlock(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitBlock(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformBlock(this);

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
}
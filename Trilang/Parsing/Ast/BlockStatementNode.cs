using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class BlockStatementNode : IStatementNode, IBlockNode, IEquatable<BlockStatementNode>
{
    private readonly List<IStatementNode> statements;

    public BlockStatementNode()
        : this([])
    {
    }

    public BlockStatementNode(IReadOnlyList<IStatementNode> statements)
    {
        this.statements = [..statements];

        foreach (var statement in statements)
            statement.Parent = this;
    }

    public static bool operator ==(BlockStatementNode? left, BlockStatementNode? right)
        => Equals(left, right);

    public static bool operator !=(BlockStatementNode? left, BlockStatementNode? right)
        => !Equals(left, right);

    public bool Equals(BlockStatementNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Statements.SequenceEqual(other.Statements);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((BlockStatementNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Statements);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitBlock(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitBlock(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformBlock(this);

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
}
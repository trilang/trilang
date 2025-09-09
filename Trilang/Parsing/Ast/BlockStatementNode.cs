using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class BlockStatementNode : IStatementNode
{
    private readonly List<IStatementNode> statements;

    public BlockStatementNode()
        : this([])
    {
    }

    public BlockStatementNode(IReadOnlyList<IStatementNode> statements)
        => this.statements = [..statements];

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitBlock(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformBlock(this);

    public IReadOnlyList<IStatementNode> Statements
        => statements;
}
using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class BlockStatementNode : IStatementNode
{
    private readonly List<IStatementNode> statements;

    public BlockStatementNode(SourceSpan sourceSpan)
        : this(sourceSpan, [])
    {
    }

    public BlockStatementNode(SourceSpan sourceSpan, IReadOnlyList<IStatementNode> statements)
    {
        SourceSpan = sourceSpan;
        this.statements = [..statements];
    }

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

    public SourceSpan SourceSpan { get; }

    public IReadOnlyList<IStatementNode> Statements
        => statements;
}
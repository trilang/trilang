using Trilang.Parsing.Ast;

namespace Trilang.Semantics.Passes.ControlFlow;

public class SemanticBlock
{
    private readonly string name;
    private readonly BlockStatementNode node;
    private readonly List<IStatementNode> statements;
    private readonly HashSet<SemanticBlock> previous;
    private readonly HashSet<SemanticBlock> next;

    public SemanticBlock(string name, BlockStatementNode node) : this(name, node, [])
    {
    }

    public SemanticBlock(string name, BlockStatementNode node, IEnumerable<IStatementNode> statements)
    {
        this.name = name;
        this.node = node;
        this.statements = [..statements];
        previous = [];
        next = [];
    }

    public void AddStatement(IStatementNode statement)
        => statements.Add(statement);

    public void AddNext(SemanticBlock block)
    {
        if (block == this)
            return;

        if (!next.Add(block))
            return;

        block.previous.Add(this);
    }

    public override string ToString()
        => name;

    public string Name => name;

    public BlockStatementNode BlockNode => node;

    public IReadOnlyCollection<IStatementNode> Statements => statements;

    public IReadOnlyCollection<SemanticBlock> Previous => previous;

    public IReadOnlyCollection<SemanticBlock> Next => next;
}
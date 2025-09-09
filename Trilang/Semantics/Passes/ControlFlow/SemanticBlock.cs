using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes.ControlFlow;

public class SemanticBlock
{
    private readonly string name;
    private readonly BlockStatement node;
    private readonly List<IStatement> statements;
    private readonly HashSet<SemanticBlock> previous;
    private readonly HashSet<SemanticBlock> next;

    public SemanticBlock(string name, BlockStatement node) : this(name, node, [])
    {
    }

    public SemanticBlock(string name, BlockStatement node, IEnumerable<IStatement> statements)
    {
        this.name = name;
        this.node = node;
        this.statements = [..statements];
        previous = [];
        next = [];
    }

    public void AddStatement(IStatement statement)
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

    public BlockStatement BlockNode => node;

    public IReadOnlyCollection<IStatement> Statements => statements;

    public IReadOnlyCollection<SemanticBlock> Previous => previous;

    public IReadOnlyCollection<SemanticBlock> Next => next;
}
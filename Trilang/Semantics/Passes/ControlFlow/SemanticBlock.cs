using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes.ControlFlow;

public class SemanticBlock
{
    private readonly List<IStatement> statements;
    private readonly HashSet<SemanticBlock> previous;
    private readonly HashSet<SemanticBlock> next;

    public SemanticBlock(string name, BlockStatement node) : this(name, node, [])
    {
    }

    public SemanticBlock(string name, BlockStatement node, IEnumerable<IStatement> statements)
    {
        Name = name;
        BlockNode = node;
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
        => Name;

    public string Name { get; }

    public BlockStatement BlockNode { get; }

    public IReadOnlyCollection<IStatement> Statements => statements;

    public IReadOnlyCollection<SemanticBlock> Previous => previous;

    public IReadOnlyCollection<SemanticBlock> Next => next;
}
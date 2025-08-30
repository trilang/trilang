using Trilang.Parsing.Ast;

namespace Trilang.Semantics.Passes.ControlFlow;

public class SemanticBlock
{
    private readonly string name;
    private readonly List<IStatementNode> statements;
    private readonly HashSet<SemanticBlock> previous;
    private readonly HashSet<SemanticBlock> next;

    public SemanticBlock(string name) : this(name, [])
    {
    }

    public SemanticBlock(string name, IEnumerable<IStatementNode> statements)
    {
        this.name = name;
        this.statements = [..statements];
        previous = [];
        next = [];
    }

    public void AddStatement(IStatementNode node)
        => statements.Add(node);

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

    public IReadOnlyCollection<IStatementNode> Statements => statements;

    public IReadOnlyCollection<SemanticBlock> Previous => previous;

    public IReadOnlyCollection<SemanticBlock> Next => next;
}
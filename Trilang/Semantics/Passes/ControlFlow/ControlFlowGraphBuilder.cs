using Trilang.Parsing.Ast;

namespace Trilang.Semantics.Passes.ControlFlow;

internal class ControlFlowGraphBuilder
{
    private readonly SemanticBlock entryBlock;

    public ControlFlowGraphBuilder(BlockStatementNode node)
    {
        CurrentBlock = new SemanticBlock("entry", node);

        entryBlock = CurrentBlock;
    }

    public void Add(IStatementNode node)
        => CurrentBlock.AddStatement(node);

    public ControlFlowGraph Build()
        => new ControlFlowGraph(entryBlock, CurrentBlock);

    public SemanticBlock CurrentBlock { get; set; }
}
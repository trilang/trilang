using Trilang.Metadata;

namespace Trilang.Semantics.Passes.ControlFlow;

public class ControlFlowGraphMap
{
    private readonly Dictionary<IFunctionMetadata, ControlFlowGraph> functions;

    public ControlFlowGraphMap()
        => functions = [];

    public void Add(IFunctionMetadata nodeMetadata, ControlFlowGraph block)
        => functions.Add(nodeMetadata, block);

    public IReadOnlyDictionary<IFunctionMetadata, ControlFlowGraph> Functions => functions;
}
using Trilang.Semantics.Passes.ControlFlow;

namespace Trilang.Lower;

public record LoweringOptions(IEnumerable<string> Directives, ControlFlowGraphMap ControlFlowGraphs)
{
    public static readonly LoweringOptions Default = new LoweringOptions([], new ControlFlowGraphMap());
}
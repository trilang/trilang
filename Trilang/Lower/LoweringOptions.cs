using Trilang.Semantics.Passes.ControlFlow;

namespace Trilang.Lower;

public record LoweringOptions(ISet<string> Directives, ControlFlowGraphMap ControlFlowGraphs)
{
    public static readonly LoweringOptions Default = new LoweringOptions(new HashSet<string>(), new ControlFlowGraphMap());
}
using Trilang.Semantics.Passes.ControlFlow;

namespace Trilang.Semantics;

public record SemanticAnalysisResult(ControlFlowGraphMap ControlFlowGraphs);
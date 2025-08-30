using Trilang.Metadata;
using Trilang.Semantics.Passes;
using Trilang.Semantics.Passes.ControlFlow;

namespace Trilang.Semantics;

public record SemanticAnalysisResult(
    SymbolTableMap SymbolTableMap,
    ITypeMetadataProvider TypeMetadataProvider,
    ControlFlowGraphMap ControlFlowGraphs);
using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Semantics.Passes;
using Trilang.Semantics.Passes.ControlFlow;

namespace Trilang.Semantics;

public record SemanticAnalysisResult(
    SemanticTree SemanticTree,
    SymbolTableMap SymbolTableMap,
    ITypeMetadataProvider TypeMetadataProvider,
    ControlFlowGraphMap ControlFlowGraphs);
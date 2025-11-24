using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Semantics.Passes;
using Trilang.Semantics.Passes.ControlFlow;

namespace Trilang.Semantics;

public record SemanticAnalysisResult(
    IReadOnlyList<SemanticTree> SemanticTrees,
    SymbolTableMap SymbolTableMap,
    IMetadataProvider MetadataProvider,
    ControlFlowGraphMap ControlFlowGraphs);
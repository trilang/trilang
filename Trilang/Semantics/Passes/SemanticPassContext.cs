using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Passes.ControlFlow;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes;

internal record SemanticPassContext(
    HashSet<string> Directives,
    SemanticDiagnosticReporter Diagnostics,
    RootSymbolTable RootSymbolTable,
    RootMetadataProvider RootMetadataProvider)
{
    public SymbolTableMap? SymbolTableMap { get; set; }

    public MetadataProviderMap? TypeProviderMap { get; set; }

    public ControlFlowGraphMap? ControlFlowGraphs { get; set; }
}
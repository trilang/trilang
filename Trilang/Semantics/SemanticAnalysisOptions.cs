using Trilang.Compilation.Diagnostics;

namespace Trilang.Semantics;

public record SemanticAnalysisOptions(
    IEnumerable<string> Directives,
    SemanticDiagnosticReporter Diagnostics);
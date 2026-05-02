using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;

namespace Trilang.Semantics;

public record SemanticAnalysisOptions(
    ISet<string> Directives,
    DiagnosticCollection Diagnostics,
    CompilationContext CompilationContext);
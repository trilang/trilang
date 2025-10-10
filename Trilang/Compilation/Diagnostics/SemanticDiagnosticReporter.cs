using Trilang.Metadata;

namespace Trilang.Compilation.Diagnostics;

public class SemanticDiagnosticReporter
{
    private readonly DiagnosticCollection diagnostics;

    public SemanticDiagnosticReporter(DiagnosticCollection diagnostics)
        => this.diagnostics = diagnostics;

    public void CyclicTypeAlias(TypeAliasMetadata alias)
        => diagnostics.Error(
            DiagnosticIds.S0001_RcursiveTypeAlias,
            alias.Definition ?? new SourceLocation(default, default),
            $"The cyclic type alias detected: '{alias.Name}'.");
}
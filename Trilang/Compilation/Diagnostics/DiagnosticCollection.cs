namespace Trilang.Compilation.Diagnostics;

public class DiagnosticCollection
{
    private readonly List<Diagnostic> diagnostics;

    public DiagnosticCollection()
        => diagnostics = [];

    public void Add(Diagnostic diagnostic)
        => diagnostics.Add(diagnostic);

    public void Report(string id, DiagnosticSeverity severity, SourceLocation location, string message)
        => Add(new Diagnostic(id, severity, location, message));

    public void Info(string id, SourceLocation location, string message)
        => Report(id, DiagnosticSeverity.Info, location, message);

    public void Warning(string id, SourceLocation location, string message)
        => Report(id, DiagnosticSeverity.Warning, location, message);

    public void Error(string id, SourceLocation location, string message)
        => Report(id, DiagnosticSeverity.Error, location, message);

    public IReadOnlyList<Diagnostic> Diagnostics
        => diagnostics;
}
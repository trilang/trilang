namespace Trilang.Compilation.Diagnostics;

public class DiagnosticCollection
{
    private readonly List<Diagnostic> diagnostics;

    public DiagnosticCollection()
        => diagnostics = [];

    internal LexerDiagnosticReporter ForLexer(SourceFile file)
        => new LexerDiagnosticReporter(this, file);

    internal ParserDiagnosticReporter ForParser(SourceFile file)
        => new ParserDiagnosticReporter(this, file);

    internal SemanticDiagnosticReporter ForSemantic()
        => new SemanticDiagnosticReporter(this);

    internal CompilerDiagnosticReporter ForCompiler()
        => new CompilerDiagnosticReporter(this);

    private void Add(Diagnostic diagnostic)
        => diagnostics.Add(diagnostic);

    private void Report(DiagnosticId id, DiagnosticSeverity severity, SourceLocation? location, string message)
        => Add(new Diagnostic(id, severity, location, message));

    public void Info(DiagnosticId id, SourceLocation? location, string message)
        => Report(id, DiagnosticSeverity.Info, location, message);

    public void Warning(DiagnosticId id, SourceLocation? location, string message)
        => Report(id, DiagnosticSeverity.Warning, location, message);

    public void Error(DiagnosticId id, SourceLocation? location, string message)
        => Report(id, DiagnosticSeverity.Error, location, message);

    public IReadOnlyList<Diagnostic> Diagnostics
        => diagnostics;

    public bool HasErrors
        => diagnostics.Any(d => d.Severity >= DiagnosticSeverity.Error);
}
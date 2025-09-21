namespace Trilang.Compilation.Diagnostics;

public class DiagnosticCollection
{
    private readonly List<Diagnostic> diagnostics;
    private SourceFile file;

    public DiagnosticCollection()
    {
        diagnostics = [];
        file = null!;

        Lexer = new LexerDiagnosticCollection(this);
    }

    public void SwitchFile(SourceFile file)
        => this.file = file;

    public void Add(Diagnostic diagnostic)
        => diagnostics.Add(diagnostic);

    public void Report(string id, DiagnosticSeverity severity, SourceSpan span, string message)
        => Add(new Diagnostic(id, severity, file, span, message));

    public void Info(string id, SourceSpan span, string message)
        => Report(id, DiagnosticSeverity.Info, span, message);

    public void Warning(string id, SourceSpan span, string message)
        => Report(id, DiagnosticSeverity.Warning, span, message);

    public void Error(string id, SourceSpan span, string message)
        => Report(id, DiagnosticSeverity.Error, span, message);

    public IReadOnlyList<Diagnostic> Diagnostics
        => diagnostics;

    public LexerDiagnosticCollection Lexer { get; }
}
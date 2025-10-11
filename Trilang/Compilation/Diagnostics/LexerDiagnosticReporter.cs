namespace Trilang.Compilation.Diagnostics;

public class LexerDiagnosticReporter
{
    private readonly DiagnosticCollection diagnostics;
    private readonly SourceFile file;

    public LexerDiagnosticReporter(DiagnosticCollection diagnostics, SourceFile file)
    {
        this.diagnostics = diagnostics;
        this.file = file;
    }

    public void UnsupportedCharacter(SourceSpan span, char character)
        => diagnostics.Error(
            DiagnosticId.L0001UnsupportedCharacter,
            new SourceLocation(file, span),
            $"Unsupported character '{character}'.");

    public void MissingEndQuoteForStringLiteral(SourceSpan span)
        => diagnostics.Error(
            DiagnosticId.L0002MissingEndQuoteForStringLiteral,
            new SourceLocation(file, span),
            "Missing end quote for string literal.");
}
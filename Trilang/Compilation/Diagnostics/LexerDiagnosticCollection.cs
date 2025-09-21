namespace Trilang.Compilation.Diagnostics;

public class LexerDiagnosticCollection
{
    private readonly DiagnosticCollection diagnostics;

    public LexerDiagnosticCollection(DiagnosticCollection diagnostics)
        => this.diagnostics = diagnostics;

    public void UnsupportedCharacter(SourceSpan span, char character)
        => diagnostics.Error(
            DiagnosticIds.L0001_UnsupportedCharacter,
            span,
            $"Unsupported character '{character}'.");

    public void MissingEndQuoteForStringLiteral(SourceSpan span)
        => diagnostics.Error(
            DiagnosticIds.L0002_MissingEndQuoteForStringLiteral,
            span,
            "Missing end quote for string literal.");
}
namespace Trilang.Compilation.Diagnostics;

internal readonly record struct LexerDiagnosticReporter(DiagnosticCollection Diagnostics, SourceFile File)
{
    public void UnsupportedCharacter(SourceSpan span, char character)
        => Diagnostics.Error(
            DiagnosticId.L0001UnsupportedCharacter,
            new SourceLocation(File, span),
            $"Unsupported character '{character}'.");

    public void MissingEndQuoteForStringLiteral(SourceSpan span)
        => Diagnostics.Error(
            DiagnosticId.L0002MissingEndQuoteForStringLiteral,
            new SourceLocation(File, span),
            "Missing end quote for string literal.");
}
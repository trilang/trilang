using Trilang.Lexing;

namespace Trilang.Compilation.Diagnostics;

public class ParserDiagnosticCollection
{
    private readonly DiagnosticCollection diagnostics;

    public ParserDiagnosticCollection(DiagnosticCollection diagnostics)
        => this.diagnostics = diagnostics;

    // TODO: better error message
    public void UnexpectedToken(Token actual, TokenKind expected)
        => diagnostics.Error(
            DiagnosticIds.P0001_UnexpectedToken,
            actual.SourceSpan,
            $"Unexpected token '{actual.Kind}'. Expected '{expected}'.");
}
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;

namespace Trilang.Parsing;

internal class ParserContext
{
    public ParserContext(
        IReadOnlyList<Token> tokens,
        ParserDiagnosticReporter diagnostics,
        Parser parser)
    {
        Reader = new TokenReader(diagnostics, tokens);
        Diagnostics = diagnostics;
        Parser = parser;
    }

    public TokenReader Reader { get; }

    public ParserDiagnosticReporter Diagnostics { get; }

    public Parser Parser { get; }
}
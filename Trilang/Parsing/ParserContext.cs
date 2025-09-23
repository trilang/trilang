using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;

namespace Trilang.Parsing;

internal class ParserContext
{
    public ParserContext(
        IReadOnlyList<Token> tokens,
        ParserDiagnosticCollection diagnostics,
        Parser parser)
    {
        Reader = new TokenReader(diagnostics, tokens);
        Diagnostics = diagnostics;
        Parser = parser;
    }

    public TokenReader Reader { get; }

    public ParserDiagnosticCollection Diagnostics { get; }

    public Parser Parser { get; }
}
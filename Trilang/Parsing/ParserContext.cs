using Trilang.Lexing;

namespace Trilang.Parsing;

internal class ParserContext
{
    public ParserContext(IReadOnlyList<Token> tokens, Parser parser)
    {
        Parser = parser;
        Reader = new TokenReader(tokens);
    }

    public TokenReader Reader { get; }

    public Parser Parser { get; }
}
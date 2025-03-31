using Trilang.Lexing;

namespace Trilang.Parsing;

internal class ParserContext
{
    public ParserContext(IReadOnlyList<Token> tokens)
    {
        Reader = new TokenReader(tokens);
    }


    public TokenReader Reader { get; }
}
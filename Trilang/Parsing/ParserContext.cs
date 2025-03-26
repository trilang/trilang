using Trilang.Lexing;
using Trilang.Parsing.Nodes;
using Trilang.Symbols;

namespace Trilang.Parsing;

internal class ParserContext
{
    public ParserContext(IReadOnlyList<Token> tokens)
        : this(new TokenReader(tokens), new SymbolTable())
    {
    }

    private ParserContext(TokenReader reader, SymbolTable symbolTable)
    {
        Reader = reader;
        SymbolTable = symbolTable;
    }

    public TResult Scoped<TResult>(Func<ParserContext, TResult> action)
        where TResult : ISyntaxNode?
    {
        var child = new ParserContext(Reader, SymbolTable.CreateChild());

        return action(child);
    }

    public TokenReader Reader { get; }

    public SymbolTable SymbolTable { get; }
}
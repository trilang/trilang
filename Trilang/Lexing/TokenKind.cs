namespace Trilang.Lexing;

public enum TokenKind
{
    EndOfFile,
    NewLine,
    CarriageReturn,
    WhiteSpace,

    Identifier,
    Number,
    String,

    // Symbols
    Colon,
    SemiColon,
    Comma,
    DoubleQuote,
    OpenParenthesis,
    CloseParenthesis,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Equal,
    Exclamation,
    EqualEqual,
    ExclamationEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Ampersand,
    AmpersandAmpersand,
    Pipe,
    PipePipe,

    // Keywords
    Var,
    If,
    Else,
    Function,
    External,
    Return,
}
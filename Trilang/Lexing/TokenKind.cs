namespace Trilang.Lexing;

public enum TokenKind
{
    EndOfFile,
    // NewLine,
    // CarriageReturn,
    // WhiteSpace,

    Identifier,
    Number,
    String,
    Char,

    // Symbols
    Colon,
    SemiColon,
    Comma,
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
    Caret,
    Tilde,

    // Keywords
    Var,
    If,
    Else,
    Function,
    External,
    Return,
    True,
    False,
}
namespace Trilang.Lexing;

public class Token : IEquatable<Token>
{
    private Token(SourceSpan sourceSpan, TokenKind kind, object? value)
    {
        SourceSpan = sourceSpan;
        Kind = kind;
        Value = value;
    }

    public static bool operator ==(Token? left, Token? right)
        => Equals(left, right);

    public static bool operator !=(Token? left, Token? right)
        => !Equals(left, right);

    public bool Equals(Token? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Kind == other.Kind &&
               Equals(Value, other.Value) &&
               SourceSpan.Equals(other.SourceSpan);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null) return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((Token)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine((int)Kind, Value);

    public override string ToString()
        => Value is null ? $"{SourceSpan}\t{Kind}" : $"{SourceSpan}\t{Kind}: {Value}";

    public static Token CreateEof(SourceSpan sourceSpan)
        => new Token(sourceSpan, TokenKind.EndOfFile, null);

    public static Token CreateId(SourceSpan sourceSpan, string value)
        => new Token(sourceSpan, TokenKind.Identifier, value);

    public static Token CreateInteger(SourceSpan sourceSpan, int value)
        => new Token(sourceSpan, TokenKind.Integer, value);

    public static Token CreateFloat(SourceSpan sourceSpan, double value)
        => new Token(sourceSpan, TokenKind.Float, value);

    public static Token CreateChar(SourceSpan sourceSpan, string value)
        => new Token(sourceSpan, TokenKind.Char, value);

    public static Token CreateString(SourceSpan sourceSpan, string value)
        => new Token(sourceSpan, TokenKind.String, value);

    public static Token Create(SourceSpan sourceSpan, TokenKind kind)
        => new Token(sourceSpan, kind, null);

    public bool Is(TokenKind kind)
        => Kind == kind;

    public SourceSpan SourceSpan { get; }

    public TokenKind Kind { get; }

    public object? Value { get; }

    public string Identifier
        => Kind == TokenKind.Identifier && Value is not null
            ? (string)Value
            : throw new InvalidOperationException();

    public int Integer
        => Kind == TokenKind.Integer && Value is not null
            ? (int)Value
            : throw new InvalidOperationException();

    public double Float
        => Kind == TokenKind.Float && Value is not null
            ? (double)Value
            : throw new InvalidOperationException();

    public string String
        => Kind == TokenKind.String && Value is not null
            ? (string)Value
            : throw new InvalidOperationException();

    public string Char
        => Kind == TokenKind.Char && Value is not null
            ? (string)Value
            : throw new InvalidOperationException();
}
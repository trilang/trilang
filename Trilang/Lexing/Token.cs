namespace Trilang.Lexing;

public class Token : IEquatable<Token>
{
    private Token(TokenKind kind, object? value)
    {
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

        return Kind == other.Kind && Equals(Value, other.Value);
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
        => Value is null ? $"{Kind}" : $"{Kind}: {Value}";

    public static Token CreateEof()
        => new Token(TokenKind.EndOfFile, null);

    public static Token CreateId(string value)
        => new Token(TokenKind.Identifier, value);

    public static Token CreateNumber(int value)
        => new Token(TokenKind.Number, value);

    public static Token CreateChar(string value)
        => new Token(TokenKind.Char, value);

    public static Token CreateString(string value)
        => new Token(TokenKind.String, value);

    public static Token Create(TokenKind kind)
        => new Token(kind, null);

    public bool Is(TokenKind kind)
        => Kind == kind;

    public TokenKind Kind { get; }

    public object? Value { get; }

    public string Identifier
        => Kind == TokenKind.Identifier && Value is not null
            ? (string)Value
            : throw new InvalidOperationException();

    public int Number
        => Kind == TokenKind.Number && Value is not null
            ? (int)Value
            : throw new InvalidOperationException();

    public string String
        => Kind == TokenKind.String && Value is not null
            ? (string)Value
            : throw new InvalidOperationException();
}
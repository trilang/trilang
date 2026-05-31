namespace Trilang.Lexing;

internal class Token : IEquatable<Token>
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

    public static Token CreateChar(SourceSpan sourceSpan, string value)
        => new Token(sourceSpan, TokenKind.Char, value);

    public static Token CreateString(SourceSpan sourceSpan, string value)
        => new Token(sourceSpan, TokenKind.String, value);

    public static Token Create(SourceSpan sourceSpan, TokenKind kind)
        => new Token(sourceSpan, kind, null);

    public static Token Create(SourceSpan sourceSpan, TokenKind kind, object? value)
        => new Token(sourceSpan, kind, value);

    public bool Is(TokenKind kind)
        => Kind == kind;

    public SourceSpan SourceSpan { get; }

    public TokenKind Kind { get; }

    public object? Value { get; }
}
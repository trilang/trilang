using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class LiteralExpressionNode : IExpressionNode, IEquatable<LiteralExpressionNode>
{
    public LiteralExpressionNode(LiteralExpressionKind kind, object value)
    {
        Kind = kind;
        Value = value;
    }

    public static LiteralExpressionNode Number(int number)
        => new LiteralExpressionNode(LiteralExpressionKind.Number, number);

    public static LiteralExpressionNode True()
        => new LiteralExpressionNode(LiteralExpressionKind.Boolean, true);

    public static LiteralExpressionNode False()
        => new LiteralExpressionNode(LiteralExpressionKind.Boolean, false);

    public static LiteralExpressionNode String(string str)
        => new LiteralExpressionNode(LiteralExpressionKind.String, str);

    public static LiteralExpressionNode Char(char c)
        => new LiteralExpressionNode(LiteralExpressionKind.Char, c);

    public static bool operator ==(LiteralExpressionNode? left, LiteralExpressionNode? right)
        => Equals(left, right);

    public static bool operator !=(LiteralExpressionNode? left, LiteralExpressionNode? right)
        => !Equals(left, right);

    public bool Equals(LiteralExpressionNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Kind == other.Kind &&
               Value.Equals(other.Value) &&
               Equals(ReturnTypeMetadata, other.ReturnTypeMetadata);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((LiteralExpressionNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine((int)Kind, Value);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitLiteral(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitLiteral(this, context);

    public ISyntaxNode Transform(ITransformer transformer)
        => transformer.TransformLiteral(this);

    public IExpressionNode Clone()
        => new LiteralExpressionNode(Kind, Value)
        {
            SymbolTable = SymbolTable,
            ReturnTypeMetadata = ReturnTypeMetadata,
        };

    public ISyntaxNode? Parent { get; set; }

    public LiteralExpressionKind Kind { get; }

    public object Value { get; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }

    public ISymbolTable? SymbolTable { get; set; }
}
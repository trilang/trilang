using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class NewArrayExpressionNode : IExpressionNode, IEquatable<NewArrayExpressionNode>
{
    public NewArrayExpressionNode(ArrayTypeNode type, IExpressionNode size)
    {
        Type = type;
        Size = size;
    }

    public static bool operator ==(NewArrayExpressionNode? left, NewArrayExpressionNode? right)
        => Equals(left, right);

    public static bool operator !=(NewArrayExpressionNode? left, NewArrayExpressionNode? right)
        => !Equals(left, right);

    public bool Equals(NewArrayExpressionNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Type.Equals(other.Type) &&
               Size.Equals(other.Size) &&
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

        return Equals((NewArrayExpressionNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Type, Size);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitNewArray(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitNewArray(this, context);

    public ISyntaxNode Transform(ITransformer transformer)
        => transformer.TransformNewArray(this);

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }

    public ArrayTypeNode Type { get; }

    public IExpressionNode Size { get; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }
}
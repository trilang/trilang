using System.Diagnostics;
using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class NewObjectExpressionNode : IExpressionNode, IEquatable<NewObjectExpressionNode>
{
    public NewObjectExpressionNode(IInlineTypeNode type, IReadOnlyList<IExpressionNode> parameters)
    {
        Debug.Assert(type is TypeNode or GenericTypeNode);

        Type = type;
        Parameters = parameters;
    }

    public static bool operator ==(NewObjectExpressionNode? left, NewObjectExpressionNode? right)
        => Equals(left, right);

    public static bool operator !=(NewObjectExpressionNode? left, NewObjectExpressionNode? right)
        => !Equals(left, right);

    public bool Equals(NewObjectExpressionNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Type.Equals(other.Type) &&
               Parameters.SequenceEqual(other.Parameters);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((NewObjectExpressionNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Type, Parameters);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitNewObject(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitNewObject(this, context);

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }

    public IInlineTypeNode Type { get; }

    public IReadOnlyList<IExpressionNode> Parameters { get; }

    public ConstructorMetadata? Metadata { get; set; }

    public ITypeMetadata? ReturnTypeMetadata
        => Metadata?.DeclaringType;
}
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class PropertyGetterNode : ISyntaxNode, IEquatable<PropertyGetterNode>
{
    private BlockStatementNode? body;

    public PropertyGetterNode(AccessModifier accessModifier, BlockStatementNode? body)
    {
        AccessModifier = accessModifier;
        Body = body;
    }

    public static bool operator ==(PropertyGetterNode? left, PropertyGetterNode? right)
        => Equals(left, right);

    public static bool operator !=(PropertyGetterNode? left, PropertyGetterNode? right)
        => !Equals(left, right);

    public bool Equals(PropertyGetterNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return AccessModifier == other.AccessModifier &&
               Equals(Body, other.Body);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((PropertyGetterNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine((int)AccessModifier, Body);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitGetter(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitGetter(this, context);

    public ISyntaxNode Transform(ITransformer transformer)
        => transformer.TransformGetter(this);

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }

    public AccessModifier AccessModifier { get; }

    public BlockStatementNode? Body
    {
        get => body;
        set
        {
            body = value;

            if (body is not null)
                body.Parent = this;
        }
    }

    public IReadOnlyList<ParameterNode> Parameters { get; set; } = [];
}
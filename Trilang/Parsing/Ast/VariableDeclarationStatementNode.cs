using Trilang.Metadata;
using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class VariableDeclarationStatementNode : IStatementNode, IEquatable<VariableDeclarationStatementNode>
{
    public VariableDeclarationStatementNode(string name, IInlineTypeNode type, IExpressionNode expression)
    {
        Name = name;
        Type = type;
        Expression = expression;

        Type.Parent = this;
        Expression.Parent = this;
    }

    public static bool operator ==(VariableDeclarationStatementNode? left, VariableDeclarationStatementNode? right)
        => Equals(left, right);

    public static bool operator !=(VariableDeclarationStatementNode? left, VariableDeclarationStatementNode? right)
        => !Equals(left, right);

    public bool Equals(VariableDeclarationStatementNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name &&
               Type.Equals(other.Type) &&
               Expression.Equals(other.Expression) &&
               Equals(Metadata, other.Metadata);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((VariableDeclarationStatementNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, Type, Expression);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitVariable(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitVariable(this, context);

    public ISyntaxNode Transform(ITransformer transformer)
        => transformer.TransformVariable(this);

    public ISyntaxNode? Parent { get; set; }

    public string Name { get; }

    public IInlineTypeNode Type { get; }

    public IExpressionNode Expression { get; }

    public VariableMetadata? Metadata { get; set; }
}
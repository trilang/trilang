using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class VariableDeclaration : IStatement
{
    public VariableDeclaration(string name, IInlineType type, IExpression expression)
    {
        Name = name;
        Type = type;
        Expression = expression;

        Type.Parent = this;
        Expression.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitVariable(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitVariable(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformVariable(this);

    public ISemanticNode? Parent { get; set; }

    public string Name { get; }

    public IInlineType Type { get; }

    public IExpression Expression { get; }

    public VariableMetadata? Metadata { get; set; }
}
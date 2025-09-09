using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class Parameter : ISemanticNode
{
    public Parameter(string name, IInlineType type)
    {
        Name = name;
        Type = type;

        Type.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitParameter(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitParameter(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformParameter(this);

    public ISemanticNode? Parent { get; set; }

    public string Name { get; }

    public IInlineType Type { get; }

    public ParameterMetadata? Metadata { get; set; }
}
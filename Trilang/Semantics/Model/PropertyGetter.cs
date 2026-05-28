using Trilang.Metadata;

namespace Trilang.Semantics.Model;

// TODO: merge with PropertySetter
public class PropertyGetter : ISemanticNode
{
    public PropertyGetter(SourceSpan? sourceSpan, AccessModifier accessModifier, BlockStatement? body)
    {
        SourceSpan = sourceSpan;
        AccessModifier = accessModifier;
        Body = body;
        Body?.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitGetter(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitGetter(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformGetter(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public AccessModifier AccessModifier { get; }

    public BlockStatement? Body
    {
        get;
        set
        {
            field = value;
            field?.Parent = this;
        }
    }

    public MethodMetadata? Metadata { get; set; }
}
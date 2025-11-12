using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class PropertySetter : ISemanticNode
{
    private BlockStatement? body;

    public PropertySetter(SourceSpan? sourceSpan, AccessModifier accessModifier, BlockStatement? body)
    {
        SourceSpan = sourceSpan;
        AccessModifier = accessModifier;
        Body = body;

        Body?.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitSetter(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitSetter(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformSetter(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public AccessModifier AccessModifier { get; }

    public BlockStatement? Body
    {
        get => body;
        set
        {
            body = value;

            if (body is not null)
                body.Parent = this;
        }
    }

    public MethodMetadata? Metadata { get; set; }
}
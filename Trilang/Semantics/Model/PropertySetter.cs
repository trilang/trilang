using Trilang.Metadata;
using Trilang.Symbols;

namespace Trilang.Semantics.Model;

public class PropertySetter : ISemanticNode
{
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
        get;
        set
        {
            field = value;
            field?.Parent = this;
        }
    }

    public MethodMetadata? Metadata { get; set; }

    public IdSymbol? FieldSymbol { get; set; }

    public IdSymbol? ValueSymbol { get; set; }
}
using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class NullExpression : IExpression
{
    private readonly BuiltInTypes builtInTypes;

    public NullExpression(SourceSpan? sourceSpan, BuiltInTypes builtInTypes)
    {
        this.builtInTypes = builtInTypes;
        SourceSpan = sourceSpan;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitNull(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitNull(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformNull(this);

    public IExpression Clone()
        => new NullExpression(SourceSpan, builtInTypes);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public ITypeMetadata ReturnTypeMetadata
        => builtInTypes.Null;
}
using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class NewArrayExpression : IExpression
{
    public NewArrayExpression(SourceSpan? sourceSpan, ArrayType type, IExpression size)
    {
        SourceSpan = sourceSpan;
        Type = type;
        Size = size;

        Type.Parent = this;
        Size.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitNewArray(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitNewArray(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformNewArray(this);

    public IExpression Clone()
        => new NewArrayExpression(SourceSpan, (ArrayType)Type.Clone(), Size.Clone())
        {
            ReturnTypeMetadata = ReturnTypeMetadata,
        };

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public ArrayType Type { get; }

    public IExpression Size { get; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }
}
using Trilang.Metadata;
using Trilang.Semantics.Providers;
using Trilang.Symbols;

namespace Trilang.Semantics.Model;

public class CastExpression : IExpression
{
    public CastExpression(SourceSpan? sourceSpan, IInlineType type, IExpression expression)
    {
        SourceSpan = sourceSpan;
        Type = type;
        Expression = expression;

        Type.Parent = this;
        Expression.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitCast(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitCast(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformCast(this);

    public IExpression Clone()
        => new CastExpression(SourceSpan, Type.Clone(), Expression.Clone())
        {
            SymbolTable = SymbolTable,
            MetadataProvider = MetadataProvider,
        };

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public SymbolTable? SymbolTable { get; set; }

    public IMetadataProvider? MetadataProvider { get; set; }

    public ITypeMetadata? ReturnTypeMetadata
        => Type.Metadata;

    public IInlineType Type { get; }

    public IExpression Expression { get; }
}
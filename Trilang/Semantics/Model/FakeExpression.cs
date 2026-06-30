using Trilang.Metadata;
using Trilang.Semantics.Providers;
using Trilang.Symbols;

namespace Trilang.Semantics.Model;

public class FakeExpression : IExpression
{
    public FakeExpression(SourceSpan? sourceSpan)
        => SourceSpan = sourceSpan;

    public void Accept(IVisitor visitor)
        => visitor.VisitFakeExpression(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitFakeExpression(this, context);

    public IExpression Clone()
        => new FakeExpression(SourceSpan)
        {
            SymbolTable = SymbolTable,
            MetadataProvider = MetadataProvider,
        };

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformFakeExpression(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public SymbolTable? SymbolTable { get; set; }

    public IMetadataProvider? MetadataProvider { get; set; }

    public ITypeMetadata ReturnTypeMetadata => TypeMetadata.InvalidType;
}
using Trilang.Metadata;
using Trilang.Semantics.Providers;
using Trilang.Symbols;

namespace Trilang.Semantics.Model;

public class ArrayAccessExpression : IAccessExpression
{
    public ArrayAccessExpression(SourceSpan? sourceSpan, IAccessExpression member, IExpression index)
    {
        SourceSpan = sourceSpan;
        Member = member;
        Index = index;

        Member.Parent = this;
        Index.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitArrayAccess(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitArrayAccess(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformArrayAccess(this);

    public IExpression Clone()
        => new ArrayAccessExpression(SourceSpan, (IAccessExpression)Member.Clone(), Index.Clone())
        {
            ReturnTypeMetadata = ReturnTypeMetadata,
            SymbolTable = SymbolTable,
            MetadataProvider = MetadataProvider,
        };

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public SymbolTable? SymbolTable { get; set; }

    public IMetadataProvider? MetadataProvider { get; set; }

    public IAccessExpression Member { get; }

    public IExpression Index { get; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }

    public IMetadata? Reference { get; set; }
}
using Trilang.Metadata;
using Trilang.Semantics.Providers;
using Trilang.Symbols;

namespace Trilang.Semantics.Model;

public class NewObjectExpression : IExpression
{
    public NewObjectExpression(SourceSpan? sourceSpan, IAccessExpression member)
    {
        SourceSpan = sourceSpan;
        Member = member;
        Member.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitNewObject(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitNewObject(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformNewObject(this);

    public IExpression Clone()
        => new NewObjectExpression(SourceSpan, (IAccessExpression)Member.Clone())
        {
            Metadata = Metadata,
            SymbolTable = SymbolTable,
            MetadataProvider = MetadataProvider,
        };

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public SymbolTable? SymbolTable { get; set; }

    public IMetadataProvider? MetadataProvider { get; set; }

    public IAccessExpression Member { get; }

    public ConstructorMetadata? Metadata { get; set; }

    public ITypeMetadata? ReturnTypeMetadata { get; set; }
}
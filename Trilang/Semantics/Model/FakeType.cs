using Trilang.Metadata;
using Trilang.Semantics.Providers;
using Trilang.Symbols;

namespace Trilang.Semantics.Model;

public class FakeType : IInlineType
{
    public FakeType(SourceSpan? sourceSpan, string name)
    {
        SourceSpan = sourceSpan;
        Name = name;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitFakeType(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitFakeType(this, context);

    public IInlineType Clone()
        => new FakeType(SourceSpan, Name)
        {
            SymbolTable = SymbolTable,
            MetadataProvider = MetadataProvider,
        };

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformFakeType(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public SymbolTable? SymbolTable { get; set; }

    public IMetadataProvider? MetadataProvider { get; set; }

    public string Name { get; }

    public ITypeMetadata? Metadata { get; set; }
}
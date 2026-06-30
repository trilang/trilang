using Trilang.Metadata;
using Trilang.Semantics.Providers;
using Trilang.Symbols;

namespace Trilang.Semantics.Model;

public class ArrayType : IInlineType
{
    public ArrayType(SourceSpan? sourceSpan, IInlineType elementType)
    {
        SourceSpan = sourceSpan;
        Name = $"{elementType.Name}[]";
        ElementType = elementType;
        ElementType.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitArrayType(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitArrayType(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformArrayType(this);

    public IInlineType Clone()
        => new ArrayType(SourceSpan, ElementType.Clone())
        {
            Metadata = Metadata,
            SymbolTable = SymbolTable,
            MetadataProvider = MetadataProvider,
        };

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public SymbolTable? SymbolTable { get; set; }

    public IMetadataProvider? MetadataProvider { get; set; }

    public string Name { get; }

    public IInlineType ElementType { get; }

    public ITypeMetadata? Metadata { get; set; }
}
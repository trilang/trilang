using Trilang.Metadata;
using Trilang.Semantics.Providers;
using Trilang.Symbols;

namespace Trilang.Semantics.Model;

public class TypeRef : IInlineType
{
    public TypeRef(SourceSpan? sourceSpan, string? package, IReadOnlyList<string> parts)
    {
        if (parts.Count == 0)
            throw new ArgumentException("TypeRef must have at least one part", nameof(parts));

        SourceSpan = sourceSpan;
        Package = package;
        Parts = parts;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitTypeRef(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitTypeNode(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformTypeNode(this);

    public IInlineType Clone()
        => new TypeRef(SourceSpan, Package, Parts)
        {
            Metadata = Metadata,
            SymbolTable = SymbolTable,
            MetadataProvider = MetadataProvider,
        };

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public SymbolTable? SymbolTable { get; set; }

    public IMetadataProvider? MetadataProvider { get; set; }

    public string? Package { get; }

    public IReadOnlyList<string> Parts { get; }

    public string Name
        => Parts[^1];

    public ITypeMetadata? Metadata { get; set; }
}
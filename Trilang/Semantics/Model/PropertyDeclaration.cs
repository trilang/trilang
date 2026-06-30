using Trilang.Metadata;
using Trilang.Semantics.Providers;
using Trilang.Symbols;

namespace Trilang.Semantics.Model;

public class PropertyDeclaration : ISemanticNode
{
    public PropertyDeclaration(
        SourceSpan? sourceSpan,
        string name,
        IInlineType type,
        PropertyGetter? getter,
        PropertySetter? setter)
    {
        SourceSpan = sourceSpan;
        Name = name;
        Type = type;
        Getter = getter;
        Setter = setter;

        Type.Parent = this;
        Getter?.Parent = this;
        Setter?.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitProperty(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitProperty(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformProperty(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public SymbolTable? SymbolTable { get; set; }

    public IMetadataProvider? MetadataProvider { get; set; }

    public string Name { get; }

    public IInlineType Type { get; }

    public PropertyGetter? Getter
    {
        get;
        set
        {
            field = value;
            field?.Parent = this;
        }
    }

    public PropertySetter? Setter
    {
        get;
        set
        {
            field = value;
            field?.Parent = this;
        }
    }

    public PropertyMetadata? Metadata { get; set; }

    public IdSymbol? Symbol { get; set; }
}
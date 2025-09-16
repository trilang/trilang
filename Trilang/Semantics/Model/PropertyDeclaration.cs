using Trilang.Metadata;

namespace Trilang.Semantics.Model;

// TODO: don't generate backing field if it's not needed
public class PropertyDeclaration : ISemanticNode
{
    private PropertyGetter? getter;
    private PropertySetter? setter;

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

        if (Getter is not null)
            Getter.Parent = this;

        if (Setter is not null)
            Setter.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitProperty(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitProperty(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformProperty(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public string Name { get; }

    public IInlineType Type { get; }

    public PropertyGetter? Getter
    {
        get => getter;
        set
        {
            getter = value;

            if (getter is not null)
                getter.Parent = this;
        }
    }

    public PropertySetter? Setter
    {
        get => setter;
        set
        {
            setter = value;

            if (setter is not null)
                setter.Parent = this;
        }
    }

    public PropertyMetadata? Metadata { get; set; }
}
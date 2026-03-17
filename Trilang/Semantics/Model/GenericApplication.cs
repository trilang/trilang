using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class GenericApplication : IInlineType
{
    public GenericApplication(
        SourceSpan? sourceSpan,
        TypeRef type,
        IReadOnlyList<IInlineType> typeArguments)
    {
        SourceSpan = sourceSpan;
        Type = type;
        TypeArguments = typeArguments;
        Name = $"{type.Name}<{string.Join(", ", typeArguments.Select(t => t.Name))}>";

        foreach (var typeArgument in TypeArguments)
            typeArgument.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitGenericType(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitGenericType(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformGenericType(this);

    public IInlineType Clone()
        => new GenericApplication(SourceSpan, Type, TypeArguments.Select(t => t.Clone()).ToArray())
        {
            Metadata = Metadata,
        };

    public string GetOpenGenericName()
        => $"{Type.Name}<{new string(',', TypeArguments.Count - 1)}>";

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public TypeRef Type { get; }

    public IReadOnlyList<IInlineType> TypeArguments { get; }

    public string Name { get; }

    public ITypeMetadata? Metadata { get; set; }
}
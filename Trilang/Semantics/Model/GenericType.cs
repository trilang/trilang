using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class GenericType : IInlineType
{
    public GenericType(string prefixName, IReadOnlyList<IInlineType> typeArguments)
    {
        PrefixName = prefixName;
        TypeArguments = typeArguments;
        Name = $"{prefixName}<{string.Join(", ", typeArguments.Select(t => t.Name))}>";

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
        => new GenericType(PrefixName, TypeArguments.Select(t => t.Clone()).ToArray())
        {
            Metadata = Metadata,
        };

    public string GetOpenGenericName()
        => $"{PrefixName}<{new string(',', TypeArguments.Count - 1)}>";

    public ISemanticNode? Parent { get; set; }

    public string PrefixName { get; }

    public IReadOnlyList<IInlineType> TypeArguments { get; }

    public string Name { get; }

    public ITypeMetadata? Metadata { get; set; }
}
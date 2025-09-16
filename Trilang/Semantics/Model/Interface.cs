using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class Interface : IInlineType
{
    public Interface(
        SourceSpan? sourceSpan,
        IReadOnlyList<InterfaceProperty> properties,
        IReadOnlyList<InterfaceMethod> methods)
    {
        SourceSpan = sourceSpan;
        Properties = properties;
        Methods = methods;

        foreach (var property in properties)
            property.Parent = this;

        foreach (var method in methods)
            method.Parent = this;

        var propertyNames = properties.Select(f => $"{f.Name}: {f.Type.Name};");
        var methodNames = methods.Select(m => $"{m.Name}({string.Join(", ", m.ParameterTypes.Select(pt => pt.Name))}): {m.ReturnType.Name};");

        var combinedSignatures = propertyNames.Concat(methodNames).ToList();
        Name = combinedSignatures.Any()
            ? $"{{ {string.Join(" ", combinedSignatures)} }}"
            : "{ }";
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitInterface(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitInterface(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformInterface(this);

    public IInlineType Clone()
        => new Interface(
            SourceSpan,
            Properties.Select(x => x.Clone()).ToArray(),
            Methods.Select(x => x.Clone()).ToArray()
        )
        {
            Metadata = Metadata,
        };

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public string Name { get; }

    public IReadOnlyList<InterfaceProperty> Properties { get; }

    public IReadOnlyList<InterfaceMethod> Methods { get; }

    public ITypeMetadata? Metadata { get; set; }
}
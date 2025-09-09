using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class InterfaceMethod : ISemanticNode
{
    public InterfaceMethod(
        string name,
        IReadOnlyList<IInlineType> parameterTypes,
        IInlineType returnType)
    {
        Name = name;
        ParameterTypes = parameterTypes;
        ReturnType = returnType;

        foreach (var parameter in parameterTypes)
            parameter.Parent = this;

        ReturnType.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitInterfaceMethod(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitInterfaceMethod(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformInterfaceMethod(this);

    public InterfaceMethod Clone()
        => new InterfaceMethod(Name, ParameterTypes.Select(t => t.Clone()).ToArray(), ReturnType.Clone())
        {
            Metadata = Metadata,
        };

    public ISemanticNode? Parent { get; set; }

    public string Name { get; }

    public IReadOnlyList<IInlineType> ParameterTypes { get; }

    public IInlineType ReturnType { get; }

    public InterfaceMethodMetadata? Metadata { get; set; }
}
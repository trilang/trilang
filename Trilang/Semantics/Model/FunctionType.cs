using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class FunctionType : IInlineType
{
    public FunctionType(
        SourceSpan? sourceSpan,
        IReadOnlyList<IInlineType> parameterTypes,
        IInlineType returnType)
    {
        SourceSpan = sourceSpan;
        ParameterTypes = parameterTypes;
        ReturnType = returnType;

        foreach (var parameter in parameterTypes)
            parameter.Parent = this;

        returnType.Parent = this;

        var parameters = string.Join(", ", parameterTypes.Select(p => p.Name));
        Name = $"({parameters}) => {returnType.Name}";
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitFunctionType(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitFunctionType(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformFunctionType(this);

    public IInlineType Clone()
        => new FunctionType(SourceSpan, ParameterTypes.Select(t => t.Clone()).ToArray(), ReturnType.Clone())
        {
            Metadata = Metadata,
        };

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public string Name { get; }

    public IReadOnlyList<IInlineType> ParameterTypes { get; }

    public IInlineType ReturnType { get; }

    public ITypeMetadata? Metadata { get; set; }
}
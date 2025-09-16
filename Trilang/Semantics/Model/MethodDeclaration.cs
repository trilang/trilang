using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class MethodDeclaration : ISemanticNode
{
    public MethodDeclaration(
        SourceSpan? sourceSpan,
        AccessModifier accessModifier,
        bool isStatic,
        string name,
        IReadOnlyList<Parameter> parameters,
        IInlineType returnType,
        BlockStatement body)
    {
        SourceSpan = sourceSpan;
        AccessModifier = accessModifier;
        IsStatic = isStatic;
        Name = name;
        Parameters = parameters;
        ReturnType = returnType;
        Body = body;

        foreach (var parameter in parameters)
            parameter.Parent = this;

        ReturnType.Parent = this;
        Body.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitMethod(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitMethod(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformMethod(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public AccessModifier AccessModifier { get; }

    public bool IsStatic { get; }

    public string Name { get; }

    public IReadOnlyList<Parameter> Parameters { get; set; }

    public IInlineType ReturnType { get; }

    public BlockStatement Body { get; }

    public MethodMetadata? Metadata { get; set; }
}
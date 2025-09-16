using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class ConstructorDeclaration : ISemanticNode
{
    public ConstructorDeclaration(
        SourceSpan? sourceSpan,
        AccessModifier accessModifier,
        IReadOnlyList<Parameter> parameters,
        BlockStatement body)
    {
        SourceSpan = sourceSpan;
        AccessModifier = accessModifier;
        Parameters = parameters;
        Body = body;

        foreach (var parameter in parameters)
            parameter.Parent = this;

        Body.Parent = this;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitConstructor(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitConstructor(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformConstructor(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public AccessModifier AccessModifier { get; }

    public IReadOnlyList<Parameter> Parameters { get; set; }

    public BlockStatement Body { get; }

    public ConstructorMetadata? Metadata { get; set; }
}
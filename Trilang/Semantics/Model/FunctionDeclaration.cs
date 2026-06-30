using Trilang.Metadata;
using Trilang.Semantics.Providers;
using Trilang.Symbols;

namespace Trilang.Semantics.Model;

public class FunctionDeclaration : IDeclaration
{
    public FunctionDeclaration(
        SourceSpan? sourceSpan,
        AccessModifier accessModifier,
        string name,
        IReadOnlyList<Parameter> parameters,
        IInlineType returnType,
        BlockStatement body)
    {
        SourceSpan = sourceSpan;
        AccessModifier = accessModifier;
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
        => visitor.VisitFunction(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitFunction(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformFunction(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public SymbolTable? SymbolTable { get; set; }

    public IMetadataProvider? MetadataProvider { get; set; }

    public AccessModifier AccessModifier { get; }

    public string Name { get; }

    public IReadOnlyList<Parameter> Parameters { get; }

    public IInlineType ReturnType { get; }

    public BlockStatement Body { get; }

    public FunctionMetadata? Metadata { get; set; }
}
using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class TypeAliasDeclaration : IDeclaration
{
    public TypeAliasDeclaration(
        SourceSpan? sourceSpan,
        AccessModifier accessModifier,
        string name,
        IReadOnlyList<Type> genericArguments,
        IInlineType type)
    {
        SourceSpan = sourceSpan;
        AccessModifier = accessModifier;
        Name = name;
        GenericArguments = genericArguments;
        Type = type;

        foreach (var genericArgument in genericArguments)
            genericArgument.Parent = this;

        Type.Parent = this;

        FullName = genericArguments.Count > 0
            ? $"{Name}<{new string(',', genericArguments.Count - 1)}>"
            : Name;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitTypeAlias(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitTypeAlias(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformTypeAlias(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public AccessModifier AccessModifier { get; }

    public string Name { get; }

    public string FullName { get; }

    public IReadOnlyList<Type> GenericArguments { get; }

    public IInlineType Type { get; }

    public ITypeMetadata? Metadata { get; set; }
}
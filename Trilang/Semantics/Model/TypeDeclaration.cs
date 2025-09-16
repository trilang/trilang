using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public class TypeDeclaration : IDeclaration
{
    public TypeDeclaration(
        SourceSpan? sourceSpan,
        AccessModifier accessModifier,
        string name,
        IReadOnlyList<Type> genericArguments,
        IReadOnlyList<Type> interfaces,
        IReadOnlyList<PropertyDeclaration> properties,
        IReadOnlyList<ConstructorDeclaration> constructors,
        IReadOnlyList<MethodDeclaration> methods)
    {
        SourceSpan = sourceSpan;
        AccessModifier = accessModifier;
        Name = name;
        GenericArguments = genericArguments;
        Interfaces = interfaces;
        Properties = properties;
        Constructors = constructors;
        Methods = methods;

        foreach (var genericArgument in genericArguments)
            genericArgument.Parent = this;

        foreach (var interfaceNode in interfaces)
            interfaceNode.Parent = this;

        foreach (var property in properties)
            property.Parent = this;

        foreach (var constructor in constructors)
            constructor.Parent = this;

        foreach (var method in methods)
            method.Parent = this;

        FullName = genericArguments.Count > 0
            ? $"{Name}<{new string(',', genericArguments.Count - 1)}>"
            : Name;
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitType(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitType(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformType(this);

    public ISemanticNode? Parent { get; set; }

    public SourceSpan? SourceSpan { get; }

    public AccessModifier AccessModifier { get; }

    public string Name { get; }

    public string FullName { get; }

    public IReadOnlyList<Type> GenericArguments { get; }

    public IReadOnlyList<Type> Interfaces { get; }

    public IReadOnlyList<PropertyDeclaration> Properties { get; }

    public IReadOnlyList<ConstructorDeclaration> Constructors { get; }

    public IReadOnlyList<MethodDeclaration> Methods { get; }

    public TypeMetadata? Metadata { get; set; }
}
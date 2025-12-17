using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class TypeDeclarationNode : IDeclarationNode, IHasGenericArguments
{
    public TypeDeclarationNode(
        SourceSpan sourceSpan,
        AccessModifier accessModifier,
        string name,
        IReadOnlyList<IInlineTypeNode> genericArguments,
        IReadOnlyList<TypeRefNode> interfaces,
        IReadOnlyList<PropertyDeclarationNode> properties,
        IReadOnlyList<ConstructorDeclarationNode> constructors,
        IReadOnlyList<MethodDeclarationNode> methods)
    {
        SourceSpan = sourceSpan;
        AccessModifier = accessModifier;
        Name = name;
        GenericArguments = genericArguments;
        Interfaces = interfaces;
        Properties = properties;
        Constructors = constructors;
        Methods = methods;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitType(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformType(this);

    public SourceSpan SourceSpan { get; }

    public AccessModifier AccessModifier { get; }

    public string Name { get; }

    public IReadOnlyList<IInlineTypeNode> GenericArguments { get; }

    public IReadOnlyList<TypeRefNode> Interfaces { get; }

    public IReadOnlyList<PropertyDeclarationNode> Properties { get; }

    public IReadOnlyList<ConstructorDeclarationNode> Constructors { get; }

    public IReadOnlyList<MethodDeclarationNode> Methods { get; }
}
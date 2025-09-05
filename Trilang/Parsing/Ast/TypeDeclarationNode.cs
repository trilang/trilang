using Trilang.Metadata;
using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class TypeDeclarationNode :
    IDeclarationNode,
    IHasGenericArguments,
    IEquatable<TypeDeclarationNode>
{
    public TypeDeclarationNode(
        AccessModifier accessModifier,
        string name,
        IReadOnlyList<TypeNode> genericArguments,
        IReadOnlyList<TypeNode> interfaces,
        IReadOnlyList<PropertyDeclarationNode> properties,
        IReadOnlyList<ConstructorDeclarationNode> constructors,
        IReadOnlyList<MethodDeclarationNode> methods)
    {
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

    public static bool operator ==(TypeDeclarationNode? left, TypeDeclarationNode? right)
        => Equals(left, right);

    public static bool operator !=(TypeDeclarationNode? left, TypeDeclarationNode? right)
        => !Equals(left, right);

    public bool Equals(TypeDeclarationNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return AccessModifier == other.AccessModifier &&
               Name == other.Name &&
               GenericArguments.SequenceEqual(other.GenericArguments) &&
               Interfaces.SequenceEqual(other.Interfaces) &&
               Properties.SequenceEqual(other.Properties) &&
               Constructors.SequenceEqual(other.Constructors) &&
               Methods.SequenceEqual(other.Methods) &&
               Equals(Metadata, other.Metadata);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((TypeDeclarationNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(
            (int)AccessModifier,
            Name,
            GenericArguments,
            Interfaces,
            Properties,
            Constructors,
            Methods);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitType(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitType(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformType(this);

    public ISyntaxNode? Parent { get; set; }

    public AccessModifier AccessModifier { get; }

    public string Name { get; }

    public string FullName { get; }

    public IReadOnlyList<TypeNode> GenericArguments { get; }

    public IReadOnlyList<TypeNode> Interfaces { get; }

    public IReadOnlyList<PropertyDeclarationNode> Properties { get; }

    public IReadOnlyList<ConstructorDeclarationNode> Constructors { get; }

    public IReadOnlyList<MethodDeclarationNode> Methods { get; }

    public TypeMetadata? Metadata { get; set; }
}
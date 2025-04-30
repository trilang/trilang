using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class TypeDeclarationNode : IDeclarationNode, IEquatable<TypeDeclarationNode>
{
    public TypeDeclarationNode(
        AccessModifier accessModifier,
        string name,
        IReadOnlyList<TypeNode> interfaces,
        IReadOnlyList<FieldDeclarationNode> fields,
        IReadOnlyList<ConstructorDeclarationNode> constructors,
        IReadOnlyList<MethodDeclarationNode> methods)
    {
        AccessModifier = accessModifier;
        Name = name;
        Interfaces = interfaces;
        Fields = fields;
        Constructors = constructors;
        Methods = methods;

        foreach (var interfaceNode in interfaces)
            interfaceNode.Parent = this;

        foreach (var field in fields)
            field.Parent = this;

        foreach (var constructor in constructors)
            constructor.Parent = this;

        foreach (var method in methods)
            method.Parent = this;
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
               Interfaces.SequenceEqual(other.Interfaces) &&
               Fields.SequenceEqual(other.Fields) &&
               Constructors.SequenceEqual(other.Constructors) &&
               Methods.SequenceEqual(other.Methods);
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
        => HashCode.Combine((int)AccessModifier, Name, Interfaces, Fields, Constructors, Methods);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.Visit(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.Visit(this, context);

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }

    public AccessModifier AccessModifier { get; }

    public string Name { get; }

    public IReadOnlyList<TypeNode> Interfaces { get; }

    public IReadOnlyList<FieldDeclarationNode> Fields { get; }

    public IReadOnlyList<ConstructorDeclarationNode> Constructors { get; }

    public IReadOnlyList<MethodDeclarationNode> Methods { get; }

    public TypeMetadata? Metadata { get; set; }
}
using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class FunctionTypeDeclarationNode : IDeclarationNode, IEquatable<FunctionTypeDeclarationNode>
{
    public FunctionTypeDeclarationNode(
        AccessModifier accessModifier,
        string name,
        IReadOnlyList<TypeNode> parameterTypes,
        TypeNode returnType)
    {
        AccessModifier = accessModifier;
        Name = name;
        ParameterTypes = parameterTypes;
        ReturnType = returnType;
    }

    public static bool operator ==(FunctionTypeDeclarationNode? left, FunctionTypeDeclarationNode? right)
        => Equals(left, right);

    public static bool operator !=(FunctionTypeDeclarationNode? left, FunctionTypeDeclarationNode? right)
        => !Equals(left, right);

    public bool Equals(FunctionTypeDeclarationNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return AccessModifier == other.AccessModifier &&
               Name == other.Name &&
               ParameterTypes.SequenceEqual(other.ParameterTypes) &&
               ReturnType.Equals(other.ReturnType) &&
               Equals(SymbolTable, other.SymbolTable);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((FunctionTypeDeclarationNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, ParameterTypes, ReturnType);

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

    public IReadOnlyList<TypeNode> ParameterTypes { get; }

    public TypeNode ReturnType { get; }

    public FunctionTypeMetadata? Metadata { get; set; }
}
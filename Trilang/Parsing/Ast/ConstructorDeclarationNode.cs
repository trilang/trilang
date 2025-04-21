using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class ConstructorDeclarationNode : ISyntaxNode, IEquatable<ConstructorDeclarationNode>
{
    public ConstructorDeclarationNode(
        AccessModifier accessModifier,
        IReadOnlyList<ParameterNode> parameters,
        BlockStatementNode body)
    {
        AccessModifier = accessModifier;
        Parameters = parameters;
        Body = body;

        foreach (var parameter in parameters)
            parameter.Parent = this;

        Body.Parent = this;
    }

    public static bool operator ==(ConstructorDeclarationNode? left, ConstructorDeclarationNode? right)
        => Equals(left, right);

    public static bool operator !=(ConstructorDeclarationNode? left, ConstructorDeclarationNode? right)
        => !Equals(left, right);

    public bool Equals(ConstructorDeclarationNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return AccessModifier == other.AccessModifier &&
               Parameters.SequenceEqual(other.Parameters) &&
               Body.Equals(other.Body);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((ConstructorDeclarationNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine((int)AccessModifier, Parameters, Body);

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

    public IReadOnlyList<ParameterNode> Parameters { get; }

    public BlockStatementNode Body { get; }

    public ConstructorMetadata? Metadata { get; set; }
}
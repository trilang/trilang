using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class MethodDeclarationNode : ISyntaxNode, IEquatable<MethodDeclarationNode>
{
    public MethodDeclarationNode(
        AccessModifier accessModifier,
        bool isStatic,
        string name,
        IReadOnlyList<ParameterNode> parameters,
        IInlineTypeNode returnType,
        BlockStatementNode body)
    {
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

    public static bool operator ==(MethodDeclarationNode? left, MethodDeclarationNode? right)
        => Equals(left, right);

    public static bool operator !=(MethodDeclarationNode? left, MethodDeclarationNode? right)
        => !Equals(left, right);

    public bool Equals(MethodDeclarationNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return AccessModifier == other.AccessModifier &&
               IsStatic == other.IsStatic &&
               Name == other.Name &&
               Parameters.SequenceEqual(other.Parameters) &&
               ReturnType.Equals(other.ReturnType) &&
               Body.Equals(other.Body) &&
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

        return Equals((MethodDeclarationNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine((int)AccessModifier, Name, Parameters, ReturnType, Body);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitMethod(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitMethod(this, context);

    public ISyntaxNode Transform(ITransformer transformer)
        => transformer.TransformMethod(this);

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }

    public AccessModifier AccessModifier { get; }

    public bool IsStatic { get; }

    public string Name { get; }

    public IReadOnlyList<ParameterNode> Parameters { get; set; }

    public IInlineTypeNode ReturnType { get; }

    public BlockStatementNode Body { get; }

    public MethodMetadata? Metadata { get; set; }
}
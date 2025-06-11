using System.Diagnostics.CodeAnalysis;
using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class FunctionDeclarationNode : IDeclarationNode, IEquatable<FunctionDeclarationNode>
{
    public FunctionDeclarationNode(
        string name,
        IReadOnlyList<ParameterNode> parameters,
        IInlineTypeNode returnType,
        BlockStatementNode? body)
    {
        Name = name;
        Parameters = parameters;
        ReturnType = returnType;
        Body = body;

        foreach (var parameter in parameters)
            parameter.Parent = this;

        ReturnType.Parent = this;

        if (Body is not null)
            Body.Parent = this;
    }

    public static FunctionDeclarationNode Create(
        string name,
        IReadOnlyList<ParameterNode> parameters,
        IInlineTypeNode returnType,
        BlockStatementNode body)
        => new FunctionDeclarationNode(name, parameters, returnType, body);

    public static bool operator ==(FunctionDeclarationNode? left, FunctionDeclarationNode? right)
        => Equals(left, right);

    public static bool operator !=(FunctionDeclarationNode? left, FunctionDeclarationNode? right)
        => !Equals(left, right);

    public bool Equals(FunctionDeclarationNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name &&
               Parameters.SequenceEqual(other.Parameters) &&
               ReturnType.Equals(other.ReturnType) &&
               Equals(Body, other.Body);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((FunctionDeclarationNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, Parameters, ReturnType, Body);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitFunction(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitFunction(this, context);

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }

    public string Name { get; }

    public IReadOnlyList<ParameterNode> Parameters { get; }

    public IInlineTypeNode ReturnType { get; }

    public FunctionMetadata? Metadata { get; set; }

    public BlockStatementNode? Body { get; }
}
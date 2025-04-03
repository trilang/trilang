using System.Diagnostics.CodeAnalysis;
using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class FunctionDeclarationNode : IStatementNode, IEquatable<FunctionDeclarationNode>
{
    private FunctionDeclarationNode(
        bool isExternal,
        string name,
        IReadOnlyList<FunctionParameterNode> parameters,
        TypeNode returnType,
        BlockStatementNode? body)
    {
        IsExternal = isExternal;
        Name = name;
        Parameters = parameters;
        ReturnType = returnType;
        Body = body;

        foreach (var parameter in parameters)
            parameter.Parent = this;

        if (body is not null)
            body.Parent = this;
    }

    public static FunctionDeclarationNode Create(
        string name,
        IReadOnlyList<FunctionParameterNode> parameters,
        TypeNode returnType,
        BlockStatementNode body)
        => new FunctionDeclarationNode(false, name, parameters, returnType, body);

    public static FunctionDeclarationNode CreateExternal(
        string name,
        IReadOnlyList<FunctionParameterNode> parameters,
        TypeNode returnType)
        => new FunctionDeclarationNode(true, name, parameters, returnType, null);

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

        return IsExternal == other.IsExternal &&
               Name == other.Name &&
               ReturnType == other.ReturnType &&
               Parameters.SequenceEqual(other.Parameters) &&
               Equals(SymbolTable, other.SymbolTable) &&
               Equals(Body, other.Body) &&
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

        return Equals((FunctionDeclarationNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(IsExternal, Name, Parameters, ReturnType, Body);

    public override string ToString()
    {
        var formatter = new CommonFormatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.Visit(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.Visit(this, context);

    public ISyntaxNode? Parent { get; set; }

    [MemberNotNullWhen(false, nameof(Body))]
    public bool IsExternal { get; }

    public string Name { get; }

    public IReadOnlyList<FunctionParameterNode> Parameters { get; }

    public TypeNode ReturnType { get; }

    public BlockStatementNode? Body { get; }

    public FunctionMetadata? Metadata { get; set; }

    public ISymbolTable? SymbolTable { get; set; }
}
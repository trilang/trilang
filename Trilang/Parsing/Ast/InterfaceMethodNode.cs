using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

// TODO: remove parameter names
public class InterfaceMethodNode : ISyntaxNode, IEquatable<InterfaceMethodNode>
{
    public InterfaceMethodNode(
        string name,
        IReadOnlyList<ParameterNode> parameters,
        IInlineTypeNode returnType)
    {
        Name = name;
        Parameters = parameters;
        ReturnType = returnType;
    }

    public static bool operator ==(InterfaceMethodNode? left, InterfaceMethodNode? right)
        => Equals(left, right);

    public static bool operator !=(InterfaceMethodNode? left, InterfaceMethodNode? right)
        => !Equals(left, right);

    public bool Equals(InterfaceMethodNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name &&
               Parameters.SequenceEqual(other.Parameters) &&
               ReturnType.Equals(other.ReturnType);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((InterfaceMethodNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, Parameters, ReturnType);

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

    public string Name { get; }

    public IReadOnlyList<ParameterNode> Parameters { get; }

    public IInlineTypeNode ReturnType { get; }

    public InterfaceMethodMetadata? Metadata { get; set; }
}
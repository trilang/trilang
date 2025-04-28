using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class InterfaceNode : IInlineTypeNode, IEquatable<InterfaceNode>
{
    public InterfaceNode(
        IReadOnlyList<InterfaceFieldNode> fields,
        IReadOnlyList<InterfaceMethodNode> methods)
    {
        Fields = fields;
        Methods = methods;

        foreach (var field in fields)
            field.Parent = this;

        foreach (var method in methods)
            method.Parent = this;

        var fieldNames = fields.Select(f => $"{f.Name}: {f.Type};");
        var methodNames = methods.Select(m => $"{m.Name}({string.Join(", ", m.Parameters.Select(p => p.Type))}): {m.ReturnType};");

        var combinedSignatures = fieldNames.Concat(methodNames).ToList();
        Name = combinedSignatures.Any()
            ? $"{{ {string.Join(" ", combinedSignatures)} }}"
            : "{ }";
    }

    public static bool operator ==(InterfaceNode? left, InterfaceNode? right)
        => Equals(left, right);

    public static bool operator !=(InterfaceNode? left, InterfaceNode? right)
        => !Equals(left, right);

    public bool Equals(InterfaceNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Fields.SequenceEqual(other.Fields) &&
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

        return Equals((InterfaceNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Fields, Methods);

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

    public IReadOnlyList<InterfaceFieldNode> Fields { get; }

    public IReadOnlyList<InterfaceMethodNode> Methods { get; }

    public ITypeMetadata? Metadata { get; set; }
}
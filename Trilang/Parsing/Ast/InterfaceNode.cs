using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class InterfaceNode : IInlineTypeNode, IEquatable<InterfaceNode>
{
    public InterfaceNode(
        IReadOnlyList<InterfacePropertyNode> properties,
        IReadOnlyList<InterfaceMethodNode> methods)
    {
        Properties = properties;
        Methods = methods;

        foreach (var property in properties)
            property.Parent = this;

        foreach (var method in methods)
            method.Parent = this;

        var propertyNames = properties.Select(f => $"{f.Name}: {f.Type};");
        var methodNames = methods.Select(m => $"{m.Name}({string.Join(", ", m.ParameterTypes)}): {m.ReturnType};");

        var combinedSignatures = propertyNames.Concat(methodNames).ToList();
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

        return Properties.SequenceEqual(other.Properties) &&
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

        return Equals((InterfaceNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Properties, Methods);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitInterface(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitInterface(this, context);

    public ISyntaxNode Transform(ITransformer transformer)
        => transformer.TransformInterface(this);

    public IInlineTypeNode Clone()
        => new InterfaceNode(
            Properties.Select(x => x.Clone()).ToArray(),
            Methods.Select(x => x.Clone()).ToArray()
        )
        {
            SymbolTable = SymbolTable,
            Metadata = Metadata,
        };

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }

    public string Name { get; }

    public IReadOnlyList<InterfacePropertyNode> Properties { get; }

    public IReadOnlyList<InterfaceMethodNode> Methods { get; }

    public ITypeMetadata? Metadata { get; set; }
}
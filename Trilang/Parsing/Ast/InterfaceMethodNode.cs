using Trilang.Metadata;
using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class InterfaceMethodNode : ISyntaxNode, IEquatable<InterfaceMethodNode>
{
    public InterfaceMethodNode(
        string name,
        IReadOnlyList<IInlineTypeNode> parameterTypes,
        IInlineTypeNode returnType)
    {
        Name = name;
        ParameterTypes = parameterTypes;
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
               ParameterTypes.SequenceEqual(other.ParameterTypes) &&
               ReturnType.Equals(other.ReturnType) &&
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

        return Equals((InterfaceMethodNode)obj);
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
        => visitor.VisitInterfaceMethod(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitInterfaceMethod(this, context);

    public ISyntaxNode Transform(ITransformer transformer)
        => transformer.TransformInterfaceMethod(this);

    public InterfaceMethodNode Clone()
        => new InterfaceMethodNode(Name, ParameterTypes.Select(t => t.Clone()).ToArray(), ReturnType.Clone())
        {
            Metadata = Metadata,
        };

    public ISyntaxNode? Parent { get; set; }

    public string Name { get; }

    public IReadOnlyList<IInlineTypeNode> ParameterTypes { get; }

    public IInlineTypeNode ReturnType { get; }

    public InterfaceMethodMetadata? Metadata { get; set; }
}
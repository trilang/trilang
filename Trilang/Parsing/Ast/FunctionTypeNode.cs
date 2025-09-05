using Trilang.Metadata;
using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class FunctionTypeNode : IInlineTypeNode, IEquatable<FunctionTypeNode>
{
    public FunctionTypeNode(IReadOnlyList<IInlineTypeNode> parameterTypes, IInlineTypeNode returnType)
    {
        ParameterTypes = parameterTypes;
        ReturnType = returnType;

        foreach (var parameter in parameterTypes)
            parameter.Parent = this;

        returnType.Parent = this;

        var parameters = string.Join(", ", parameterTypes.Select(p => p.Name));
        Name = $"({parameters}) => {returnType.Name}";
    }

    public static bool operator ==(FunctionTypeNode? left, FunctionTypeNode? right)
        => Equals(left, right);

    public static bool operator !=(FunctionTypeNode? left, FunctionTypeNode? right)
        => !Equals(left, right);

    public bool Equals(FunctionTypeNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return ParameterTypes.SequenceEqual(other.ParameterTypes) &&
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

        return Equals((FunctionTypeNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(ParameterTypes, ReturnType);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitFunctionType(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitFunctionType(this, context);

    public T Transform<T>(ITransformer<T> transformer)
        => transformer.TransformFunctionType(this);

    public IInlineTypeNode Clone()
        => new FunctionTypeNode(ParameterTypes.Select(t => t.Clone()).ToArray(), ReturnType.Clone())
        {
            Metadata = Metadata,
        };

    public ISyntaxNode? Parent { get; set; }

    public string Name { get; }

    public IReadOnlyList<IInlineTypeNode> ParameterTypes { get; }

    public IInlineTypeNode ReturnType { get; }

    public ITypeMetadata? Metadata { get; set; }
}
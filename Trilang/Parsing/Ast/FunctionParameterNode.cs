using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class FunctionParameterNode : VariableDeclarationNode, IEquatable<FunctionParameterNode>
{
    public FunctionParameterNode(string name, TypeNode type)
        : base(name, type)
    {
    }

    public static bool operator ==(FunctionParameterNode? left, FunctionParameterNode? right)
        => Equals(left, right);

    public static bool operator !=(FunctionParameterNode? left, FunctionParameterNode? right)
        => !Equals(left, right);

    public bool Equals(FunctionParameterNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name &&
               Type == other.Type &&
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

        return Equals((FunctionParameterNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, Type);

    public override string ToString()
    {
        var formatter = new CommonFormatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public override void Accept(IVisitor visitor)
        => visitor.Visit(this);

    public override void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.Visit(this, context);
}
using Trilang.Metadata;
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class CallExpressionNode : IExpressionNode, IEquatable<CallExpressionNode>
{
    public CallExpressionNode(string functionName, IReadOnlyList<IExpressionNode> parameters)
    {
        FunctionName = functionName;
        Parameters = parameters;

        foreach (var parameter in parameters)
            parameter.Parent = this;
    }

    public static bool operator ==(CallExpressionNode? left, CallExpressionNode? right)
        => Equals(left, right);

    public static bool operator !=(CallExpressionNode? left, CallExpressionNode? right)
        => !Equals(left, right);

    public bool Equals(CallExpressionNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return FunctionName == other.FunctionName &&
               Equals(ReturnTypeMetadata, other.ReturnTypeMetadata) &&
               Parameters.SequenceEqual(other.Parameters) &&
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

        return Equals((CallExpressionNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(FunctionName, Parameters);

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

    public string FunctionName { get; }

    public IReadOnlyList<IExpressionNode> Parameters { get; }

    public FunctionMetadata? Metadata { get; set; }

    public TypeMetadata? ReturnTypeMetadata
        => Metadata?.ReturnType;

    public SymbolTable? SymbolTable { get; set; }
}
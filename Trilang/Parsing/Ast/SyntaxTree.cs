using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class SyntaxTree : ISyntaxNode, IEquatable<SyntaxTree>
{
    public SyntaxTree(IReadOnlyList<FunctionDeclarationNode> functions)
    {
        Functions = functions;

        foreach (var function in functions)
            function.Parent = this;
    }

    public static bool operator ==(SyntaxTree? left, SyntaxTree? right)
        => Equals(left, right);

    public static bool operator !=(SyntaxTree? left, SyntaxTree? right)
        => !Equals(left, right);

    public bool Equals(SyntaxTree? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Functions.SequenceEqual(other.Functions) &&
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

        return Equals((SyntaxTree)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Functions);

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

    public IReadOnlyList<FunctionDeclarationNode> Functions { get; }

    public SymbolTable? SymbolTable { get; set; }
}
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Nodes;

public class SyntaxTree : ISyntaxNode, IEquatable<SyntaxTree>
{
    public SyntaxTree(IReadOnlyList<FunctionDeclarationNode> functions, SymbolTable symbolTable)
    {
        Functions = functions;
        SymbolTable = symbolTable;
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
               SymbolTable.Equals(other.SymbolTable);
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

    public override string? ToString()
    {
        var formatter = new CommonFormatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.Visit(this);

    public IReadOnlyList<FunctionDeclarationNode> Functions { get; }

    public SymbolTable SymbolTable { get; }
}
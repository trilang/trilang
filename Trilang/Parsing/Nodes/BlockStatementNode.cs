using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Nodes;

public class BlockStatementNode : IStatementNode, IEquatable<BlockStatementNode>
{
    public BlockStatementNode(SymbolTable symbolTable)
        : this([], symbolTable)
    {
    }

    public BlockStatementNode(IReadOnlyList<IStatementNode> statements, SymbolTable symbolTable)
    {
        Statements = statements;
        SymbolTable = symbolTable;
    }

    public static bool operator ==(BlockStatementNode? left, BlockStatementNode? right)
        => Equals(left, right);

    public static bool operator !=(BlockStatementNode? left, BlockStatementNode? right)
        => !Equals(left, right);

    public bool Equals(BlockStatementNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Statements.SequenceEqual(other.Statements) &&
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

        return Equals((BlockStatementNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Statements);

    public override string? ToString()
    {
        var formatter = new CommonFormatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.Visit(this);

    public IReadOnlyList<IStatementNode> Statements { get; }

    public SymbolTable SymbolTable { get; }
}
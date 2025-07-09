using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class LabelNode : IEquatable<LabelNode>, IStatementNode
{
    public LabelNode(string name)
        => Name = name;

    public static bool operator ==(LabelNode? left, LabelNode? right)
        => Equals(left, right);

    public static bool operator !=(LabelNode? left, LabelNode? right)
        => !Equals(left, right);

    public bool Equals(LabelNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name;
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((LabelNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitLabel(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitLabel(this, context);

    public string Name { get; }

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }
}
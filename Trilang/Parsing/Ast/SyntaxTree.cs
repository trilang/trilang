using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class SyntaxTree : ISyntaxNode, IEquatable<SyntaxTree>
{
    public SyntaxTree(IReadOnlyList<IDeclarationNode> declarations)
    {
        Declarations = declarations;

        foreach (var function in declarations)
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

        return Declarations.SequenceEqual(other.Declarations);
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
        => HashCode.Combine(Declarations);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitTree(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitTree(this, context);

    public ISyntaxNode? Parent { get; set; }

    public IReadOnlyList<IDeclarationNode> Declarations { get; }

    public ISymbolTable? SymbolTable { get; set; }
}
using Trilang.Parsing.Formatters;
using Trilang.Symbols;

namespace Trilang.Parsing.Ast;

public class IfDirectiveNode : IDeclarationNode, IStatementNode, IEquatable<IfDirectiveNode>
{
    public IfDirectiveNode(string directiveName, IReadOnlyList<ISyntaxNode> then, IReadOnlyList<ISyntaxNode> @else)
    {
        DirectiveName = directiveName;
        Then = then;
        Else = @else;
    }

    public static bool operator ==(IfDirectiveNode? left, IfDirectiveNode? right)
        => Equals(left, right);

    public static bool operator !=(IfDirectiveNode? left, IfDirectiveNode? right)
        => !Equals(left, right);

    public bool Equals(IfDirectiveNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return DirectiveName == other.DirectiveName &&
               Then.SequenceEqual(other.Then) &&
               Else.SequenceEqual(other.Else);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((IfDirectiveNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(DirectiveName, Then, Else);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitIfDirective(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitIfDirective(this, context);

    public ISyntaxNode? Parent { get; set; }

    public ISymbolTable? SymbolTable { get; set; }

    public string DirectiveName { get; }

    // IDeclarationNode | IStatementNode
    public IReadOnlyList<ISyntaxNode> Then { get; }

    public IReadOnlyList<ISyntaxNode> Else { get; }
}
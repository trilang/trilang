using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class GoToNode : IEquatable<GoToNode>, IStatementNode
{
    public GoToNode(string label)
        => Label = label;

    public static bool operator ==(GoToNode? left, GoToNode? right)
        => Equals(left, right);

    public static bool operator !=(GoToNode? left, GoToNode? right)
        => !Equals(left, right);

    public bool Equals(GoToNode? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Label == other.Label;
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((GoToNode)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Label);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(IVisitor visitor)
        => visitor.VisitGoTo(this);

    public void Accept<TContext>(IVisitor<TContext> visitor, TContext context)
        => visitor.VisitGoTo(this, context);

    public ISyntaxNode Transform(ITransformer transformer)
        => transformer.TransformGoTo(this);

    public string Label { get; }

    public ISyntaxNode? Parent { get; set; }
}
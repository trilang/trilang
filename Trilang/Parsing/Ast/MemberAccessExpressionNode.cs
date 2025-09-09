using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class MemberAccessExpressionNode : IExpressionNode
{
    public MemberAccessExpressionNode(string name) : this(null, name)
    {
    }

    public MemberAccessExpressionNode(IExpressionNode? member, string name)
    {
        Member = member;
        Name = name;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitMemberAccess(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformMemberAccess(this);

    public IExpressionNode? Member { get; }

    public string Name { get; }
}
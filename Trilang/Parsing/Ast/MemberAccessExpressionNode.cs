using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class MemberAccessExpressionNode : IExpressionNode
{
    public MemberAccessExpressionNode(SourceSpan sourceSpan, string name) : this(sourceSpan, null, name)
    {
    }

    public MemberAccessExpressionNode(SourceSpan sourceSpan, IExpressionNode? member, string name)
    {
        SourceSpan = sourceSpan;
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

    public SourceSpan SourceSpan { get; }

    public IExpressionNode? Member { get; }

    public string Name { get; }
}
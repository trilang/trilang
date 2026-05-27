using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class GenericExpressionNode : IExpressionNode, IHasGenericArguments
{
    public GenericExpressionNode(
        SourceSpan sourceSpan,
        MemberAccessExpressionNode member,
        IReadOnlyList<IInlineTypeNode> genericArguments)
    {
        SourceSpan = sourceSpan;
        Member = member;
        GenericArguments = genericArguments;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitGenericExpression(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformGenericExpression(this);

    public SourceSpan SourceSpan { get; }

    public MemberAccessExpressionNode Member { get; }

    public IReadOnlyList<IInlineTypeNode> GenericArguments { get; }
}
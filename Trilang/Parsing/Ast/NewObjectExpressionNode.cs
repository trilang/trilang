using System.Diagnostics;
using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class NewObjectExpressionNode : IExpressionNode
{
    public NewObjectExpressionNode(
        SourceSpan sourceSpan,
        IInlineTypeNode type,
        IReadOnlyList<IExpressionNode> parameters)
    {
        Debug.Assert(type is TypeRefNode or GenericApplicationNode or FakeTypeNode);

        SourceSpan = sourceSpan;
        Type = type;
        Parameters = parameters;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitNewObject(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformNewObject(this);

    public SourceSpan SourceSpan { get; }

    public IInlineTypeNode Type { get; }

    public IReadOnlyList<IExpressionNode> Parameters { get; }
}
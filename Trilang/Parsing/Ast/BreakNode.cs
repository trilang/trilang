using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class BreakNode : IStatementNode
{
    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitBreak(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformBreak(this);
}
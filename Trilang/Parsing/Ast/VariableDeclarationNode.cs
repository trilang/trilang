using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class VariableDeclarationNode : IStatementNode
{
    public VariableDeclarationNode(
        SourceSpan sourceSpan,
        string name,
        IInlineTypeNode type,
        IExpressionNode expression)
    {
        SourceSpan = sourceSpan;
        Name = name;
        Type = type;
        Expression = expression;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitVariable(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformVariable(this);

    public SourceSpan SourceSpan { get; }

    public string Name { get; }

    public IInlineTypeNode Type { get; }

    public IExpressionNode Expression { get; }
}
using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class ParameterNode : ISyntaxNode
{
    public ParameterNode(string name, IInlineTypeNode type)
    {
        Name = name;
        Type = type;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitParameter(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformParameter(this);

    public string Name { get; }

    public IInlineTypeNode Type { get; }
}
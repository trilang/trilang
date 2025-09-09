using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class FunctionTypeNode : IInlineTypeNode
{
    public FunctionTypeNode(IReadOnlyList<IInlineTypeNode> parameterTypes, IInlineTypeNode returnType)
    {
        ParameterTypes = parameterTypes;
        ReturnType = returnType;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitFunctionType(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformFunctionType(this);

    public IReadOnlyList<IInlineTypeNode> ParameterTypes { get; }

    public IInlineTypeNode ReturnType { get; }
}
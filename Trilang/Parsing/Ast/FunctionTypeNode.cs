using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class FunctionTypeNode : IInlineTypeNode
{
    public FunctionTypeNode(
        SourceSpan sourceSpan,
        IReadOnlyList<IInlineTypeNode> parameterTypes,
        IInlineTypeNode returnType)
    {
        SourceSpan = sourceSpan;
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

    public SourceSpan SourceSpan { get; }

    public IReadOnlyList<IInlineTypeNode> ParameterTypes { get; }

    public IInlineTypeNode ReturnType { get; }
}
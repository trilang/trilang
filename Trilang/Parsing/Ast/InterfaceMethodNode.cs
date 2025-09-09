using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class InterfaceMethodNode : ISyntaxNode
{
    public InterfaceMethodNode(
        string name,
        IReadOnlyList<IInlineTypeNode> parameterTypes,
        IInlineTypeNode returnType)
    {
        Name = name;
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
        => visitor.VisitInterfaceMethod(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformInterfaceMethod(this);

    public string Name { get; }

    public IReadOnlyList<IInlineTypeNode> ParameterTypes { get; }

    public IInlineTypeNode ReturnType { get; }
}
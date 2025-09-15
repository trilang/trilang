using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class InterfaceNode : IInlineTypeNode
{
    public InterfaceNode(
        SourceSpan sourceSpan,
        IReadOnlyList<InterfacePropertyNode> properties,
        IReadOnlyList<InterfaceMethodNode> methods)
    {
        SourceSpan = sourceSpan;
        Properties = properties;
        Methods = methods;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitInterface(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformInterface(this);

    public SourceSpan SourceSpan { get; }

    public IReadOnlyList<InterfacePropertyNode> Properties { get; }

    public IReadOnlyList<InterfaceMethodNode> Methods { get; }
}
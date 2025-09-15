using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class PropertyGetterNode : ISyntaxNode
{
    public PropertyGetterNode(SourceSpan sourceSpan, AccessModifier accessModifier, BlockStatementNode? body)
    {
        SourceSpan = sourceSpan;
        AccessModifier = accessModifier;
        Body = body;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitGetter(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformGetter(this);

    public SourceSpan SourceSpan { get; }

    public AccessModifier AccessModifier { get; }

    public BlockStatementNode? Body { get; }
}
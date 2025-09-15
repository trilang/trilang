using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class ConstructorDeclarationNode : ISyntaxNode
{
    public ConstructorDeclarationNode(
        SourceSpan sourceSpan,
        AccessModifier accessModifier,
        IReadOnlyList<ParameterNode> parameters,
        BlockStatementNode body)
    {
        SourceSpan = sourceSpan;
        AccessModifier = accessModifier;
        Parameters = parameters;
        Body = body;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitConstructor(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformConstructor(this);

    public SourceSpan SourceSpan { get; }

    public AccessModifier AccessModifier { get; }

    public IReadOnlyList<ParameterNode> Parameters { get; }

    public BlockStatementNode Body { get; }
}
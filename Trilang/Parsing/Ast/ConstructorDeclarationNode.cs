using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class ConstructorDeclarationNode : ISyntaxNode
{
    public ConstructorDeclarationNode(
        AccessModifier accessModifier,
        IReadOnlyList<ParameterNode> parameters,
        BlockStatementNode body)
    {
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

    public AccessModifier AccessModifier { get; }

    public IReadOnlyList<ParameterNode> Parameters { get; }

    public BlockStatementNode Body { get; }
}
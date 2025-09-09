using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class FunctionDeclarationNode : IDeclarationNode
{
    public FunctionDeclarationNode(
        string name,
        IReadOnlyList<ParameterNode> parameters,
        IInlineTypeNode returnType,
        BlockStatementNode body)
    {
        Name = name;
        Parameters = parameters;
        ReturnType = returnType;
        Body = body;
    }

    public static FunctionDeclarationNode Create(
        string name,
        IReadOnlyList<ParameterNode> parameters,
        IInlineTypeNode returnType,
        BlockStatementNode body)
        => new FunctionDeclarationNode(name, parameters, returnType, body);

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitFunction(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformFunction(this);

    public string Name { get; }

    public IReadOnlyList<ParameterNode> Parameters { get; }

    public IInlineTypeNode ReturnType { get; }

    public BlockStatementNode Body { get; }
}
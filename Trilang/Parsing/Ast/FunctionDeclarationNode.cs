using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class FunctionDeclarationNode : IDeclarationNode
{
    public FunctionDeclarationNode(
        SourceSpan sourceSpan,
        AccessModifier accessModifier,
        string name,
        IReadOnlyList<ParameterNode> parameters,
        IInlineTypeNode returnType,
        BlockStatementNode body)
    {
        AccessModifier = accessModifier;
        SourceSpan = sourceSpan;
        Name = name;
        Parameters = parameters;
        ReturnType = returnType;
        Body = body;
    }

    public static FunctionDeclarationNode Create(
        SourceSpan sourceSpan,
        AccessModifier accessModifier,
        string name,
        IReadOnlyList<ParameterNode> parameters,
        IInlineTypeNode returnType,
        BlockStatementNode body)
        => new FunctionDeclarationNode(sourceSpan, accessModifier, name, parameters, returnType, body);

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

    public SourceSpan SourceSpan { get; }

    public AccessModifier AccessModifier { get; }

    public string Name { get; }

    public IReadOnlyList<ParameterNode> Parameters { get; }

    public IInlineTypeNode ReturnType { get; }

    public BlockStatementNode Body { get; }
}
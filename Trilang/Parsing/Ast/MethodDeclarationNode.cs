using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class MethodDeclarationNode : ISyntaxNode
{
    public MethodDeclarationNode(
        SourceSpan sourceSpan,
        AccessModifier accessModifier,
        bool isStatic,
        string name,
        IReadOnlyList<ParameterNode> parameters,
        IInlineTypeNode returnType,
        BlockStatementNode body)
    {
        SourceSpan = sourceSpan;
        AccessModifier = accessModifier;
        IsStatic = isStatic;
        Name = name;
        Parameters = parameters;
        ReturnType = returnType;
        Body = body;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitMethod(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformMethod(this);

    public SourceSpan SourceSpan { get; }

    public AccessModifier AccessModifier { get; }

    public bool IsStatic { get; }

    public string Name { get; }

    public IReadOnlyList<ParameterNode> Parameters { get; }

    public IInlineTypeNode ReturnType { get; }

    public BlockStatementNode Body { get; }
}
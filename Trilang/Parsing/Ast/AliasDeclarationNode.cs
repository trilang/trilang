using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class AliasDeclarationNode : IDeclarationNode, IHasGenericArguments
{
    public AliasDeclarationNode(
        SourceSpan sourceSpan,
        AccessModifier accessModifier,
        string name,
        IReadOnlyList<IInlineTypeNode> genericArguments,
        IInlineTypeNode type)
    {
        SourceSpan = sourceSpan;
        AccessModifier = accessModifier;
        Name = name;
        GenericArguments = genericArguments;
        Type = type;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitTypeAlias(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformTypeAlias(this);

    public SourceSpan SourceSpan { get; }

    public AccessModifier AccessModifier { get; }

    public string Name { get; }

    public IReadOnlyList<IInlineTypeNode> GenericArguments { get; }

    public IInlineTypeNode Type { get; }
}
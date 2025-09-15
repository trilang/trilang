using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

// TODO: don't generate backing field if it's not needed
public class PropertyDeclarationNode : ISyntaxNode
{
    public PropertyDeclarationNode(SourceSpan sourceSpan, string name, IInlineTypeNode type)
        : this(sourceSpan, name, type, null, null)
    {
    }

    public PropertyDeclarationNode(
        SourceSpan sourceSpan,
        string name,
        IInlineTypeNode type,
        PropertyGetterNode? getter,
        PropertySetterNode? setter)
    {
        SourceSpan = sourceSpan;
        Name = name;
        Type = type;
        Getter = getter;
        Setter = setter;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitProperty(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformProperty(this);

    public SourceSpan SourceSpan { get; }

    public string Name { get; }

    public IInlineTypeNode Type { get; }

    public PropertyGetterNode? Getter { get; }

    public PropertySetterNode? Setter { get; }
}
using Trilang.Parsing.Formatters;

namespace Trilang.Parsing.Ast;

public class InterfacePropertyNode : ISyntaxNode
{
    public InterfacePropertyNode(
        string name,
        IInlineTypeNode type,
        AccessModifier? getterModifier,
        AccessModifier? setterModifier)
    {
        Name = name;
        Type = type;
        GetterModifier = getterModifier;
        SetterModifier = setterModifier;
    }

    public override string ToString()
    {
        var formatter = new Formatter();
        Accept(formatter);

        return formatter.ToString();
    }

    public void Accept(INodeVisitor visitor)
        => visitor.VisitInterfaceProperty(this);

    public T Transform<T>(INodeTransformer<T> transformer)
        => transformer.TransformInterfaceProperty(this);

    public string Name { get; }

    public IInlineTypeNode Type { get; }

    public AccessModifier? GetterModifier { get; }

    public AccessModifier? SetterModifier { get; }
}
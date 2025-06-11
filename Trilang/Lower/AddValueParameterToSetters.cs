using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Lower;

internal class AddValueParameterToSetters : Visitor
{
    protected override void VisitSetterEnter(PropertySetterNode node)
    {
        var parent = node.FindInParent<PropertyDeclarationNode>()!;
        var valueParameter = new ParameterNode(MemberAccessExpressionNode.Value, parent.Type);
        node.Parameters = [..node.Parameters, valueParameter];
    }
}
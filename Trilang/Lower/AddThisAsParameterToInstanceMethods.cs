using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Lower;

internal class AddThisAsParameterToInstanceMethods : Visitor
{
    // TODO: support generic types
    // TODO: support static ctors, props
    protected override void VisitMethodEnter(MethodDeclarationNode node)
    {
        if (node.IsStatic)
            return;

        var parent = node.FindInParent<TypeDeclarationNode>()!;
        var thisType = new TypeNode(parent.Name) { Metadata = parent.Metadata };
        var thisParameter = new ParameterNode(MemberAccessExpressionNode.This, thisType);
        node.Parameters = [thisParameter, ..node.Parameters];
    }

    protected override void VisitConstructorEnter(ConstructorDeclarationNode node)
    {
        var parent = node.FindInParent<TypeDeclarationNode>()!;
        var thisType = new TypeNode(parent.Name) { Metadata = parent.Metadata };
        var thisParameter = new ParameterNode(MemberAccessExpressionNode.This, thisType);
        node.Parameters = [thisParameter, ..node.Parameters];
    }

    protected override void VisitSetterEnter(PropertySetterNode node)
    {
        var parent = node.FindInParent<TypeDeclarationNode>()!;
        var thisType = new TypeNode(parent.Name) { Metadata = parent.Metadata };
        var thisParameter = new ParameterNode(MemberAccessExpressionNode.This, thisType);
        node.Parameters = [thisParameter];
    }

    protected override void VisitGetterEnter(PropertyGetterNode node)
    {
        var parent = node.FindInParent<TypeDeclarationNode>()!;
        var thisType = new TypeNode(parent.Name) { Metadata = parent.Metadata };
        var thisParameter = new ParameterNode(MemberAccessExpressionNode.This, thisType);
        node.Parameters = [thisParameter];
    }
}
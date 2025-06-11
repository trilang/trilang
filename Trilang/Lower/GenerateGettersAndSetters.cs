using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Lower;

internal class GenerateGettersAndSetters : Visitor
{
    protected override void VisitPropertyExit(PropertyDeclarationNode node)
    {
        node.Getter ??= new PropertyGetterNode(AccessModifier.Public, null);
        node.Setter ??= new PropertySetterNode(AccessModifier.Private, null);

        node.Getter.Body ??= new BlockStatementNode([
            new ReturnStatementNode(
                new MemberAccessExpressionNode(MemberAccessExpressionNode.Field)
            ),
        ]);

        node.Setter.Body ??= new BlockStatementNode([
            new ExpressionStatementNode(
                new BinaryExpressionNode(
                    BinaryExpressionKind.Assignment,
                    new MemberAccessExpressionNode(MemberAccessExpressionNode.Field),
                    new MemberAccessExpressionNode(MemberAccessExpressionNode.Value)
                )
            )
        ]);
    }
}
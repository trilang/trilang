using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Lower;

internal class GenerateGettersAndSetters : Visitor
{
    protected override void VisitPropertyExit(PropertyDeclarationNode node)
    {
        var propertyMetadata = node.Metadata!;
        var returnTypeMetadata = propertyMetadata.Type;

        node.Getter ??= new PropertyGetterNode(AccessModifier.Public, null)
        {
            Metadata = propertyMetadata.Getter,
        };
        node.Setter ??= new PropertySetterNode(AccessModifier.Private, null)
        {
            Metadata = propertyMetadata.Setter,
        };

        node.Getter.Body ??= new BlockStatementNode([
            new ReturnStatementNode(
                new MemberAccessExpressionNode(MemberAccessExpressionNode.Field)
                {
                    Reference = propertyMetadata,
                    AccessKind = PropertyAccessKind.Read,
                }
            ),
        ]);

        node.Setter.Body ??= new BlockStatementNode([
            new ExpressionStatementNode(
                new BinaryExpressionNode(
                    BinaryExpressionKind.Assignment,
                    new MemberAccessExpressionNode(MemberAccessExpressionNode.Field)
                    {
                        Reference = propertyMetadata,
                        AccessKind = PropertyAccessKind.Write,
                    },
                    new MemberAccessExpressionNode(MemberAccessExpressionNode.Value)
                    {
                        Reference = propertyMetadata,
                        AccessKind = PropertyAccessKind.Read,
                    }
                )
                {
                    ReturnTypeMetadata = returnTypeMetadata,
                }
            )
        ]);
    }
}
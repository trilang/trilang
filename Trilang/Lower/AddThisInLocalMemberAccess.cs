using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Lower;

internal class AddThisInLocalMemberAccess : Visitor
{
    protected override void VisitMemberAccessExit(MemberAccessExpressionNode node)
    {
        if (node.Member is not null)
            return;

        if (node.IsThis || node.IsField || node.IsValue)
            return;

        if (node.Reference is not IdSymbol id)
            return;

        if (id.Node is not PropertyDeclarationNode and not MethodDeclarationNode)
            return;

        var parent = node.FindInParent<TypeDeclarationNode>()!;
        var thisSymbol = node.SymbolTable?.GetId(MemberAccessExpressionNode.This) ??
                         throw new Exception("Missing `this` symbol.");

        node.Member = new MemberAccessExpressionNode(MemberAccessExpressionNode.This)
        {
            ReturnTypeMetadata = parent.Metadata,
            Reference = thisSymbol,
        };
    }
}
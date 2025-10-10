using Trilang.Metadata;
using Trilang.Semantics;
using Trilang.Semantics.Model;

namespace Trilang.Lower;

internal class AddThisInLocalMemberAccess : Visitor
{
    protected override void VisitMemberAccessExit(MemberAccessExpression node)
    {
        if (node.Member is not null)
            return;

        if (node.IsThis || node.IsField || node.IsValue)
            return;

        if (node.Reference is not PropertyMetadata and not MethodMetadata)
            return;

        var parent = node.FindInParent<TypeDeclaration>()!;

        node.Member = new MemberAccessExpression(null, MemberAccessExpression.This)
        {
            Reference = new ParameterMetadata(null, MemberAccessExpression.This, parent.Metadata!),
            AccessKind = MemberAccessKind.Read,
        };
    }
}
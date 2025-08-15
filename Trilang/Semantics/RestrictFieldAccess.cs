using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics;

internal class RestrictFieldAccess : Visitor
{
    protected override void VisitMemberAccessExit(MemberAccessExpressionNode node)
    {
        if (node.IsFirstMember)
            return;

        if (node.Reference is FieldMetadata)
            throw new SemanticAnalysisException($"The '{node.Name}' field is not accessible.");
    }
}
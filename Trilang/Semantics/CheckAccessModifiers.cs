using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics;

public class CheckAccessModifiers : Visitor
{
    protected override void VisitEnter(NewObjectExpressionNode node)
    {
        var ctor = node.Metadata!;
        var parentType = node.FindInParent<TypeDeclarationNode>()?.Metadata;

        // no need to check access modifier
        // ctor is used inside the type
        if (ctor.DeclaringType == parentType)
            return;

        if (ctor.AccessModifier != AccessModifierMetadata.Public)
            throw new SemanticAnalysisException($"The constructor of '{ctor.DeclaringType.Name}' is not accessible.");
    }

    protected override void VisitEnter(MemberAccessExpressionNode node)
    {
        // TODO: implement check access
    }
}
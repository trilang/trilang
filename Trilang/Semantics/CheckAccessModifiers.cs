using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics;

internal class CheckAccessModifiers : Visitor
{
    private static PropertyAccessKind FindParentAssignment(MemberAccessExpressionNode node)
    {
        var parent = node.Parent;
        while (parent is not null)
        {
            if (parent is BinaryExpressionNode { Kind: BinaryExpressionKind.Assignment } assignment)
            {
                if (ReferenceEquals(assignment.Left, node))
                    return PropertyAccessKind.Write;

                return PropertyAccessKind.Read;
            }

            if (parent is BinaryExpressionNode { IsCompoundAssignment: true } compound)
            {
                if (ReferenceEquals(compound.Left, node))
                    return PropertyAccessKind.ReadWrite;

                return PropertyAccessKind.Read;
            }

            parent = parent.Parent;
        }

        return PropertyAccessKind.Read;
    }

    protected override void VisitNewObjectEnter(NewObjectExpressionNode node)
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

    protected override void VisitMemberAccessEnter(MemberAccessExpressionNode node)
    {
        node.AccessKind = FindParentAssignment(node);

        if (node.Reference is not PropertyMetadata property)
            return;

        var type = node.FindInParent<TypeDeclarationNode>()?.Metadata;
        if (property.DeclaringType.Equals(type))
            return;

        if (node.AccessKind == PropertyAccessKind.Read &&
            property.Getter.AccessModifier == AccessModifierMetadata.Private)
            throw new SemanticAnalysisException($"The getter of '{property.Name}' is private.");

        if (node.AccessKind == PropertyAccessKind.Write &&
            property.Setter.AccessModifier == AccessModifierMetadata.Private)
            throw new SemanticAnalysisException($"The setter of '{property.Name}' is private.");
    }
}
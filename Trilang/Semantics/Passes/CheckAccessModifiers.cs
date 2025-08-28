using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics.Passes;

internal class CheckAccessModifiers : Visitor, ISemanticPass
{
    public void Analyze(SyntaxTree tree, SemanticPassContext context)
        => tree.Accept(this);

    protected override void VisitNewObjectEnter(NewObjectExpressionNode node)
    {
        var ctor = node.Metadata!;
        var parentType = node.FindInParent<TypeDeclarationNode>()?.Metadata;

        // no need to check access modifier
        // ctor is used inside the type
        if (ctor.DeclaringType.Equals(parentType))
            return;

        if (ctor.AccessModifier != AccessModifierMetadata.Public)
            throw new SemanticAnalysisException($"The constructor of '{ctor.DeclaringType}' is not accessible.");
    }

    protected override void VisitMemberAccessEnter(MemberAccessExpressionNode node)
    {
        if (node.AccessKind is null)
            return;

        if (node.Reference is not PropertyMetadata property)
            return;

        var type = node.FindInParent<TypeDeclarationNode>()?.Metadata;
        if (property.DeclaringType.Equals(type))
            return;

        if (node.AccessKind == MemberAccessKind.Read)
        {
            if (property.Getter is null)
                throw new SemanticAnalysisException($"The '{property.Name}' property does not have a getter.");

            if (property.Getter.AccessModifier == AccessModifierMetadata.Private)
                throw new SemanticAnalysisException($"The getter of '{property.Name}' is private.");
        }

        if (node.AccessKind == MemberAccessKind.Write)
        {
            if (property.Setter is null)
                throw new SemanticAnalysisException($"The '{property.Name}' property does not have a setter.");

            if (property.Setter.AccessModifier == AccessModifierMetadata.Private)
                throw new SemanticAnalysisException($"The setter of '{property.Name}' is private.");
        }
    }

    public string Name => nameof(CheckAccessModifiers);

    public IEnumerable<string> DependsOn =>
    [
        nameof(TypeChecker),
        nameof(MemberAccessKindAnalyser),
    ];
}
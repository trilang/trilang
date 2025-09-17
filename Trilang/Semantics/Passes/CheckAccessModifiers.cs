using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

// TODO: validate functions, implement when the package system is ready
// TODO: validate private types (what does it mean), implement when the package system is ready
internal class CheckAccessModifiers : Visitor, ISemanticPass
{
    public void Analyze(SemanticTree tree, SemanticPassContext context)
        => tree.Accept(this);

    protected override void VisitFunctionEnter(FunctionDeclaration node)
    {
        // TODO: mark type metadata as invalid
        if (node.Metadata!.AccessModifier == AccessModifierMetadata.Internal)
            throw new SemanticAnalysisException($"The '{node.Name}' function can't be internal.");
    }

    protected override void VisitNewObjectEnter(NewObjectExpression node)
    {
        var ctor = node.Metadata!;
        var parentType = node.FindInParent<TypeDeclaration>()?.Metadata;

        // no need to check access modifier
        // ctor is used inside the type
        if (ctor.DeclaringType.Equals(parentType))
            return;

        // TODO: check internal ctors, implement when the package system is ready
        if (ctor.AccessModifier != AccessModifierMetadata.Public)
            throw new SemanticAnalysisException($"The constructor of '{ctor.DeclaringType}' is not accessible.");
    }

    protected override void VisitMemberAccessEnter(MemberAccessExpression node)
    {
        if (node.AccessKind is null)
            return;

        if (node.Reference is not PropertyMetadata property)
            return;

        var type = node.FindInParent<TypeDeclaration>()?.Metadata;
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
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

// TODO: validate functions, implement when the package system is ready
// TODO: validate private types (what does it mean), implement when the package system is ready
internal class CheckAccessModifiers : Visitor, ISemanticPass
{
    private SemanticDiagnosticReporter diagnostics = null!;

    public void Analyze(IEnumerable<SemanticTree> semanticTrees, SemanticPassContext context)
    {
        diagnostics = context.Diagnostics;

        foreach (var tree in semanticTrees)
            tree.Accept(this);
    }

    protected override void VisitNewObjectEnter(NewObjectExpression node)
    {
        var ctor = node.Metadata!;
        var type = ctor.DeclaringType;
        var parentType = node.FindInParent<TypeDeclaration>()?.Metadata;

        // no need to check access modifier
        // ctor is used inside the type
        if (type.Equals(parentType))
            return;

        // TODO: check internal ctors, implement when the package system is ready
        if (ctor.AccessModifier != AccessModifierMetadata.Public)
            diagnostics.ConstructorNotAccessible(node, type);
    }

    protected override void VisitMemberAccessEnter(MemberAccessExpression node)
    {
        if (node.AccessKind is null)
            return;

        // TODO: check method access
        // TODO: check functions
        if (node.Reference is not PropertyMetadata property)
            return;

        var type = node.FindInParent<TypeDeclaration>()?.Metadata;
        if (property.DeclaringType.Equals(type))
            return;

        if (node.AccessKind == MemberAccessKind.Read)
        {
            if (property.Getter is null)
                diagnostics.UnknownGetter(node, property);
            else if (property.Getter.AccessModifier == AccessModifierMetadata.Private)
                diagnostics.GetterNotAccessible(node, property);
        }
        else if (node.AccessKind == MemberAccessKind.Write)
        {
            if (property.Setter is null)
                diagnostics.UnknownSetter(node, property);
            else if (property.Setter.AccessModifier == AccessModifierMetadata.Private)
                diagnostics.SetterNotAccessible(node, property);
        }
    }

    public string Name => nameof(CheckAccessModifiers);

    public IEnumerable<string> DependsOn =>
    [
        nameof(TypeChecker),
        nameof(MemberAccessKindAnalyser),
    ];
}
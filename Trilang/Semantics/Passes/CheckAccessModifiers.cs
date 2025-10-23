using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class CheckAccessModifiers : Visitor, ISemanticPass
{
    private SourceFile file;
    private SemanticDiagnosticReporter diagnostics = null!;

    public void Analyze(IEnumerable<SemanticTree> semanticTrees, SemanticPassContext context)
    {
        diagnostics = context.Diagnostics;

        foreach (var tree in semanticTrees)
        {
            file = tree.SourceFile;
            tree.Accept(this);
        }
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

        // TODO: implement and check type accessor
        // TODO: check internal ctors, implement when the package system is ready
        if (ctor.AccessModifier != AccessModifierMetadata.Public)
            diagnostics.ConstructorNotAccessible(node, type);
    }

    protected override void VisitMemberAccessEnter(MemberAccessExpression node)
    {
        if (node.AccessKind is null)
            return;

        if (node.Reference is PropertyMetadata property)
            CheckProperty(node, property);
        else if (node.Reference is MethodMetadata method)
            CheckMethod(node, method);
        else if (node.Reference is FunctionMetadata type)
            CheckFunction(node, type);
        else if (node.Reference is TypeMetadata typeMetadata)
            CheckType(node, typeMetadata);
    }

    private void CheckProperty(MemberAccessExpression node, PropertyMetadata property)
    {
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

    private void CheckMethod(MemberAccessExpression node, MethodMetadata method)
    {
        var type = node.FindInParent<TypeDeclaration>()?.Metadata;
        if (method.DeclaringType.Equals(type))
            return;

        switch (method.AccessModifier)
        {
            case AccessModifierMetadata.Public:
                break;
            case AccessModifierMetadata.Internal:
                // TODO: check internal methods, implement when the package system is ready
                break;
            case AccessModifierMetadata.Private:
                diagnostics.MethodNotAccessible(node, method);
                break;
            default:
                throw new ArgumentOutOfRangeException();
        }
    }

    private void CheckFunction(MemberAccessExpression node, FunctionMetadata function)
    {
        if (function.Definition?.File == file)
            return;

        switch (function.AccessModifier)
        {
            case AccessModifierMetadata.Public:
                break;
            case AccessModifierMetadata.Internal:
                // TODO: check internal functions, implement when the package system is ready
                break;
            case AccessModifierMetadata.Private:
                diagnostics.FunctionNotAccessible(node, function);
                break;
            default:
                throw new ArgumentOutOfRangeException();
        }
    }

    private void CheckType(MemberAccessExpression node, TypeMetadata type)
    {
        // TODO: check static access
    }

    public string Name => nameof(CheckAccessModifiers);

    public IEnumerable<string> DependsOn =>
    [
        nameof(TypeChecker),
        nameof(MemberAccessKindAnalyser),
    ];
}
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class CheckAccessModifiers : Visitor, ISemanticPass
{
    private readonly SemanticDiagnosticReporter diagnostics;

    public CheckAccessModifiers(ISet<string> directives, SemanticDiagnosticReporter diagnostics)
        : base(directives)
    {
        this.diagnostics = diagnostics;
    }

    public void Analyze(IEnumerable<SemanticTree> semanticTrees)
    {
        foreach (var tree in semanticTrees)
            tree.Accept(new CheckAccessModifiersVisitor(directives, diagnostics, tree.SourceFile));
    }

    public string Name => nameof(CheckAccessModifiers);

    public IEnumerable<string> DependsOn =>
    [
        nameof(TypeChecker),
        nameof(MemberAccessKindAnalyser),
    ];

    private sealed class CheckAccessModifiersVisitor : Visitor
    {
        private readonly SemanticDiagnosticReporter diagnostics;
        private readonly SourceFile file;

        public CheckAccessModifiersVisitor(
            ISet<string> directives,
            SemanticDiagnosticReporter diagnostics,
            SourceFile file)
            : base(directives)
        {
            this.diagnostics = diagnostics;
            this.file = file;
        }

        public override void VisitNewObject(NewObjectExpression node)
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

            base.VisitNewObject(node);
        }

        public override void VisitMemberAccess(MemberAccessExpression node)
        {
            if (node.AccessKind is null)
                return;

            if (node.Reference is PropertyMetadata property)
                CheckProperty(node, property);
            else if (node.Reference is MethodMetadata method)
                CheckMethod(node, method);
            else if (node.Reference is FunctionMetadata function)
                CheckFunction(node, function);
            else if (node.Reference is TypeMetadata type)
                CheckType(node, type);

            base.VisitMemberAccess(node);
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
            var enclosingType = node.FindInParent<TypeDeclaration>()?.Metadata;
            if (type.Equals(enclosingType))
                return;

            // TODO: check static access
        }
    }
}
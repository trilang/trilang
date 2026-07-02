using Trilang.Compilation;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class CheckAccessModifiers : Visitor, ISemanticPass
{
    private readonly CompilationContext compilationContext;
    private readonly SemanticDiagnosticReporter diagnostics;

    public CheckAccessModifiers(
        ISet<string> directives,
        DiagnosticCollection diagnostics,
        CompilationContext compilationContext)
        : base(directives)
    {
        this.compilationContext = compilationContext;
        this.diagnostics = diagnostics.ForSemantic();
    }

    public void Analyze(Project project)
    {
        var semanticTrees = project.SourceFiles.Select(x => x.SemanticTree!);
        foreach (var tree in semanticTrees)
        {
            var visitor = new CheckAccessModifiersVisitor(
                directives,
                diagnostics,
                tree.SourceFile,
                compilationContext);

            tree.Accept(visitor);
        }
    }

    public string Name => nameof(CheckAccessModifiers);

    public IEnumerable<string> DependsOn =>
    [
        nameof(Binder),
        nameof(MemberAccessKindAnalyser),
    ];

    private sealed class CheckAccessModifiersVisitor : Visitor
    {
        private readonly SemanticDiagnosticReporter diagnostics;
        private readonly SourceFile file;
        private readonly CompilationContext compilationContext;

        public CheckAccessModifiersVisitor(
            ISet<string> directives,
            SemanticDiagnosticReporter diagnostics,
            SourceFile file,
            CompilationContext compilationContext)
            : base(directives)
        {
            this.diagnostics = diagnostics;
            this.file = file;
            this.compilationContext = compilationContext;
        }

        private static PackageMetadata? GetPackage(ITypeMetadata type)
            => type.Namespace is NamespaceMetadata namespaceMetadata
                ? namespaceMetadata.Package
                : null;

        private static PackageMetadata? GetPackage(FunctionMetadata function)
            => function.Namespace is NamespaceMetadata namespaceMetadata
                ? namespaceMetadata.Package
                : null;

        private bool IsTypeAccessible(ITypeMetadata type, ISemanticNode node)
        {
            if (type is TypeArgumentMetadata)
                return true;

            if (type is GenericApplicationMetadata genericMetadata)
                type = genericMetadata.OpenGeneric;

            var typeAccess = type is INamedMetadata namedMetadata
                ? namedMetadata.AccessModifier
                : AccessModifierMetadata.Public;

            if (typeAccess is AccessModifierMetadata.Internal)
            {
                var package = GetPackage(type);
                var currentPackage = compilationContext.CurrentPackage!;
                if (package != currentPackage)
                {
                    diagnostics.TypeNotAccessible(node, type);
                    return false;
                }
            }
            else if (typeAccess is AccessModifierMetadata.Private)
            {
                if (type.Definition?.File != file)
                {
                    diagnostics.TypeNotAccessible(node, type);
                    return false;
                }
            }

            return true;
        }

        public override void VisitNewObject(NewObjectExpression node)
        {
            var ctor = node.Metadata;
            if (ctor is null || ctor.IsInvalid)
                return;

            var type = ctor.DeclaringType;
            var parentType = node.FindInParent<TypeDeclaration>()?.Metadata;

            // no need to check access modifier
            // when ctor is used inside the type
            if (type == parentType)
                return;

            if (!IsTypeAccessible(type, node))
                return;

            if (ctor.AccessModifier is AccessModifierMetadata.Internal)
            {
                var package = GetPackage(type);
                var currentPackage = compilationContext.CurrentPackage!;
                if (package != currentPackage)
                    diagnostics.ConstructorNotAccessible(node, type);
            }
            else if (ctor.AccessModifier is AccessModifierMetadata.Private)
            {
                diagnostics.ConstructorNotAccessible(node, type);
            }

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
                CheckStaticType(node, type);

            base.VisitMemberAccess(node);
        }

        public override void VisitTypeRef(TypeRef node)
        {
            var type = node.Metadata!;
            IsTypeAccessible(type, node);

            base.VisitTypeRef(node);
        }

        public override void VisitGenericType(GenericApplication node)
        {
            var type = node.Metadata!;
            IsTypeAccessible(type, node);

            base.VisitGenericType(node);
        }

        private void CheckProperty(MemberAccessExpression node, PropertyMetadata property)
        {
            var type = property.DeclaringType;
            var parentType = node.FindInParent<TypeDeclaration>()?.Metadata;
            if (type == parentType)
                return;

            if (node.AccessKind == MemberAccessKind.Read)
            {
                var getter = property.Getter;
                if (getter is null)
                {
                    diagnostics.UnknownGetter(node, property);
                }
                else
                {
                    if (getter.AccessModifier is AccessModifierMetadata.Internal)
                    {
                        var package = GetPackage(type);
                        var currentPackage = compilationContext.CurrentPackage!;
                        if (package != currentPackage)
                            diagnostics.GetterNotAccessible(node, property);
                    }
                    else if (getter.AccessModifier is AccessModifierMetadata.Private)
                    {
                        diagnostics.GetterNotAccessible(node, property);
                    }
                }
            }
            else if (node.AccessKind == MemberAccessKind.Write)
            {
                var setter = property.Setter;
                if (setter is null)
                {
                    diagnostics.UnknownSetter(node, property);
                }
                else
                {
                    if (setter.AccessModifier is AccessModifierMetadata.Internal)
                    {
                        var package = GetPackage(type);
                        var currentPackage = compilationContext.CurrentPackage!;
                        if (package != currentPackage)
                            diagnostics.SetterNotAccessible(node, property);
                    }
                    else if (setter.AccessModifier is AccessModifierMetadata.Private)
                    {
                        diagnostics.SetterNotAccessible(node, property);
                    }
                }
            }
        }

        private void CheckMethod(MemberAccessExpression node, MethodMetadata method)
        {
            var type = method.DeclaringType;
            var parentType = node.FindInParent<TypeDeclaration>()?.Metadata;
            if (type == parentType)
                return;

            if (method.AccessModifier is AccessModifierMetadata.Internal)
            {
                var package = GetPackage(type);
                var currentPackage = compilationContext.CurrentPackage!;
                if (package != currentPackage)
                    diagnostics.MethodNotAccessible(node, method);
            }
            else if (method.AccessModifier is AccessModifierMetadata.Private)
            {
                diagnostics.MethodNotAccessible(node, method);
            }
        }

        private void CheckFunction(MemberAccessExpression node, FunctionMetadata function)
        {
            if (function.Definition?.File == file)
                return;

            if (function.AccessModifier is AccessModifierMetadata.Internal)
            {
                var package = GetPackage(function);
                var currentPackage = compilationContext.CurrentPackage!;
                if (package != currentPackage)
                    diagnostics.FunctionNotAccessible(node, function);
            }
            else if (function.AccessModifier is AccessModifierMetadata.Private)
            {
                diagnostics.FunctionNotAccessible(node, function);
            }
        }

        private void CheckStaticType(MemberAccessExpression node, TypeMetadata type)
        {
            var parentType = node.FindInParent<TypeDeclaration>()?.Metadata;
            if (type == parentType)
                return;

            IsTypeAccessible(type, node);
        }
    }
}
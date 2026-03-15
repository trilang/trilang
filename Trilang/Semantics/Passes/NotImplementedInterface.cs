using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class NotImplementedInterface : ISemanticPass
{
    private readonly ISet<string> directives;
    private readonly SemanticDiagnosticReporter diagnostics;

    public NotImplementedInterface(ISet<string> directives, SemanticDiagnosticReporter diagnostics)
    {
        this.directives = directives;
        this.diagnostics = diagnostics;
    }

    public void Analyze(IEnumerable<SemanticTree> semanticTrees)
    {
        var visitor = new NotImplementedInterfaceVisitor(directives, diagnostics);
        foreach (var tree in semanticTrees)
            tree.Accept(visitor);
    }

    public string Name => nameof(NotImplementedInterface);

    public IEnumerable<string> DependsOn =>
    [
        nameof(TypeChecker),
        nameof(PrivateInterfaceProperties),
    ];

    private sealed class NotImplementedInterfaceVisitor : Visitor
    {
        private readonly SemanticDiagnosticReporter diagnostics;

        public NotImplementedInterfaceVisitor(
            ISet<string> directives,
            SemanticDiagnosticReporter diagnostics)
            : base(directives)
        {
            this.diagnostics = diagnostics;
        }

        public override void VisitType(TypeDeclaration node)
        {
            var type = node.Metadata;
            if (node.Interfaces.Count == 0 || type is null || type.IsInvalid)
                return;

            foreach (var @interface in type.Interfaces)
            {
                if (@interface.IsInvalid)
                    continue;

                foreach (var interfaceProperty in @interface.Properties)
                {
                    if (interfaceProperty.IsInvalid)
                        continue;

                    var properties = type.GetProperties(interfaceProperty.Name);
                    if (properties.IsEmpty)
                    {
                        diagnostics.PropertyIsNotImplemented(node, interfaceProperty);
                        continue;
                    }

                    var propertyMetadata = (PropertyMetadata)properties[0];
                    if (!interfaceProperty.Type.Equals(propertyMetadata.Type))
                        diagnostics.PropertyImplementationHasIncorrectType(propertyMetadata, interfaceProperty);

                    // TODO: allow to implement with higher access modifier?
                    if (interfaceProperty.GetterModifier is not null &&
                        propertyMetadata.Getter?.AccessModifier != interfaceProperty.GetterModifier)
                        diagnostics.PropertyGetterIncorrectAccessModifier(propertyMetadata);

                    if (interfaceProperty.SetterModifier is not null &&
                        propertyMetadata.Setter?.AccessModifier != interfaceProperty.SetterModifier)
                        diagnostics.PropertySetterIncorrectAccessModifier(propertyMetadata);
                }

                foreach (var interfaceMethod in @interface.Methods)
                {
                    if (interfaceMethod.IsInvalid)
                        continue;

                    var aggregate = type.GetMethods(interfaceMethod.Name);
                    if (aggregate.IsEmpty)
                    {
                        diagnostics.MethodIsNotImplemented(node, interfaceMethod);
                        continue;
                    }

                    var method = (MethodMetadata?)null;
                    if (aggregate.Members.Count > 1)
                    {
                        var matches = aggregate
                            .MatchFunction(interfaceMethod.Type.ParameterTypes)
                            .ToArray();

                        if (matches.Length == 0)
                        {
                            diagnostics.MethodIsNotImplemented(node, interfaceMethod);
                            continue;
                        }

                        method = (MethodMetadata)matches[0];
                    }
                    else
                    {
                        method = aggregate.Members.OfType<MethodMetadata>().FirstOrDefault();
                    }

                    if (method is null)
                    {
                        diagnostics.MethodIsNotImplemented(node, interfaceMethod);
                        continue;
                    }

                    if (!interfaceMethod.Type.Equals(method.Type))
                        diagnostics.MethodImplementationHasIncorrectType(method, interfaceMethod);

                    if (method.AccessModifier != AccessModifierMetadata.Public)
                        diagnostics.MethodImplementationIsNotPublic(method);
                }
            }

            base.VisitType(node);
        }
    }
}
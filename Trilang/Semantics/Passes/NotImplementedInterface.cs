using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class NotImplementedInterface : Visitor, ISemanticPass
{
    private SemanticDiagnosticReporter diagnostics = null!;

    public void Analyze(IEnumerable<SemanticTree> semanticTrees, SemanticPassContext context)
    {
        diagnostics = context.Diagnostics;

        foreach (var tree in semanticTrees)
            tree.Accept(this);
    }

    protected override void VisitTypeEnter(TypeDeclaration node)
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

                var propertyMetadata = type.GetProperty(interfaceProperty.Name);
                if (propertyMetadata is null)
                {
                    diagnostics.PropertyIsNotImplemented(node, interfaceProperty);
                    continue;
                }

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

                var group = type.GetMethod(interfaceMethod.Name);
                if (group is null)
                {
                    diagnostics.MethodIsNotImplemented(node, interfaceMethod);
                    continue;
                }

                var method = (MethodMetadata)group.Functions[0];
                if (group.Functions.Count > 1)
                {
                    var matches = group.Match(interfaceMethod.Type.ParameterTypes).ToArray();
                    if (matches.Length == 0)
                    {
                        diagnostics.MethodIsNotImplemented(node, interfaceMethod);
                        continue;
                    }

                    method = (MethodMetadata)matches[0];
                }

                if (!interfaceMethod.Type.Equals(method.Type))
                    diagnostics.MethodImplementationHasIncorrectType(method, interfaceMethod);

                if (method.AccessModifier != AccessModifierMetadata.Public)
                    diagnostics.MethodImplementationIsNotPublic(method);
            }
        }
    }

    public string Name => nameof(NotImplementedInterface);

    public IEnumerable<string> DependsOn =>
    [
        nameof(TypeChecker),
        nameof(PrivateInterfaceProperties),
    ];
}
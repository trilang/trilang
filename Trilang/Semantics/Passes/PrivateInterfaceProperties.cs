using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class PrivateInterfaceProperties : ISemanticPass
{
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly NamespaceMetadata rootNamespace;

    public PrivateInterfaceProperties(
        SemanticDiagnosticReporter diagnostics,
        NamespaceMetadata rootNamespace)
    {
        this.diagnostics = diagnostics;
        this.rootNamespace = rootNamespace;
    }

    public void Analyze(IEnumerable<SemanticTree> _)
    {
        foreach (var interfaceMetadata in rootNamespace.Types.OfType<InterfaceMetadata>())
        {
            foreach (var propertyMetadata in interfaceMetadata.Properties)
            {
                if (propertyMetadata.GetterModifier == AccessModifierMetadata.Private)
                {
                    propertyMetadata.MarkAsInvalid();
                    diagnostics.InterfaceGetterCantBePrivate(propertyMetadata);
                }

                if (propertyMetadata.SetterModifier == AccessModifierMetadata.Private)
                {
                    propertyMetadata.MarkAsInvalid();
                    diagnostics.InterfaceSetterCantBePrivate(propertyMetadata);
                }
            }
        }
    }

    public string Name => nameof(PrivateInterfaceProperties);

    public IEnumerable<string> DependsOn => [nameof(MetadataGenerator)];
}
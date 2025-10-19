using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class PrivateInterfaceProperties : ISemanticPass
{
    public void Analyze(SemanticTree tree, SemanticPassContext context)
    {
        var diagnostics = context.Diagnostics;
        var typeProvider = context.RootSymbolTable.TypeProvider;
        foreach (var interfaceMetadata in typeProvider.Types.OfType<InterfaceMetadata>())
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
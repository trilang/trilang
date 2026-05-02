using Trilang.Compilation;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;

namespace Trilang.Semantics.Passes;

internal class PrivateInterfaceProperties : ISemanticPass
{
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly CompilationContext compilationContext;

    public PrivateInterfaceProperties(DiagnosticCollection diagnostics, CompilationContext compilationContext)
    {
        this.diagnostics = diagnostics.ForSemantic();
        this.compilationContext = compilationContext;
    }

    public void Analyze(Project _)
    {
        var rootNamespace = compilationContext.RootNamespace;

        foreach (var interfaceMetadata in rootNamespace.Types.OfType<InterfaceMetadata>())
        {
            if (interfaceMetadata.IsInvalid)
                continue;

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
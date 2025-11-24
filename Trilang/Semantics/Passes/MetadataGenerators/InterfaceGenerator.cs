using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class InterfaceGenerator
{
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly MetadataProviderMap metadataProviderMap;
    private readonly HashSet<Interface> typesToProcess;

    public InterfaceGenerator(SemanticDiagnosticReporter diagnostics, MetadataProviderMap metadataProviderMap)
    {
        this.diagnostics = diagnostics;
        this.metadataProviderMap = metadataProviderMap;
        typesToProcess = [];
    }

    public void CreateInterfaces(IReadOnlyList<TypeSymbol> types)
    {
        foreach (var symbol in types)
        {
            if (!symbol.IsInterface)
                continue;

            var node = (Interface)symbol.Node;
            var typeProvider = metadataProviderMap.Get(node);

            if (typeProvider.GetType(symbol.Name) is not InterfaceMetadata metadata)
            {
                metadata = new InterfaceMetadata(node.GetLocation());

                if (typeProvider.DefineType(symbol.Name, metadata))
                    typesToProcess.Add(node);
            }

            node.Metadata = metadata;
        }
    }

    public void PopulateInterfaces()
    {
        foreach (var node in typesToProcess)
        {
            var metadata = (InterfaceMetadata)node.Metadata!;
            var typeProvider = metadataProviderMap.Get(node);

            foreach (var property in node.Properties)
                PopulateProperty(typeProvider, metadata, property);

            foreach (var methods in node.Methods.GroupBy(x => x.Name))
            {
                var group = new FunctionGroupMetadata();

                foreach (var method in methods)
                    PopulateMethod(typeProvider, metadata, method, group);
            }
        }
    }

    private void PopulateProperty(
        IMetadataProvider metadataProvider,
        InterfaceMetadata metadata,
        InterfaceProperty property)
    {
        var propertyTypeMetadata = property.Type.PopulateMetadata(metadataProvider, diagnostics);
        var propertyMetadata = new InterfacePropertyMetadata(
            metadata.Definition! with { Span = property.SourceSpan.GetValueOrDefault() },
            metadata,
            property.Name,
            propertyTypeMetadata,
            property.GetterModifier?.ToMetadata() ?? AccessModifierMetadata.Public,
            property.SetterModifier?.ToMetadata());

        if (metadata.GetProperty(property.Name) is not null)
        {
            propertyMetadata.MarkAsInvalid();
            diagnostics.InterfacePropertyAlreadyDefined(property);
        }

        metadata.AddProperty(propertyMetadata);
        property.Metadata = propertyMetadata;
    }

    private void PopulateMethod(
        IMetadataProvider metadataProvider,
        InterfaceMetadata metadata,
        InterfaceMethod method,
        FunctionGroupMetadata group)
    {
        var parameters = new ITypeMetadata[method.ParameterTypes.Count];
        for (var i = 0; i < method.ParameterTypes.Count; i++)
            parameters[i] = method.ParameterTypes[i].PopulateMetadata(metadataProvider, diagnostics);

        var returnTypeMetadata = method.ReturnType.PopulateMetadata(metadataProvider, diagnostics);
        var functionType = new FunctionTypeMetadata(null, parameters, returnTypeMetadata);

        if (metadataProvider.GetType(functionType.ToString()) is not FunctionTypeMetadata existingFunctionType)
        {
            metadataProvider.DefineType(functionType.ToString(), functionType);
            existingFunctionType = functionType;
        }

        // TODO: generic?
        var methodMetadata = new InterfaceMethodMetadata(
            metadata.Definition! with { Span = method.SourceSpan.GetValueOrDefault() },
            metadata,
            method.Name,
            existingFunctionType,
            group);

        if (metadata.GetMethods(method.Name, parameters).Any())
        {
            methodMetadata.MarkAsInvalid();
            diagnostics.InterfaceMethodAlreadyDefined(method);
        }

        metadata.AddMethod(methodMetadata);
        method.Metadata = methodMetadata;
    }
}
using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class InterfaceGenerator
{
    private record Item(InterfaceMetadata Metadata, Interface Node);

    private readonly SymbolTableMap symbolTableMap;
    private readonly HashSet<Item> typesToProcess;

    public InterfaceGenerator(SymbolTableMap symbolTableMap)
    {
        this.symbolTableMap = symbolTableMap;
        typesToProcess = [];
    }

    public void CreateInterfaces(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsInterface)
                continue;

            var interfaceNode = (Interface)symbol.Node;
            var root = interfaceNode.GetRoot();
            var typeProvider = symbolTableMap.Get(symbol.Node).TypeProvider;
            var metadata = new InterfaceMetadata(
                new SourceLocation(root.SourceFile, interfaceNode.SourceSpan.GetValueOrDefault()));

            if (typeProvider.DefineType(symbol.Name, metadata))
                typesToProcess.Add(new Item(metadata, interfaceNode));
        }
    }

    public void PopulateInterfaces()
    {
        foreach (var (metadata, interfaceNode) in typesToProcess)
        {
            var typeProvider = symbolTableMap.Get(interfaceNode).TypeProvider;

            foreach (var property in interfaceNode.Properties)
            {
                var propertyType = typeProvider.GetType(property.Type.Name) ??
                                   TypeMetadata.Invalid(property.Type.Name);

                var propertyMetadata = new InterfacePropertyMetadata(
                    metadata.Definition! with { Span = property.SourceSpan.GetValueOrDefault() },
                    metadata,
                    property.Name,
                    propertyType,
                    property.GetterModifier.HasValue
                        ? GetAccessModifierMetadata(property.GetterModifier.Value)
                        : AccessModifierMetadata.Public,
                    property.SetterModifier.HasValue
                        ? GetAccessModifierMetadata(property.SetterModifier.Value)
                        : null);

                metadata.AddProperty(propertyMetadata);
            }

            foreach (var method in interfaceNode.Methods)
            {
                var parameters = method.ParameterTypes
                    .Select(x => typeProvider.GetType(x.Name) ??
                                 TypeMetadata.Invalid(x.Name));

                var returnType = typeProvider.GetType(method.ReturnType.Name) ??
                                 TypeMetadata.Invalid(method.ReturnType.Name);

                var functionType = new FunctionTypeMetadata(null, parameters, returnType);

                if (typeProvider.GetType(functionType.ToString()) is not FunctionTypeMetadata existingFunctionType)
                {
                    typeProvider.DefineType(functionType.ToString(), functionType);
                    existingFunctionType = functionType;
                }

                // TODO: generic?
                var methodMetadata = new InterfaceMethodMetadata(
                    metadata.Definition! with { Span = method.SourceSpan.GetValueOrDefault() },
                    metadata,
                    method.Name,
                    existingFunctionType);
                metadata.AddMethod(methodMetadata);
            }
        }
    }

    private static AccessModifierMetadata GetAccessModifierMetadata(AccessModifier accessModifier)
        => accessModifier switch
        {
            AccessModifier.Public => AccessModifierMetadata.Public,
            AccessModifier.Private => AccessModifierMetadata.Private,

            _ => throw new ArgumentOutOfRangeException(nameof(accessModifier), accessModifier, null)
        };
}
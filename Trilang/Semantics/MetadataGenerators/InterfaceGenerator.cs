using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics.MetadataGenerators;

internal class InterfaceGenerator
{
    private record Item(InterfaceMetadata Metadata, InterfaceNode Node);

    private readonly HashSet<Item> typesToProcess;

    public InterfaceGenerator()
        => typesToProcess = [];

    public void CreateInterfaces(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsInterface)
                continue;

            if (symbol.Node is not InterfaceNode interfaceNode)
                throw new SemanticAnalysisException($"Expected '{symbol.Name}' to have an InterfaceNode, but found '{symbol.Node.GetType().Name}' instead.");

            var typeProvider = symbol.Node.SymbolTable!.TypeProvider;
            var metadata = new InterfaceMetadata();
            if (typeProvider.DefineType(symbol.Name, metadata))
                typesToProcess.Add(new Item(metadata, interfaceNode));
        }
    }

    public void PopulateInterfaces()
    {
        foreach (var (metadata, interfaceNode) in typesToProcess)
        {
            var typeProvider = interfaceNode.SymbolTable!.TypeProvider;

            foreach (var property in interfaceNode.Properties)
            {
                var propertyType = typeProvider.GetType(property.Type.Name) ??
                                   throw new SemanticAnalysisException($"The '{property.Name}' property has unknown type: '{property.Type.Name}'.");

                var propertyMetadata = new InterfacePropertyMetadata(
                    metadata,
                    property.Name,
                    propertyType,
                    property.GetterModifier.HasValue
                        ? GetAccessModifierMetadata(property.GetterModifier.Value)
                        : AccessModifierMetadata.Public,
                    property.SetterModifier.HasValue
                        ? GetAccessModifierMetadata(property.SetterModifier.Value)
                        : AccessModifierMetadata.Private);

                metadata.AddProperty(propertyMetadata);
            }

            foreach (var method in interfaceNode.Methods)
            {
                var parameters = method.ParameterTypes
                    .Select(x => typeProvider.GetType(x.Name) ??
                                 throw new SemanticAnalysisException($"The '{x.Name}' parameter has unknown type: '{x.Name}'."));

                var returnType = typeProvider.GetType(method.ReturnType.Name) ??
                                 throw new SemanticAnalysisException($"The '{method.Name}' method has unknown return type: '{method.ReturnType.Name}'.");

                var functionType = new FunctionTypeMetadata(parameters, returnType);

                if (typeProvider.GetType(functionType.ToString()) is not FunctionTypeMetadata existingFunctionType)
                {
                    typeProvider.DefineType(functionType.ToString(), functionType);
                    existingFunctionType = functionType;
                }

                // TODO: generic?
                var methodMetadata = new InterfaceMethodMetadata(metadata, method.Name, existingFunctionType);
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
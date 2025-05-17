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
            var metadata = new InterfaceMetadata(symbol.Name);
            if (typeProvider.DefineType(metadata))
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

                var propertyMetadata = new InterfacePropertyMetadata(metadata, property.Name, propertyType);
                metadata.AddProperty(propertyMetadata);
            }

            foreach (var method in interfaceNode.Methods)
            {
                var parameterNames = string.Join(", ", method.Parameters.Select(p => p.Type.Name));
                var functionTypeName = $"({parameterNames}) => {method.ReturnType.Name}";
                if (typeProvider.GetType(functionTypeName) is not FunctionTypeMetadata functionType)
                {
                    functionType = new FunctionTypeMetadata(functionTypeName);

                    foreach (var parameter in method.Parameters)
                    {
                        var parameterType = typeProvider.GetType(parameter.Type.Name) ??
                                            throw new SemanticAnalysisException($"The '{parameter.Name}' parameter has unknown type: '{parameter.Type.Name}'.");

                        functionType.AddParameter(parameterType);
                    }

                    functionType.ReturnType = typeProvider.GetType(method.ReturnType.Name) ??
                                              throw new SemanticAnalysisException($"The '{method.Name}' method has unknown return type: '{method.ReturnType.Name}'.");

                    typeProvider.DefineType(functionType);
                }

                // TODO: generic?
                var methodMetadata = new InterfaceMethodMetadata(metadata, method.Name, functionType);
                metadata.AddMethod(methodMetadata);
            }
        }
    }
}
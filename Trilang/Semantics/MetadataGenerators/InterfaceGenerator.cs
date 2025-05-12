using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics.MetadataGenerators;

internal class InterfaceGenerator
{
    public void CreateInterfaces(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsInterface)
                continue;

            var typeProvider = symbol.Node.SymbolTable!.TypeProvider;
            var metadata = new InterfaceMetadata(symbol.Name);
            if (!typeProvider.DefineType(metadata))
                throw new SemanticAnalysisException($"The '{symbol.Name}' type is already defined.");
        }
    }

    public void PopulateInterfaces(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsInterface)
                continue;

            var node = symbol.Node;
            if (node is not InterfaceNode interfaceNode)
                continue;

            var typeProvider = node.SymbolTable!.TypeProvider;
            if (typeProvider.GetType(symbol.Name) is not InterfaceMetadata metadata)
                throw new SemanticAnalysisException($"The '{symbol.Name}' type is not an interface.");

            foreach (var property in interfaceNode.Properties)
            {
                var propertyType = typeProvider.GetType(property.Type.Name) ??
                                   throw new SemanticAnalysisException($"The '{property.Name}' property has unknown type: '{property.Type.Name}'.");

                var propertyMetadata = new InterfacePropertyMetadata(metadata, property.Name, propertyType);
                metadata.AddProperty(propertyMetadata);
            }

            foreach (var method in interfaceNode.Methods)
            {
                var parameters = new ITypeMetadata[method.Parameters.Count];
                for (var i = 0; i < parameters.Length; i++)
                {
                    var parameter = method.Parameters[i];
                    var parameterType = typeProvider.GetType(parameter.Type.Name) ??
                                        throw new SemanticAnalysisException($"The '{parameter.Name}' parameter has unknown type: '{parameter.Type.Name}'.");

                    parameters[i] = parameterType;
                }

                var returnType = typeProvider.GetType(method.ReturnType.Name) ??
                                 throw new SemanticAnalysisException($"The '{method.Name}' method has unknown return type: '{method.ReturnType.Name}'.");

                var functionType = new FunctionTypeMetadata(parameters, returnType);
                typeProvider.DefineType(functionType);

                var methodMetadata = new InterfaceMethodMetadata(metadata, method.Name, functionType);
                metadata.AddMethod(methodMetadata);
            }
        }
    }
}
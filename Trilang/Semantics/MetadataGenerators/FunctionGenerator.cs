using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics.MetadataGenerators;

internal class FunctionGenerator
{
    public void BuildFunctionTypes(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsFunction)
                continue;

            if (symbol.Node is not FunctionTypeNode function)
                throw new SemanticAnalysisException($"The '{symbol.Name}' symbol is not a function.");

            var typeProvider = symbol.Node.SymbolTable!.TypeProvider;
            var parameterTypes = GetParameterTypes(typeProvider, function.ParameterTypes);
            var returnType = typeProvider.GetType(function.ReturnType.Name) ??
                             throw new SemanticAnalysisException($"The function has unknown return type: '{function.ReturnType.Name}'.");

            // TODO: generic?
            var functionTypeMetadata = new FunctionTypeMetadata(parameterTypes, returnType);
            if (typeProvider.GetType(functionTypeMetadata.Name) is null)
                typeProvider.DefineType(functionTypeMetadata);
        }
    }

    public void BuildFunctions(IReadOnlyDictionary<string, IdSymbol> functions)
    {
        foreach (var (_, symbol) in functions)
        {
            if (symbol.Node is not FunctionDeclarationNode function)
                continue;

            var typeProvider = symbol.Node.SymbolTable!.TypeProvider;
            var parameterTypes = GetParameterTypes(typeProvider, function.Parameters.Select(x => x.Type).ToList());
            var returnType = typeProvider.GetType(function.ReturnType.Name) ??
                             throw new SemanticAnalysisException($"The function has unknown return type: '{function.ReturnType.Name}'.");

            // TODO: generic?
            var functionTypeMetadata = new FunctionTypeMetadata(parameterTypes, returnType);
            typeProvider.DefineType(functionTypeMetadata);
        }
    }

    private ITypeMetadata[] GetParameterTypes(
        ITypeMetadataProvider typeProvider,
        IReadOnlyList<IInlineTypeNode> parameters)
    {
        var parameterTypes = new ITypeMetadata[parameters.Count];
        for (var i = 0; i < parameterTypes.Length; i++)
        {
            var parameterType = parameters[i];

            parameterTypes[i] = typeProvider.GetType(parameterType.Name) ??
                                throw new SemanticAnalysisException($"The function has unknown parameter type: '{parameterType.Name}'.");
        }

        return parameterTypes;
    }
}
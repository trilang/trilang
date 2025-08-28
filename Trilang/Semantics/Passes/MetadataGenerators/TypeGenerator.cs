using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class TypeGenerator
{
    private record Item(TypeMetadata Metadata, TypeDeclarationNode Node);

    private readonly SymbolTableMap symbolTableMap;
    private readonly HashSet<Item> typesToProcess;

    public TypeGenerator(SymbolTableMap symbolTableMap)
    {
        this.symbolTableMap = symbolTableMap;
        typesToProcess = [];
    }

    public void CreateTypes(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (symbol is { IsType: false, IsGenericType: false })
                continue;

            if (symbol.Node is not TypeDeclarationNode typeDeclarationNode)
                throw new SemanticAnalysisException($"Expected '{symbol.Name}' to have a TypeDeclarationNode, but found '{symbol.Node.GetType().Name}' instead.");

            var typeProvider = symbolTableMap.Get(symbol.Node).TypeProvider;
            var metadata = new TypeMetadata(typeDeclarationNode.Name);

            foreach (var genericArgument in typeDeclarationNode.GenericArguments)
            {
                var argumentMetadata = new TypeArgumentMetadata(genericArgument.Name);
                if (!typeProvider.DefineType(genericArgument.Name, argumentMetadata))
                    throw new SemanticAnalysisException($"The '{genericArgument.Name}' type argument is already defined.");

                metadata.AddGenericArgument(argumentMetadata);
            }

            if (!typeProvider.DefineType(symbol.Name, metadata))
                throw new SemanticAnalysisException($"The '{symbol.Name}' type is already defined.");

            typesToProcess.Add(new Item(metadata, typeDeclarationNode));
        }
    }

    public void PopulateTypes()
    {
        foreach (var (type, typeDeclarationNode) in typesToProcess)
        {
            var typeProvider = symbolTableMap.Get(typeDeclarationNode).TypeProvider;

            foreach (var @interface in typeDeclarationNode.Interfaces)
            {
                // TODO: support generic interfaces
                var aliasMetadata = typeProvider.GetType(@interface.Name) as TypeAliasMetadata ??
                                    throw new SemanticAnalysisException($"The '{@interface.Name}' interface is not defined.");

                var interfaceMetadata = aliasMetadata.Type as InterfaceMetadata ??
                                        throw new SemanticAnalysisException($"The '{@interface.Name}' interface is not an interface.");

                type.AddInterface(interfaceMetadata);
            }

            foreach (var property in typeDeclarationNode.Properties)
            {
                var propertyType = typeProvider.GetType(property.Type.Name) ??
                                   throw new SemanticAnalysisException($"The '{property.Name}' property has unknown type: '{property.Type.Name}'.");

                var propertyMetadata = new PropertyMetadata(
                    type,
                    property.Name,
                    propertyType,
                    GetAccessModifierMetadata(property.Getter?.AccessModifier),
                    GetAccessModifierMetadata(property.Setter?.AccessModifier));

                // TODO: add in a constructor?
                type.AddProperty(propertyMetadata);

                if (propertyMetadata.Getter is not null)
                {
                    type.AddMethod(propertyMetadata.Getter);
                    typeProvider.DefineType(propertyMetadata.Getter.Type);
                }

                if (propertyMetadata.Setter is not null)
                {
                    type.AddMethod(propertyMetadata.Setter);
                    typeProvider.DefineType(propertyMetadata.Setter.Type);
                }
            }

            if (typeDeclarationNode.Constructors.Count > 0)
            {
                foreach (var constructor in typeDeclarationNode.Constructors)
                {
                    var parameters = constructor.Parameters
                        .Select(x => typeProvider.GetType(x.Type.Name) ??
                                     throw new SemanticAnalysisException($"The '{x.Name}' parameter has unknown type: '{x.Type.Name}'."));
                    var functionType = new FunctionTypeMetadata(parameters, type);
                    functionType = typeProvider.GetOrDefine(functionType);

                    var parametersMetadata = GetParameters(typeProvider, constructor.Parameters);
                    var constructorMetadata = new ConstructorMetadata(
                        type,
                        GetAccessModifierMetadata(constructor.AccessModifier) ?? AccessModifierMetadata.Public,
                        parametersMetadata,
                        functionType);

                    type.AddConstructor(constructorMetadata);
                }
            }
            else
            {
                var functionType = new FunctionTypeMetadata([], type);
                functionType = typeProvider.GetOrDefine(functionType);

                type.AddConstructor(new ConstructorMetadata(
                    type,
                    AccessModifierMetadata.Public,
                    [],
                    functionType
                ));
            }

            foreach (var method in typeDeclarationNode.Methods)
            {
                var parameters = GetParameters(typeProvider, method.Parameters);
                var parameterTypes = method.Parameters
                    .Select(x => typeProvider.GetType(x.Type.Name) ??
                                 throw new SemanticAnalysisException($"The '{x.Name}' parameter has unknown type: '{x.Type.Name}'."));

                var returnType = typeProvider.GetType(method.ReturnType.Name) ??
                                 throw new SemanticAnalysisException($"The '{method.Name}' method has unknown return type: '{method.ReturnType.Name}'.");

                var functionType = new FunctionTypeMetadata(parameterTypes, returnType);
                functionType = typeProvider.GetOrDefine(functionType);

                var methodMetadata = new MethodMetadata(
                    type,
                    GetAccessModifierMetadata(method.AccessModifier) ?? AccessModifierMetadata.Public,
                    method.IsStatic,
                    method.Name,
                    parameters,
                    functionType);

                type.AddMethod(methodMetadata);
            }
        }
    }

    private ParameterMetadata[] GetParameters(
        ITypeMetadataProvider typeProvider,
        IReadOnlyList<ParameterNode> parameters)
    {
        var result = new ParameterMetadata[parameters.Count];
        for (var i = 0; i < parameters.Count; i++)
        {
            var parameter = parameters[i];
            var parameterType = typeProvider.GetType(parameter.Type.Name) ??
                                throw new SemanticAnalysisException($"The '{parameter.Name}' parameter has unknown type: '{parameter.Type.Name}'.");

            result[i] = new ParameterMetadata(parameter.Name, parameterType);
        }

        return result;
    }

    private static AccessModifierMetadata? GetAccessModifierMetadata(AccessModifier? accessModifier)
        => accessModifier switch
        {
            AccessModifier.Public => AccessModifierMetadata.Public,
            AccessModifier.Private => AccessModifierMetadata.Private,

            null => null,
            _ => throw new ArgumentOutOfRangeException(nameof(accessModifier), accessModifier, null)
        };
}
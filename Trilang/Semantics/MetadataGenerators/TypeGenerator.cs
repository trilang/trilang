using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics.MetadataGenerators;

internal class TypeGenerator
{
    public void CreateTypes(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsType && !symbol.IsGenericType)
                continue;

            if (symbol.Node is not TypeDeclarationNode typeDeclarationNode)
                continue;

            var typeProvider = symbol.Node.SymbolTable!.TypeProvider;
            var metadata = new TypeMetadata(symbol.Name);

            foreach (var genericArgument in typeDeclarationNode.GenericArguments)
            {
                var argumentMetadata = new TypeArgumentMetadata(genericArgument.Name);
                if (!typeProvider.DefineType(argumentMetadata))
                    throw new SemanticAnalysisException($"The '{genericArgument.Name}' type argument is already defined.");

                metadata.AddGenericArgument(argumentMetadata);
            }

            if (!typeProvider.DefineType(metadata))
                throw new SemanticAnalysisException($"The '{symbol.Name}' type is already defined.");
        }
    }

    public void PopulateTypes(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsType && !symbol.IsGenericType)
                continue;

            var node = symbol.Node;
            if (node is not TypeDeclarationNode typeDeclarationNode)
                continue;

            var typeProvider = node.SymbolTable!.TypeProvider;
            var metadata = typeProvider.GetType(symbol.Name) ??
                           throw new SemanticAnalysisException($"The '{symbol.Name}' type is not defined.");

            if (metadata is not TypeMetadata type)
                continue;

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

                var propertyMetadata = new PropertyMetadata(type, property.Name, propertyType);

                if (property.Getter is not null)
                    propertyMetadata.Getter = new PropertyGetterMetadata(
                        propertyMetadata,
                        GetAccessModifierMetadata(property.Getter.AccessModifier));

                if (property.Setter is not null)
                    propertyMetadata.Setter = new PropertySetterMetadata(
                        propertyMetadata,
                        GetAccessModifierMetadata(property.Setter.AccessModifier));

                type.AddProperty(propertyMetadata);
            }

            if (typeDeclarationNode.Constructors.Count > 0)
            {
                foreach (var constructor in typeDeclarationNode.Constructors)
                {
                    var parameters = GetParameterTypes(typeProvider, constructor.Parameters);
                    var constructorMetadata = new ConstructorMetadata(
                        type,
                        GetAccessModifierMetadata(constructor.AccessModifier),
                        parameters);

                    type.AddConstructor(constructorMetadata);
                }
            }
            else
            {
                type.AddConstructor(new ConstructorMetadata(type, AccessModifierMetadata.Public, []));
            }

            foreach (var method in typeDeclarationNode.Methods)
            {
                var parameters = GetParameterTypes(typeProvider, method.Parameters);
                var returnType = typeProvider.GetType(method.ReturnType.Name) ??
                                 throw new SemanticAnalysisException($"The '{method.Name}' method has unknown return type: '{method.ReturnType.Name}'.");

                var functionType = new FunctionTypeMetadata(parameters, returnType);
                typeProvider.DefineType(functionType);

                var methodMetadata = new MethodMetadata(
                    type,
                    GetAccessModifierMetadata(method.AccessModifier),
                    method.Name,
                    functionType);

                type.AddMethod(methodMetadata);
            }
        }
    }

    private ITypeMetadata[] GetParameterTypes(
        ITypeMetadataProvider typeProvider,
        IReadOnlyList<ParameterNode> parameters)
    {
        var result = new ITypeMetadata[parameters.Count];
        for (var i = 0; i < parameters.Count; i++)
        {
            var parameter = parameters[i];

            result[i] = typeProvider.GetType(parameter.Type.Name) ??
                        throw new SemanticAnalysisException($"The '{parameter.Name}' parameter has unknown type: '{parameter.Type.Name}'.");
        }

        return result;
    }

    private static AccessModifierMetadata GetAccessModifierMetadata(AccessModifier accessModifier)
        => accessModifier switch
        {
            AccessModifier.Public => AccessModifierMetadata.Public,
            AccessModifier.Private => AccessModifierMetadata.Private,

            _ => throw new ArgumentOutOfRangeException(nameof(accessModifier), accessModifier, null)
        };
}
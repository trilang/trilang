using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics.MetadataGenerators;

internal class GenericTypeGenerator
{
    public void CreateGenericTypes(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsClosedGenericType)
                continue;

            var typeProvider = symbol.Node.SymbolTable!.TypeProvider;
            var metadata = new TypeMetadata(symbol.Name);
            typeProvider.DefineType(metadata);
        }
    }

    public void PopulateGenericTypes(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsClosedGenericType)
                continue;

            if (symbol.Node is not GenericTypeNode genericTypeNode)
                continue;

            var typeProvider = symbol.Node.SymbolTable!.TypeProvider;
            var type = typeProvider.GetType(symbol.Name) as TypeMetadata ??
                       throw new SemanticAnalysisException($"The '{symbol.Name}' type is not defined.");

            // ignore open generic types
            if (IsOpenGeneric(typeProvider, genericTypeNode))
                continue;

            var openName = genericTypeNode.GetOpenGenericName();
            var openGenericType = typeProvider.GetType(openName) as TypeMetadata ??
                                  throw new SemanticAnalysisException($"The '{openName}' generic type is not defined.");

            foreach (var argumentNode in genericTypeNode.TypeArguments)
            {
                var typeArgument = typeProvider.GetType(argumentNode.Name) ??
                                   throw new SemanticAnalysisException($"The '{argumentNode.Name}' type argument is not defined.");

                type.AddGenericArgument(typeArgument);
            }

            var typeArgumentsMap = TypeArgumentMap.Create(type.GenericArguments, openGenericType.GenericArguments);

            foreach (var @interface in openGenericType.Interfaces)
            {
                // TODO: support generic interfaces
                var aliasMetadata = typeProvider.GetType(@interface.Name) as TypeAliasMetadata ??
                                    throw new SemanticAnalysisException($"The '{@interface.Name}' interface is not defined.");

                var interfaceMetadata = aliasMetadata.Type as InterfaceMetadata ??
                                        throw new SemanticAnalysisException($"The '{@interface.Name}' interface is not an interface.");

                type.AddInterface(interfaceMetadata);
            }

            foreach (var property in openGenericType.Properties)
            {
                var propertyType = typeArgumentsMap.Map(property.Type);
                var propertyMetadata = new PropertyMetadata(type, property.Name, propertyType);

                if (property.Getter is not null)
                    propertyMetadata.Getter = new PropertyGetterMetadata(
                        propertyMetadata,
                        property.Getter.AccessModifier);

                if (property.Setter is not null)
                    propertyMetadata.Setter = new PropertySetterMetadata(
                        propertyMetadata,
                        property.Setter.AccessModifier);

                type.AddProperty(propertyMetadata);
            }

            foreach (var constructor in openGenericType.Constructors)
            {
                var parameters = new ITypeMetadata[constructor.ParameterTypes.Count];
                for (var i = 0; i < parameters.Length; i++)
                    parameters[i] = typeArgumentsMap.Map(constructor.ParameterTypes[i]);

                var constructorMetadata = new ConstructorMetadata(
                    type,
                    constructor.AccessModifier,
                    parameters);

                type.AddConstructor(constructorMetadata);
            }

            foreach (var method in openGenericType.Methods)
            {
                // TODO: support generic methods
                var methodType = method.TypeMetadata;
                var parameters = new ITypeMetadata[methodType.ParameterTypes.Count];
                for (var i = 0; i < parameters.Length; i++)
                    parameters[i] = typeArgumentsMap.Map(methodType.ParameterTypes[i]);

                var returnType = typeArgumentsMap.Map(methodType.ReturnType);
                var functionType = new FunctionTypeMetadata(parameters, returnType);
                typeProvider.DefineType(functionType);

                var methodMetadata = new MethodMetadata(
                    type,
                    method.AccessModifier,
                    method.Name,
                    functionType);

                type.AddMethod(methodMetadata);
            }
        }
    }

    private bool IsOpenGeneric(ITypeMetadataProvider typeProvider, GenericTypeNode genericTypeNode)
    {
        var isOpenGeneric = true;
        foreach (var argumentNode in genericTypeNode.TypeArguments)
        {
            var typeArgument = typeProvider.GetType(argumentNode.Name) ??
                               throw new SemanticAnalysisException($"The '{argumentNode.Name}' type argument is not defined.");

            if (typeArgument is not TypeArgumentMetadata)
                isOpenGeneric = false;
        }

        return isOpenGeneric;
    }
}
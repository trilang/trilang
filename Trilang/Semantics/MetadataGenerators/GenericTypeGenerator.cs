using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics.MetadataGenerators;

internal class GenericTypeGenerator
{
    private record Item(TypeMetadata Metadata, GenericTypeNode Node);

    private readonly HashSet<Item> typesToProcess;

    public GenericTypeGenerator()
        => typesToProcess = [];

    public void CreateGenericTypes(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsClosedGenericType)
                continue;

            if (symbol.Node is not GenericTypeNode genericTypeNode)
                throw new SemanticAnalysisException($"Expected '{symbol.Name}' to have a GenericTypeNode, but found '{symbol.Node.GetType().Name}' instead.");

            var typeProvider = genericTypeNode.SymbolTable!.TypeProvider;

            // ignore open generic types
            if (IsOpenGeneric(typeProvider, genericTypeNode))
                continue;

            var metadata = new TypeMetadata(symbol.Name);
            if (typeProvider.DefineType(symbol.Name, metadata))
                typesToProcess.Add(new Item(metadata, genericTypeNode));
        }
    }

    public void PopulateGenericTypes()
    {
        foreach (var (type, genericTypeNode) in typesToProcess)
        {
            var typeProvider = genericTypeNode.SymbolTable!.TypeProvider;
            var openName = genericTypeNode.GetOpenGenericName();
            var openGenericType = typeProvider.GetType(openName) as TypeMetadata ??
                                  throw new SemanticAnalysisException($"The '{openName}' generic type is not defined.");

            foreach (var argumentNode in genericTypeNode.TypeArguments)
            {
                var typeArgument = typeProvider.GetType(argumentNode.Name) ??
                                   throw new SemanticAnalysisException($"The '{argumentNode.Name}' type argument is not defined.");

                type.AddGenericArgument(typeArgument);
            }

            var typeArgumentsMap = TypeArgumentMap.Create(
                typeProvider,
                type.GenericArguments,
                openGenericType.GenericArguments);

            foreach (var @interface in openGenericType.Interfaces)
            {
                // TODO: support generic interfaces
                var aliasMetadata = typeProvider.GetType(@interface.ToString()) as TypeAliasMetadata ??
                                    throw new SemanticAnalysisException($"The '{@interface}' interface is not defined.");

                var interfaceMetadata = aliasMetadata.Type as InterfaceMetadata ??
                                        throw new SemanticAnalysisException($"The '{@interface}' interface is not an interface.");

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
                var parameters = methodType.ParameterTypes.Select(x => typeArgumentsMap.Map(x));
                var returnType = typeArgumentsMap.Map(methodType.ReturnType);
                var functionType = new FunctionTypeMetadata(parameters, returnType);
                if (typeProvider.GetType(functionType.ToString()) is not FunctionTypeMetadata existingFunctionType)
                {
                    typeProvider.DefineType(functionType.ToString(), functionType);
                    existingFunctionType = functionType;
                }

                var methodMetadata = new MethodMetadata(
                    type,
                    method.AccessModifier,
                    method.Name,
                    existingFunctionType);

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
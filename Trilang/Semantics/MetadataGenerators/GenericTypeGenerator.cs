using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Trilang.Semantics.MetadataGenerators;

internal class GenericTypeGenerator
{
    private record Item(ITypeMetadata Closed, ITypeMetadata Open, GenericTypeNode Node);

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

            var openName = genericTypeNode.GetOpenGenericName();
            var openGenericType = typeProvider.GetType(openName);
            var closedType = default(ITypeMetadata);

            if (openGenericType is TypeMetadata type)
                closedType = new TypeMetadata(type.Name);
            else if (openGenericType is TypeAliasMetadata alias)
                closedType = new TypeAliasMetadata(alias.Name);
            else
                throw new SemanticAnalysisException($"The '{openName}' generic type is not defined.");

            if (typeProvider.DefineType(symbol.Name, closedType))
                typesToProcess.Add(new Item(closedType, openGenericType, genericTypeNode));
        }
    }

    public void PopulateGenericTypes()
    {
        foreach (var (closed, open, genericTypeNode) in typesToProcess)
        {
            if (closed is TypeMetadata closedType && open is TypeMetadata openType)
                PopulateClosedTypes(genericTypeNode, closedType, openType);
            else if (closed is TypeAliasMetadata closedAlias && open is TypeAliasMetadata openAlias)
                PopulateClosedTypes(genericTypeNode, closedAlias, openAlias);
            else
                throw new SemanticAnalysisException($"The '{genericTypeNode.Name}' generic type is not defined.");
        }
    }

    private void PopulateClosedTypes(
        GenericTypeNode genericTypeNode,
        TypeMetadata closed,
        TypeMetadata open)
    {
        var typeProvider = genericTypeNode.SymbolTable!.TypeProvider;

        foreach (var argumentNode in genericTypeNode.TypeArguments)
        {
            var typeArgument = typeProvider.GetType(argumentNode.Name) ??
                               throw new SemanticAnalysisException($"The '{argumentNode.Name}' type argument is not defined.");

            closed.AddGenericArgument(typeArgument);
        }

        var typeArgumentsMap = TypeArgumentMap.Create(
            typeProvider,
            closed.GenericArguments,
            open.GenericArguments);

        foreach (var @interface in open.Interfaces)
        {
            // TODO: support generic interfaces
            var aliasMetadata = typeProvider.GetType(@interface.ToString()) as TypeAliasMetadata ??
                                throw new SemanticAnalysisException($"The '{@interface}' interface is not defined.");

            var interfaceMetadata = aliasMetadata.Type as InterfaceMetadata ??
                                    throw new SemanticAnalysisException($"The '{@interface}' interface is not an interface.");

            closed.AddInterface(interfaceMetadata);
        }

        foreach (var property in open.Properties)
        {
            var propertyType = typeArgumentsMap.Map(property.Type);
            var propertyMetadata = new PropertyMetadata(
                closed,
                property.Name,
                propertyType,
                property.GetterModifier,
                property.SetterModifier);

            closed.AddProperty(propertyMetadata);
        }

        foreach (var constructor in open.Constructors)
        {
            var parameters = GetParameters(typeArgumentsMap, constructor.Parameters);

            var functionType = new FunctionTypeMetadata(parameters.Select(x => x.Type), closed);
            functionType = typeProvider.GetOrDefine(functionType);

            var constructorMetadata = new ConstructorMetadata(
                closed,
                constructor.AccessModifier,
                parameters,
                functionType);

            closed.AddConstructor(constructorMetadata);
        }

        foreach (var method in open.Methods)
        {
            // TODO: support generic methods
            var parameters = GetParameters(typeArgumentsMap, method.Parameters);

            var methodType = method.TypeMetadata;
            var parameterTypes = methodType.ParameterTypes.Select(x => typeArgumentsMap.Map(x));
            var returnType = typeArgumentsMap.Map(methodType.ReturnType);
            var functionType = new FunctionTypeMetadata(parameterTypes, returnType);
            functionType = typeProvider.GetOrDefine(functionType);

            var methodMetadata = new MethodMetadata(
                closed,
                method.AccessModifier,
                method.IsStatic,
                method.Name,
                parameters,
                functionType);

            closed.AddMethod(methodMetadata);
        }
    }

    private void PopulateClosedTypes(
        GenericTypeNode genericTypeNode,
        TypeAliasMetadata closed,
        TypeAliasMetadata open)
    {
        var typeProvider = genericTypeNode.SymbolTable!.TypeProvider;

        foreach (var argumentNode in genericTypeNode.TypeArguments)
        {
            var typeArgument = typeProvider.GetType(argumentNode.Name) ??
                               throw new SemanticAnalysisException($"The '{argumentNode.Name}' type argument is not defined.");

            closed.AddGenericArgument(typeArgument);
        }

        var typeArgumentsMap = TypeArgumentMap.Create(
            typeProvider,
            closed.GenericArguments,
            open.GenericArguments);

        closed.Type = typeArgumentsMap.Map(open.Type!);
    }

    private ParameterMetadata[] GetParameters(TypeArgumentMap map, IReadOnlyList<ParameterMetadata> parameters)
    {
        var parametersMetadata = new ParameterMetadata[parameters.Count];
        for (var i = 0; i < parametersMetadata.Length; i++)
            parametersMetadata[i] = new ParameterMetadata(
                parametersMetadata[i].Name,
                map.Map(parameters[i].Type)
            );

        return parametersMetadata;
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
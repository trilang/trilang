using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class GenericTypeGenerator
{
    private record Item(ITypeMetadata Closed, ITypeMetadata Open, GenericType Node);

    private readonly SymbolTableMap symbolTableMap;
    private readonly HashSet<Item> typesToProcess;

    public GenericTypeGenerator(SymbolTableMap symbolTableMap)
    {
        this.symbolTableMap = symbolTableMap;
        typesToProcess = [];
    }

    public void CreateGenericTypes(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (!symbol.IsClosedGenericType)
                continue;

            if (symbol.Node is not GenericType genericTypeNode)
                throw new SemanticAnalysisException($"Expected '{symbol.Name}' to have a GenericTypeNode, but found '{symbol.Node.GetType().Name}' instead.");

            var typeProvider = symbolTableMap.Get(genericTypeNode).TypeProvider;

            // ignore open generic types
            if (IsOpenGeneric(typeProvider, genericTypeNode))
                continue;

            var openName = genericTypeNode.GetOpenGenericName();
            var openGenericType = typeProvider.GetType(openName);
            var closedType = openGenericType switch
            {
                TypeMetadata type
                    => (ITypeMetadata)new TypeMetadata(openGenericType.Definition, type.Name),

                TypeAliasMetadata alias
                    => new TypeAliasMetadata(openGenericType.Definition, alias.Name),

                _ => throw new SemanticAnalysisException($"The '{openName}' generic type is not defined."),
            };

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
        GenericType genericTypeNode,
        TypeMetadata closed,
        TypeMetadata open)
    {
        var typeProvider = symbolTableMap.Get(genericTypeNode).TypeProvider;

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
            var getter = default(MethodMetadata);
            if (property.Getter is not null)
                getter = PopulateClosedMethod(typeProvider, typeArgumentsMap, closed, property.Getter);

            var setter = default(MethodMetadata);
            if (property.Setter is not null)
                setter = PopulateClosedMethod(typeProvider, typeArgumentsMap, closed, property.Setter);

            var propertyMetadata = new PropertyMetadata(
                closed,
                property.Name,
                propertyType,
                getter,
                setter);

            closed.AddProperty(propertyMetadata);
        }

        foreach (var constructor in open.Constructors)
        {
            var parameters = GetParameters(typeArgumentsMap, constructor.Parameters);

            var functionType = new FunctionTypeMetadata(null, parameters.Select(x => x.Type), closed);
            functionType = typeProvider.GetOrDefine(functionType);

            var constructorMetadata = new ConstructorMetadata(
                constructor.Definition,
                closed,
                constructor.AccessModifier,
                parameters,
                functionType);

            closed.AddConstructor(constructorMetadata);
        }

        foreach (var method in open.Methods)
        {
            var methodMetadata = PopulateClosedMethod(typeProvider, typeArgumentsMap, closed, method);
            closed.AddMethod(methodMetadata);
        }
    }

    private MethodMetadata PopulateClosedMethod(
        ITypeMetadataProvider typeProvider,
        TypeArgumentMap typeArgumentsMap,
        TypeMetadata closed,
        MethodMetadata method)
    {
        // TODO: support generic methods
        var parameters = GetParameters(typeArgumentsMap, method.Parameters);

        var methodType = method.Type;
        var parameterTypes = methodType.ParameterTypes.Select(typeArgumentsMap.Map);
        var returnType = typeArgumentsMap.Map(methodType.ReturnType);
        var functionType = new FunctionTypeMetadata(null, parameterTypes, returnType);
        functionType = typeProvider.GetOrDefine(functionType);

        return new MethodMetadata(
            method.Definition,
            closed,
            method.AccessModifier,
            method.IsStatic,
            method.Name,
            parameters,
            functionType);
    }

    private void PopulateClosedTypes(
        GenericType genericTypeNode,
        TypeAliasMetadata closed,
        TypeAliasMetadata open)
    {
        var typeProvider = symbolTableMap.Get(genericTypeNode).TypeProvider;

        foreach (var argumentNode in genericTypeNode.TypeArguments)
        {
            var typeArgument = typeProvider.GetType(argumentNode.Name) ??
                               throw new SemanticAnalysisException($"The '{argumentNode.Name}' type argument is not defined.");

            closed.AddGenericArgument(typeArgument);
        }

        if (open.IsInvalid)
        {
            closed.MarkAsInvalid();
            return;
        }

        var typeArgumentsMap = TypeArgumentMap.Create(
            typeProvider,
            closed.GenericArguments,
            open.GenericArguments);

        closed.Type = typeArgumentsMap.Map(open.Type!);
    }

    private ParameterMetadata[] GetParameters(
        TypeArgumentMap map,
        IReadOnlyList<ParameterMetadata> parameters)
    {
        var parametersMetadata = new ParameterMetadata[parameters.Count];
        for (var i = 0; i < parametersMetadata.Length; i++)
        {
            var parameterMetadata = parameters[i];

            parametersMetadata[i] = new ParameterMetadata(
                parameterMetadata.Definition,
                parameterMetadata.Name,
                map.Map(parameterMetadata.Type)
            );
        }

        return parametersMetadata;
    }

    private bool IsOpenGeneric(ITypeMetadataProvider typeProvider, GenericType genericTypeNode)
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
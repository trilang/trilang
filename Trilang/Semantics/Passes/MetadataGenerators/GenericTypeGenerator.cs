using System.Diagnostics;
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

            var genericTypeNode = (GenericType)symbol.Node;
            var typeProvider = symbolTableMap.Get(genericTypeNode).TypeProvider;

            // ignore open generic types
            if (IsOpenGeneric(typeProvider, genericTypeNode))
                continue;

            var openName = genericTypeNode.GetOpenGenericName();
            var openGenericType = typeProvider.GetType(openName);
            var closedType = default(ITypeMetadata);
            if (openGenericType is TypeMetadata type)
                closedType = new TypeMetadata(openGenericType.Definition, type.Name);
            else if (openGenericType is TypeAliasMetadata alias)
                closedType = new TypeAliasMetadata(openGenericType.Definition, alias.Name);
            else
                continue;

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
                Debug.Fail($"The '{genericTypeNode.Name}' generic type is not defined.");
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
                               TypeArgumentMetadata.Invalid(argumentNode.Name);

            closed.AddGenericArgument(typeArgument);
        }

        var typeArgumentsMap = TypeArgumentMap.Create(
            typeProvider,
            closed.GenericArguments,
            open.GenericArguments);

        foreach (var @interface in open.Interfaces)
        {
            // TODO: support generic interfaces
            var interfaceMetadata = default(InterfaceMetadata);
            var aliasMetadata = typeProvider.GetType(@interface.ToString()) as TypeAliasMetadata;
            if (aliasMetadata is not null)
                interfaceMetadata = aliasMetadata.Type as InterfaceMetadata;

            interfaceMetadata ??= InterfaceMetadata.Invalid();

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
                               TypeArgumentMetadata.Invalid(argumentNode.Name);

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
            var typeArgument = typeProvider.GetType(argumentNode.Name);
            if (typeArgument is null or not TypeArgumentMetadata)
            {
                isOpenGeneric = false;
                break;
            }
        }

        return isOpenGeneric;
    }
}
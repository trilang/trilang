using System.Diagnostics;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class GenericTypeGenerator
{
    private record Item(ITypeMetadata Open, GenericType Node);

    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly MetadataProviderMap metadataProviderMap;
    private readonly HashSet<Item> typesToProcess;

    public GenericTypeGenerator(
        SemanticDiagnosticReporter diagnostics,
        MetadataProviderMap metadataProviderMap)
    {
        this.diagnostics = diagnostics;
        this.metadataProviderMap = metadataProviderMap;
        typesToProcess = [];
    }

    public void CreateGenericTypes(IReadOnlyList<TypeSymbol> types)
    {
        foreach (var symbol in types)
        {
            if (!symbol.IsGenericType)
                continue;

            var node = (GenericType)symbol.Node;
            var typeProvider = metadataProviderMap.Get(node);

            // ignore open generic types
            if (IsOpenGeneric(typeProvider, node))
                continue;

            var closedType = typeProvider.GetType(symbol.Name);
            if (closedType is null)
            {
                var openName = node.GetOpenGenericName();
                var openGenericType = typeProvider.GetType(openName);
                if (openGenericType is TypeMetadata type)
                    closedType = new TypeMetadata(openGenericType.Definition, type.Name);
                else if (openGenericType is AliasMetadata alias)
                    closedType = new AliasMetadata(openGenericType.Definition, alias.Name);
                else
                    Debug.Fail($"The '{openName}' generic type is not supported.");

                if (typeProvider.DefineType(symbol.Name, closedType))
                    typesToProcess.Add(new Item(openGenericType, node));
            }

            node.Metadata = closedType;
        }
    }

    public void PopulateGenericTypes()
    {
        foreach (var (open, genericTypeNode) in typesToProcess)
        {
            var closed = genericTypeNode.Metadata!;
            if (closed is TypeMetadata closedType && open is TypeMetadata openType)
                PopulateClosedTypes(genericTypeNode, closedType, openType);
            else if (closed is AliasMetadata closedAlias && open is AliasMetadata openAlias)
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
        var typeProvider = metadataProviderMap.Get(genericTypeNode);

        foreach (var argumentNode in genericTypeNode.TypeArguments)
            closed.AddGenericArgument(argumentNode.PopulateMetadata(typeProvider, diagnostics));

        var typeArgumentsMap = TypeArgumentMap.Create(
            typeProvider,
            closed.GenericArguments,
            open.GenericArguments);

        foreach (var @interface in open.Interfaces)
        {
            // TODO: support generic interfaces
            var interfaceMetadata = default(InterfaceMetadata);
            var aliasMetadata = typeProvider.GetType(@interface.ToString()) as AliasMetadata;
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
                getter = PopulateClosedMethod(
                    typeProvider,
                    typeArgumentsMap,
                    closed,
                    property.Getter,
                    new FunctionGroupMetadata());

            var setter = default(MethodMetadata);
            if (property.Setter is not null)
                setter = PopulateClosedMethod(
                    typeProvider,
                    typeArgumentsMap,
                    closed,
                    property.Setter,
                    new FunctionGroupMetadata());

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

        foreach (var methods in open.Methods.GroupBy(x => x.Name))
        {
            var group = new FunctionGroupMetadata();

            foreach (var method in methods)
                closed.AddMethod(
                    PopulateClosedMethod(
                        typeProvider,
                        typeArgumentsMap,
                        closed,
                        method,
                        group));
        }
    }

    private MethodMetadata PopulateClosedMethod(IMetadataProvider provider,
        TypeArgumentMap typeArgumentsMap,
        TypeMetadata closed,
        MethodMetadata method,
        FunctionGroupMetadata group)
    {
        // TODO: support generic methods
        var parameters = GetParameters(typeArgumentsMap, method.Parameters);

        var methodType = method.Type;
        var parameterTypes = methodType.ParameterTypes.Select(typeArgumentsMap.Map);
        var returnType = typeArgumentsMap.Map(methodType.ReturnType);
        var functionType = new FunctionTypeMetadata(null, parameterTypes, returnType);
        functionType = provider.GetOrDefine(functionType);

        return new MethodMetadata(
            method.Definition,
            closed,
            method.AccessModifier,
            method.IsStatic,
            method.Name,
            parameters,
            functionType,
            group);
    }

    private void PopulateClosedTypes(
        GenericType genericTypeNode,
        AliasMetadata closed,
        AliasMetadata open)
    {
        var typeProvider = metadataProviderMap.Get(genericTypeNode);

        foreach (var argumentNode in genericTypeNode.TypeArguments)
            closed.AddGenericArgument(argumentNode.PopulateMetadata(typeProvider, diagnostics));

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

    private bool IsOpenGeneric(IMetadataProvider metadataProvider, GenericType genericTypeNode)
    {
        var isOpenGeneric = true;
        foreach (var argumentNode in genericTypeNode.TypeArguments)
        {
            var typeArgument = metadataProvider.GetType(argumentNode.Name);
            if (typeArgument is null or not TypeArgumentMetadata)
            {
                isOpenGeneric = false;
                break;
            }
        }

        return isOpenGeneric;
    }
}
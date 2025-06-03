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
            var parameters = new ITypeMetadata[constructor.ParameterTypes.Count];
            for (var i = 0; i < parameters.Length; i++)
                parameters[i] = typeArgumentsMap.Map(constructor.ParameterTypes[i]);

            var constructorMetadata = new ConstructorMetadata(
                closed,
                constructor.AccessModifier,
                parameters);

            closed.AddConstructor(constructorMetadata);
        }

        foreach (var method in open.Methods)
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
                closed,
                method.AccessModifier,
                method.IsStatic,
                method.Name,
                existingFunctionType);

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
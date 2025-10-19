using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class TypeGenerator
{
    private record Item(TypeMetadata Metadata, TypeDeclaration Node);

    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly SymbolTableMap symbolTableMap;
    private readonly HashSet<Item> typesToProcess;

    public TypeGenerator(SemanticDiagnosticReporter diagnostics, SymbolTableMap symbolTableMap)
    {
        this.diagnostics = diagnostics;
        this.symbolTableMap = symbolTableMap;
        typesToProcess = [];
    }

    public void CreateTypes(IReadOnlyDictionary<string, TypeSymbol> types)
    {
        foreach (var (_, symbol) in types)
        {
            if (symbol is { IsType: false, IsGenericType: false })
                continue;

            var typeDeclarationNode = (TypeDeclaration)symbol.Node;
            var root = typeDeclarationNode.GetRoot();
            var typeProvider = symbolTableMap.Get(symbol.Node).TypeProvider;
            var metadata = new TypeMetadata(
                new SourceLocation(root.SourceFile, typeDeclarationNode.SourceSpan.GetValueOrDefault()),
                typeDeclarationNode.Name);

            foreach (var genericArgument in typeDeclarationNode.GenericArguments)
            {
                var argumentMetadata = new TypeArgumentMetadata(
                    new SourceLocation(root.SourceFile, genericArgument.SourceSpan.GetValueOrDefault()),
                    genericArgument.Name) as ITypeMetadata;

                if (!typeProvider.DefineType(genericArgument.Name, argumentMetadata))
                {
                    argumentMetadata = TypeArgumentMetadata.Invalid(genericArgument.Name);
                    diagnostics.TypeArgumentAlreadyDefined(genericArgument);
                }

                metadata.AddGenericArgument(argumentMetadata);
            }

            if (typeProvider.DefineType(symbol.Name, metadata))
                typesToProcess.Add(new Item(metadata, typeDeclarationNode));
            else
                diagnostics.TypeAlreadyDefined(typeDeclarationNode);
        }
    }

    public void PopulateTypes()
    {
        foreach (var (type, typeDeclarationNode) in typesToProcess)
        {
            var root = typeDeclarationNode.GetRoot();
            var typeProvider = symbolTableMap.Get(typeDeclarationNode).TypeProvider;

            foreach (var @interface in typeDeclarationNode.Interfaces)
            {
                // TODO: support generic interfaces
                var interfaceMetadata = default(InterfaceMetadata);
                var aliasMetadata = typeProvider.GetType(@interface.Name) as TypeAliasMetadata;
                if (aliasMetadata is not null)
                    interfaceMetadata = aliasMetadata.Type as InterfaceMetadata;

                interfaceMetadata ??= InterfaceMetadata.Invalid();

                type.AddInterface(interfaceMetadata);
            }

            foreach (var property in typeDeclarationNode.Properties)
            {
                var propertyType = typeProvider.GetType(property.Type.Name) ??
                                   TypeMetadata.Invalid(property.Type.Name);

                var propertyMetadata = new PropertyMetadata(
                    new SourceLocation(root.SourceFile, property.SourceSpan.GetValueOrDefault()),
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
                                     TypeMetadata.Invalid(x.Type.Name));
                    var functionType = new FunctionTypeMetadata(null, parameters, type);
                    functionType = typeProvider.GetOrDefine(functionType);

                    var parametersMetadata = GetParameters(root, typeProvider, constructor.Parameters);
                    var constructorMetadata = new ConstructorMetadata(
                        new SourceLocation(root.SourceFile, constructor.SourceSpan.GetValueOrDefault()),
                        type,
                        GetAccessModifierMetadata(constructor.AccessModifier) ?? AccessModifierMetadata.Public,
                        parametersMetadata,
                        functionType);

                    type.AddConstructor(constructorMetadata);
                }
            }
            else
            {
                var functionType = new FunctionTypeMetadata(null, [], type);
                functionType = typeProvider.GetOrDefine(functionType);

                type.AddConstructor(new ConstructorMetadata(
                    null,
                    type,
                    AccessModifierMetadata.Public,
                    [],
                    functionType
                ));
            }

            foreach (var method in typeDeclarationNode.Methods)
            {
                var parameters = GetParameters(root, typeProvider, method.Parameters);
                var parameterTypes = method.Parameters
                    .Select(x => typeProvider.GetType(x.Type.Name) ??
                                 TypeMetadata.Invalid(x.Type.Name));

                var returnType = typeProvider.GetType(method.ReturnType.Name) ??
                                 TypeMetadata.Invalid(method.ReturnType.Name);

                var functionType = new FunctionTypeMetadata(null, parameterTypes, returnType);
                functionType = typeProvider.GetOrDefine(functionType);

                var methodMetadata = new MethodMetadata(
                    new SourceLocation(root.SourceFile, method.SourceSpan.GetValueOrDefault()),
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
        SemanticTree root,
        ITypeMetadataProvider typeProvider,
        IReadOnlyList<Parameter> parameters)
    {
        var result = new ParameterMetadata[parameters.Count];
        for (var i = 0; i < parameters.Count; i++)
        {
            var parameter = parameters[i];
            var parameterType = typeProvider.GetType(parameter.Type.Name) ??
                                TypeMetadata.Invalid(parameter.Type.Name);

            result[i] = new ParameterMetadata(
                new SourceLocation(root.SourceFile, parameter.SourceSpan.GetValueOrDefault()),
                parameter.Name,
                parameterType);
        }

        return result;
    }

    private static AccessModifierMetadata? GetAccessModifierMetadata(AccessModifier? accessModifier)
        => accessModifier switch
        {
            AccessModifier.Public => AccessModifierMetadata.Public,
            AccessModifier.Internal => AccessModifierMetadata.Internal,
            AccessModifier.Private => AccessModifierMetadata.Private,

            null => null,
            _ => throw new ArgumentOutOfRangeException(nameof(accessModifier), accessModifier, null)
        };
}
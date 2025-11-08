using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Symbols;
using Type = Trilang.Semantics.Model.Type;

namespace Trilang.Semantics.Passes.MetadataGenerators;

internal class TypeGenerator
{
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly SymbolTableMap symbolTableMap;
    private readonly HashSet<TypeDeclaration> typesToProcess;

    public TypeGenerator(SemanticDiagnosticReporter diagnostics, SymbolTableMap symbolTableMap)
    {
        this.diagnostics = diagnostics;
        this.symbolTableMap = symbolTableMap;
        typesToProcess = [];
    }

    public void CreateTypes(IReadOnlyList<TypeSymbol> types)
    {
        foreach (var symbol in types)
        {
            if (symbol is { IsTypeDeclaration: false, IsGenericTypeDeclaration: false })
                continue;

            var typeProvider = symbolTableMap.Get(symbol.Node).TypeProvider;
            var node = (TypeDeclaration)symbol.Node;
            var root = node.GetRoot();
            var metadata = new TypeMetadata(
                new SourceLocation(root.SourceFile, node.SourceSpan ?? default),
                node.Name);

            foreach (var genericArgument in node.GenericArguments)
            {
                var argumentMetadata = new TypeArgumentMetadata(
                    new SourceLocation(root.SourceFile, genericArgument.SourceSpan ?? default),
                    genericArgument.Name) as ITypeMetadata;

                if (!typeProvider.DefineType(genericArgument.Name, argumentMetadata))
                {
                    argumentMetadata = TypeArgumentMetadata.Invalid(genericArgument.Name);
                    diagnostics.TypeArgumentAlreadyDefined(genericArgument);
                }

                metadata.AddGenericArgument(argumentMetadata);
                genericArgument.Metadata = argumentMetadata;
            }

            if (!typeProvider.DefineType(symbol.Name, metadata))
            {
                diagnostics.TypeAlreadyDefined(node);
                metadata.MarkAsInvalid();
            }

            node.Metadata = metadata;

            typesToProcess.Add(node);
        }
    }

    public void PopulateTypes()
    {
        foreach (var node in typesToProcess)
        {
            var type = node.Metadata!;
            var root = node.GetRoot();
            var typeProvider = symbolTableMap.Get(node).TypeProvider;

            foreach (var @interface in node.Interfaces)
                PopulateInterface(typeProvider, type, @interface);

            foreach (var property in node.Properties)
                PopulateProperty(typeProvider, root, type, property);

            if (node.Constructors.Count > 0)
            {
                foreach (var constructor in node.Constructors)
                    PopulateConstructor(typeProvider, root, type, constructor);
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

            foreach (var method in node.Methods)
                PopulateMethod(typeProvider, root, type, method);
        }
    }

    private void PopulateInterface(
        ITypeMetadataProvider typeProvider,
        TypeMetadata type,
        Type @interface)
    {
        // TODO: support generic interfaces
        var interfaceMetadata = default(InterfaceMetadata);
        if (typeProvider.GetType(@interface.Name) is TypeAliasMetadata aliasMetadata)
            interfaceMetadata = aliasMetadata.Type as InterfaceMetadata;

        if (interfaceMetadata is null)
        {
            interfaceMetadata = InterfaceMetadata.Invalid();
            diagnostics.UnknownType(@interface);
        }

        type.AddInterface(interfaceMetadata);
        @interface.Metadata = interfaceMetadata;
    }

    private void PopulateProperty(
        ITypeMetadataProvider typeProvider,
        SemanticTree root,
        TypeMetadata type,
        PropertyDeclaration property)
    {
        var propertyTypeMetadata = property.Type.PopulateMetadata(typeProvider, diagnostics);
        var propertyMetadata = new PropertyMetadata(
            new SourceLocation(root.SourceFile, property.SourceSpan.GetValueOrDefault()),
            type,
            property.Name,
            propertyTypeMetadata,
            property.Getter?.AccessModifier.ToMetadata(),
            property.Setter?.AccessModifier.ToMetadata());

        if (type.GetProperty(property.Name) is not null)
        {
            propertyMetadata.MarkAsInvalid();
            diagnostics.PropertyAlreadyDefined(property);
        }

        // TODO: add in a constructor?
        type.AddProperty(propertyMetadata);
        property.Metadata = propertyMetadata;

        if (propertyMetadata.Getter is not null)
        {
            type.AddMethod(propertyMetadata.Getter);
            typeProvider.DefineType(propertyMetadata.Getter.Type);

            if (property.Getter is not null)
                property.Getter.Metadata = propertyMetadata.Getter;
        }

        if (propertyMetadata.Setter is not null)
        {
            type.AddMethod(propertyMetadata.Setter);
            typeProvider.DefineType(propertyMetadata.Setter.Type);

            if (property.Setter is not null)
                property.Setter.Metadata = propertyMetadata.Setter;
        }
    }

    private void PopulateConstructor(
        ITypeMetadataProvider typeProvider,
        SemanticTree root,
        TypeMetadata type,
        ConstructorDeclaration constructor)
    {
        var parameters = GetParameters(root, typeProvider, constructor.Parameters);
        var functionType = new FunctionTypeMetadata(null, parameters.Select(x => x.Type), type);
        functionType = typeProvider.GetOrDefine(functionType);

        var constructorMetadata = new ConstructorMetadata(
            new SourceLocation(root.SourceFile, constructor.SourceSpan.GetValueOrDefault()),
            type,
            constructor.AccessModifier.ToMetadata(),
            parameters,
            functionType);

        type.AddConstructor(constructorMetadata);
        constructor.Metadata = constructorMetadata;
    }

    private void PopulateMethod(
        ITypeMetadataProvider typeProvider,
        SemanticTree root,
        TypeMetadata type,
        MethodDeclaration method)
    {
        var parameters = GetParameters(root, typeProvider, method.Parameters);
        var returnTypeMetadata = method.ReturnType.PopulateMetadata(typeProvider, diagnostics);

        var functionType = new FunctionTypeMetadata(
            null,
            parameters.Select(x => x.Type),
            returnTypeMetadata);
        functionType = typeProvider.GetOrDefine(functionType);

        var methodMetadata = new MethodMetadata(
            new SourceLocation(root.SourceFile, method.SourceSpan.GetValueOrDefault()),
            type,
            method.AccessModifier.ToMetadata(),
            method.IsStatic,
            method.Name,
            parameters,
            functionType);

        if (type.GetMethod(method.Name) is not null)
        {
            methodMetadata.MarkAsInvalid();
            diagnostics.MethodAlreadyDefined(method);
        }

        type.AddMethod(methodMetadata);
        method.Metadata = methodMetadata;
    }

    private ParameterMetadata[] GetParameters(
        SemanticTree root,
        ITypeMetadataProvider typeProvider,
        IReadOnlyList<Parameter> parameters)
    {
        var result = new ParameterMetadata?[parameters.Count];
        for (var i = 0; i < parameters.Count; i++)
        {
            var parameter = parameters[i];
            var parameterTypeMetadata = parameter.Type.PopulateMetadata(typeProvider, diagnostics);
            var parameterMetadata = new ParameterMetadata(
                new SourceLocation(root.SourceFile, parameter.SourceSpan.GetValueOrDefault()),
                parameter.Name,
                parameterTypeMetadata);

            var isDefined = result.Any(x => x?.Name == parameter.Name);
            if (isDefined)
            {
                parameterMetadata.MarkAsInvalid();
                diagnostics.ParameterAlreadyDefined(parameter);
            }

            result[i] = parameterMetadata;
            parameter.Metadata = parameterMetadata;
        }

        return result!;
    }
}
using System.Diagnostics;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes;

internal class MetadataGenerator : Visitor, ISemanticPass
{
    private SemanticDiagnosticReporter diagnostics = null!;
    private MetadataProviderMap metadataProviderMap = null!;
    private SymbolTableMap symbolTableMap = null!;

    private readonly HashSet<TypeDeclaration> typesToProcess;
    private readonly HashSet<AliasDeclaration> aliasToProcess;
    private readonly HashSet<FunctionDeclaration> functionsToProcess;
    private readonly HashSet<(GenericApplication, GenericApplicationMetadata)> genericToProcess;

    public MetadataGenerator()
    {
        typesToProcess = [];
        aliasToProcess = [];
        functionsToProcess = [];
        genericToProcess = [];
    }

    public void Analyze(IEnumerable<SemanticTree> semanticTrees, SemanticPassContext context)
    {
        diagnostics = context.Diagnostics;
        metadataProviderMap = context.MetadataProviderMap!;
        symbolTableMap = context.SymbolTableMap!;

        var rootSymbolTable = context.RootSymbolTable;
        var types = rootSymbolTable.Types;
        var ids = rootSymbolTable.Ids;

        CreateAliases(types);
        CreateTypes(types);
        CreateFunctions(ids);

        PopulateAliases();
        PopulateTypes();
        PopulateFunctions();

        foreach (var semanticTree in semanticTrees)
            semanticTree.Accept(this);

        CreateClosedGenericTypes();
    }

    private ITypeMetadata CreateTypeArgumentMetadata(TypeRef genericArgument)
    {
        var argumentMetadata = new TypeArgumentMetadata(
            genericArgument.GetLocation(),
            genericArgument.Name) as ITypeMetadata;

        var argumentProvider = metadataProviderMap.Get(genericArgument);
        if (!argumentProvider.DefineType(genericArgument.Name, argumentMetadata))
        {
            argumentMetadata = TypeArgumentMetadata.Invalid(genericArgument.Name);
            diagnostics.TypeArgumentAlreadyDefined(genericArgument);
        }

        genericArgument.Metadata = argumentMetadata;

        return argumentMetadata;
    }

    private void CreateTypes(IReadOnlyList<TypeSymbol> types)
    {
        foreach (var symbol in types)
        {
            if (symbol is { IsTypeDeclaration: false, IsGenericTypeDeclaration: false })
                continue;

            var node = (TypeDeclaration)symbol.Node;
            var metadata = new TypeMetadata(node.GetLocation(), node.Name);

            foreach (var genericArgument in node.GenericArguments)
                metadata.AddGenericArgument(CreateTypeArgumentMetadata(genericArgument));

            var metadataProvider = metadataProviderMap.Get(node);
            if (!metadataProvider.DefineType(symbol.Name, metadata))
            {
                diagnostics.TypeAlreadyDefined(node);
                metadata.MarkAsInvalid();
            }

            node.Metadata = metadata;

            typesToProcess.Add(node);
        }
    }

    private void PopulateTypes()
    {
        foreach (var node in typesToProcess)
        {
            var type = node.Metadata!;
            var root = node.GetRoot();
            var typeProvider = metadataProviderMap.Get(node);

            foreach (var @interface in node.Interfaces)
            {
                // TODO: support generic interfaces
                var interfaceMetadata = default(InterfaceMetadata);
                if (typeProvider.GetType(@interface.Name) is AliasMetadata aliasMetadata)
                    interfaceMetadata = aliasMetadata.Type as InterfaceMetadata;

                if (interfaceMetadata is null)
                {
                    interfaceMetadata = InterfaceMetadata.Invalid();
                    diagnostics.UnknownType(@interface);
                }

                type.AddInterface(interfaceMetadata);
                @interface.Metadata = interfaceMetadata;
            }

            foreach (var property in node.Properties)
            {
                var propertyMetadata = new PropertyMetadata(
                    new SourceLocation(root.SourceFile, property.SourceSpan.GetValueOrDefault()),
                    type,
                    property.Name,
                    GetOrCreateType(property.Type),
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

                    property.Getter?.Metadata = propertyMetadata.Getter;
                }

                if (propertyMetadata.Setter is not null)
                {
                    type.AddMethod(propertyMetadata.Setter);
                    typeProvider.DefineType(propertyMetadata.Setter.Type);

                    property.Setter?.Metadata = propertyMetadata.Setter;
                }
            }

            if (node.Constructors.Count > 0)
            {
                foreach (var constructor in node.Constructors)
                {
                    var parameters = GetParameters(root, constructor.Parameters);
                    var functionType = new FunctionTypeMetadata(
                        null,
                        parameters.Select(x => x.Type),
                        TypeMetadata.Void);
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
            }
            else
            {
                var functionType = new FunctionTypeMetadata(null, [], TypeMetadata.Void);
                functionType = typeProvider.GetOrDefine(functionType);

                type.AddConstructor(new ConstructorMetadata(
                    null,
                    type,
                    AccessModifierMetadata.Public,
                    [],
                    functionType));
            }

            foreach (var methods in node.Methods.GroupBy(x => x.Name))
            {
                var group = new FunctionGroupMetadata();

                foreach (var method in methods)
                {
                    var parameters = GetParameters(root, method.Parameters);
                    var parameterTypes = parameters.Select(x => x.Type).ToArray();

                    var functionType = new FunctionTypeMetadata(
                        null,
                        parameterTypes,
                        GetOrCreateType(method.ReturnType));
                    functionType = typeProvider.GetOrDefine(functionType);

                    var methodMetadata = new MethodMetadata(
                        new SourceLocation(root.SourceFile, method.SourceSpan.GetValueOrDefault()),
                        type,
                        method.AccessModifier.ToMetadata(),
                        method.IsStatic,
                        method.Name,
                        parameters,
                        functionType,
                        group);

                    if (type.GetMethods(method.Name, parameterTypes).Any())
                    {
                        methodMetadata.MarkAsInvalid();
                        diagnostics.MethodAlreadyDefined(method);
                    }

                    type.AddMethod(methodMetadata);
                    method.Metadata = methodMetadata;
                }
            }
        }
    }

    private ParameterMetadata[] GetParameters(SemanticTree root, IReadOnlyList<Parameter> parameters)
    {
        var result = new ParameterMetadata?[parameters.Count];
        for (var i = 0; i < parameters.Count; i++)
        {
            var parameter = parameters[i];
            var parameterMetadata = new ParameterMetadata(
                new SourceLocation(root.SourceFile, parameter.SourceSpan.GetValueOrDefault()),
                parameter.Name,
                GetOrCreateType(parameter.Type));

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

    private void CreateAliases(IReadOnlyList<TypeSymbol> types)
    {
        foreach (var symbol in types)
        {
            if (!symbol.IsAlias)
                continue;

            var node = (AliasDeclaration)symbol.Node;
            var metadata = new AliasMetadata(node.GetLocation(), node.Name);

            foreach (var genericArgument in node.GenericArguments)
                metadata.AddGenericArgument(CreateTypeArgumentMetadata(genericArgument));

            var metadataProvider = metadataProviderMap.Get(node);
            if (!metadataProvider.DefineType(symbol.Name, metadata))
            {
                diagnostics.TypeAlreadyDefined(node);
                metadata.MarkAsInvalid();
            }

            node.Metadata = metadata;

            aliasToProcess.Add(node);
        }
    }

    private void PopulateAliases()
    {
        foreach (var aliasNode in aliasToProcess)
        {
            var metadata = (AliasMetadata)aliasNode.Metadata!;

            metadata.Type = GetOrCreateType(aliasNode.Type);
        }
    }

    private void CreateFunctions(IReadOnlyDictionary<string, IdSymbol> functions)
    {
        foreach (var (_, symbol) in functions)
        {
            var group = new FunctionGroupMetadata();

            foreach (var node in symbol.Nodes)
            {
                if (node is not FunctionDeclaration function)
                    continue;

                var metadataProvider = metadataProviderMap.Get(function);
                var metadata = new FunctionMetadata(
                    function.GetLocation(),
                    function.AccessModifier.ToMetadata(),
                    function.Name,
                    [],
                    null!,
                    group);

                metadataProvider.AddFunction(metadata);

                function.Metadata = metadata;

                functionsToProcess.Add(function);
            }
        }
    }

    private void PopulateFunctions()
    {
        foreach (var function in functionsToProcess)
        {
            var root = function.GetRoot();
            var metadata = function.Metadata!;
            var metadataProvider = metadataProviderMap.Get(function);

            foreach (var functionParameter in function.Parameters)
            {
                var parameter = new ParameterMetadata(
                    new SourceLocation(root.SourceFile, functionParameter.SourceSpan.GetValueOrDefault()),
                    functionParameter.Name,
                    GetOrCreateType(functionParameter.Type));

                var isParameterDefined = metadata.Parameters.Any(x => x.Name == parameter.Name);
                if (isParameterDefined)
                {
                    parameter.MarkAsInvalid();
                    diagnostics.ParameterAlreadyDefined(functionParameter);
                }

                functionParameter.Metadata = parameter;
                metadata.AddParameter(parameter);
            }

            var functionTypeMetadata = new FunctionTypeMetadata(
                null,
                metadata.Parameters.Select(x => x.Type),
                GetOrCreateType(function.ReturnType));

            if (metadataProvider.GetType(functionTypeMetadata.ToString()) is not FunctionTypeMetadata existingFunctionType)
            {
                metadataProvider.DefineType(functionTypeMetadata.ToString(), functionTypeMetadata);
                existingFunctionType = functionTypeMetadata;
            }

            metadata.Type = existingFunctionType;
        }

        foreach (var function in functionsToProcess)
        {
            // TODO: use type provider to check for existing functions
            var metadata = function.Metadata!;
            var group = metadata.Group;
            if (ReferenceEquals(metadata, group.Functions[0]))
                continue;

            var actualParameters = metadata.Parameters.Select(x => x.Type).ToArray();
            var isDefined = group.Functions
                .Any(x => !ReferenceEquals(x, metadata) &&
                          x.Type.ParameterTypes.SequenceEqual(actualParameters));

            if (isDefined)
            {
                metadata.MarkAsInvalid();
                diagnostics.FunctionAlreadyDefined(function);
            }
        }
    }

    private void CreateClosedGenericTypes()
    {
        foreach (var (node, generic) in genericToProcess)
        {
            if (generic.ClosedGeneric is not null)
                continue;

            var metadataProvider = metadataProviderMap.Get(node);
            var map = new TypeArgumentMap(metadataProvider, generic);
            map.Map();
        }
    }

    private ITypeMetadata GetOrCreateType(IInlineType inlineType)
    {
        var typeProvider = metadataProviderMap.Get(inlineType);
        var metadata = typeProvider.GetType(inlineType.Name);
        if (metadata is null)
        {
            metadata = inlineType switch
            {
                ArrayType arrayType => CreateArray(arrayType),
                DiscriminatedUnion discriminatedUnion => CreateDiscriminatedUnion(discriminatedUnion),
                FunctionType functionType => CreateFunctionType(functionType),
                GenericApplication genericApplication => CreateGenericApplication(genericApplication),
                Interface @interface => CreateInterface(@interface),
                TupleType tupleType => CreateTuple(tupleType),

                TypeRef => null,

                _ => throw new ArgumentOutOfRangeException(nameof(inlineType)),
            };

            if (metadata is not null)
            {
                typeProvider.DefineType(inlineType.Name, metadata);
            }
            else
            {
                metadata = TypeMetadata.Invalid(inlineType.Name);
                diagnostics.UnknownType(inlineType);
            }
        }

        inlineType.Metadata = metadata;

        return metadata;
    }

    private ArrayMetadata CreateArray(ArrayType arrayType)
        => new ArrayMetadata(
            arrayType.GetLocation(),
            GetOrCreateType(arrayType.ElementType));

    private DiscriminatedUnionMetadata CreateDiscriminatedUnion(DiscriminatedUnion discriminatedUnion)
        => new DiscriminatedUnionMetadata(
            discriminatedUnion.GetLocation(),
            discriminatedUnion.Types.Select(GetOrCreateType));

    private FunctionTypeMetadata CreateFunctionType(FunctionType functionType)
        => new FunctionTypeMetadata(
            functionType.GetLocation(),
            functionType.ParameterTypes.Select(GetOrCreateType),
            GetOrCreateType(functionType.ReturnType));

    private ITypeMetadata CreateGenericApplication(GenericApplication genericApplication)
    {
        var typeProvider = metadataProviderMap.Get(genericApplication);

        foreach (var argumentNode in genericApplication.TypeArguments)
            GetOrCreateType(argumentNode);

        var openGenericName = genericApplication.GetOpenGenericName();
        if (typeProvider.GetType(openGenericName) is not IGenericMetadata openGeneric)
        {
            diagnostics.UnknownType(genericApplication);
            openGeneric = TypeMetadata.Invalid(openGenericName);
        }

        var metadata = new GenericApplicationMetadata(
            genericApplication.GetLocation(),
            openGeneric,
            genericApplication.TypeArguments.Select(x => x.Metadata!));

        metadata = typeProvider.GetOrDefine(metadata);
        genericToProcess.Add((genericApplication, metadata));

        return metadata;
    }

    private InterfaceMetadata CreateInterface(Interface @interface)
    {
        var typeProvider = metadataProviderMap.Get(@interface);
        var metadata = new InterfaceMetadata(@interface.GetLocation());

        foreach (var property in @interface.Properties)
        {
            var propertyMetadata = new InterfacePropertyMetadata(
                metadata.Definition! with { Span = property.SourceSpan.GetValueOrDefault() },
                metadata,
                property.Name,
                GetOrCreateType(property.Type),
                property.GetterModifier?.ToMetadata() ?? AccessModifierMetadata.Public,
                property.SetterModifier?.ToMetadata());

            if (metadata.GetProperty(property.Name) is not null)
            {
                propertyMetadata.MarkAsInvalid();
                diagnostics.InterfacePropertyAlreadyDefined(property);
            }

            metadata.AddProperty(propertyMetadata);
            property.Metadata = propertyMetadata;
        }

        foreach (var methods in @interface.Methods.GroupBy(x => x.Name))
        {
            var group = new FunctionGroupMetadata();

            foreach (var method in methods)
            {
                var parameters = new ITypeMetadata[method.ParameterTypes.Count];
                for (var i = 0; i < method.ParameterTypes.Count; i++)
                    parameters[i] = GetOrCreateType(method.ParameterTypes[i]);

                var functionType = new FunctionTypeMetadata(
                    null,
                    parameters,
                    GetOrCreateType(method.ReturnType));

                if (typeProvider.GetType(functionType.ToString()) is not FunctionTypeMetadata existingFunctionType)
                {
                    typeProvider.DefineType(functionType.ToString(), functionType);
                    existingFunctionType = functionType;
                }

                // TODO: generic?
                var methodMetadata = new InterfaceMethodMetadata(
                    metadata.Definition! with { Span = method.SourceSpan.GetValueOrDefault() },
                    metadata,
                    method.Name,
                    existingFunctionType,
                    group);

                if (metadata.GetMethods(method.Name, parameters).Any())
                {
                    methodMetadata.MarkAsInvalid();
                    diagnostics.InterfaceMethodAlreadyDefined(method);
                }

                metadata.AddMethod(methodMetadata);
                method.Metadata = methodMetadata;
            }
        }

        return metadata;
    }

    private TupleMetadata CreateTuple(TupleType tupleType)
        => new TupleMetadata(
            tupleType.GetLocation(),
            tupleType.Types.Select(GetOrCreateType));

    protected override void VisitArrayTypeEnter(ArrayType node)
    {
        if (node.Metadata is not null)
            return;

        GetOrCreateType(node);
    }

    protected override void VisitDiscriminatedUnionEnter(DiscriminatedUnion node)
    {
        if (node.Metadata is not null)
            return;

        GetOrCreateType(node);
    }

    protected override void VisitFunctionTypeEnter(FunctionType node)
    {
        if (node.Metadata is not null)
            return;

        GetOrCreateType(node);
    }

    protected override void VisitGenericTypeRefEnter(GenericApplication node)
    {
        if (node.Metadata is not null)
            return;

        GetOrCreateType(node);
    }

    protected override void VisitInterfaceEnter(Interface node)
    {
        if (node.Metadata is not null)
            return;

        GetOrCreateType(node);
    }

    protected override void VisitTupleTypeEnter(TupleType node)
    {
        if (node.Metadata is not null)
            return;

        GetOrCreateType(node);
    }

    protected override void VisitTypeRefEnter(TypeRef node)
    {
        if (node.Metadata is not null)
            return;

        GetOrCreateType(node);
    }

    protected override void VisitVariableExit(VariableDeclaration node)
    {
        Debug.Assert(node.Type.Metadata is not null);

        var symbolTable = symbolTableMap.Get(node);
        var metadata = new VariableMetadata(node.GetLocation(), node.Name, node.Type.Metadata);
        var symbol = symbolTable.GetId(node.Name);
        if (symbol is not null && symbol.Nodes[0] != node)
        {
            metadata.MarkAsInvalid();
            diagnostics.VariableAlreadyDefined(node);
        }

        node.Metadata = metadata;
    }

    public string Name
        => nameof(MetadataGenerator);

    public IEnumerable<string> DependsOn
        => [nameof(SymbolFinder), nameof(MetadataProviderAnalyzer)];
}
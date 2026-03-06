using System.Diagnostics;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Semantics.Providers;

namespace Trilang.Semantics.Passes;

internal class MetadataGenerator : Visitor, ISemanticPass
{
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly SymbolTableMap symbolTableMap;
    private readonly MetadataProviderMap metadataProviderMap;
    private readonly NamespaceMetadata rootNamespace;
    private readonly BuiltInTypes builtInTypes;

    private readonly HashSet<TypeDeclaration> typesToProcess;
    private readonly HashSet<AliasDeclaration> aliasToProcess;
    private readonly HashSet<FunctionDeclaration> functionsToProcess;
    private readonly HashSet<GenericApplicationMetadata> genericToProcess;

    public MetadataGenerator(
        ISet<string> directives,
        SemanticDiagnosticReporter diagnostics,
        SymbolTableMap symbolTableMap,
        MetadataProviderMap metadataProviderMap,
        NamespaceMetadata rootNamespace,
        BuiltInTypes builtInTypes)
        : base(directives)
    {
        this.diagnostics = diagnostics;
        this.symbolTableMap = symbolTableMap;
        this.metadataProviderMap = metadataProviderMap;
        this.rootNamespace = rootNamespace;
        this.builtInTypes = builtInTypes;

        typesToProcess = [];
        aliasToProcess = [];
        functionsToProcess = [];
        genericToProcess = [];
    }

    public void Analyze(IEnumerable<SemanticTree> semanticTrees)
    {
        var treesToAnalyze = semanticTrees as SemanticTree[] ??
                             semanticTrees.ToArray();

        var rootProvider = new FileMetadataProvider(rootNamespace);
        CreateNamespaces(treesToAnalyze, rootProvider);
        AddUses(treesToAnalyze, rootProvider);

        var (types, functions) = CollectTypesAndFunctions(treesToAnalyze);

        CreateAliases(types);
        CreateTypes(types);
        CreateGeneric(types);
        CreateFunctions(functions);

        PopulateAliases();
        PopulateTypes();
        PopulateFunctions();

        foreach (var semanticTree in treesToAnalyze)
            semanticTree.Accept(this);

        CreateClosedGenericTypes();
    }

    private void CreateNamespaces(
        SemanticTree[] treesToAnalyze,
        FileMetadataProvider rootProvider)
    {
        var visitor = new NamespaceMetadataGenerator(directives, rootProvider, metadataProviderMap);
        foreach (var tree in treesToAnalyze)
            tree.Accept(visitor);
    }

    private void AddUses(SemanticTree[] treesToAnalyze, FileMetadataProvider rootProvider)
    {
        var addNamespaceUses = new AddNamespaceUses(
            directives,
            diagnostics,
            rootProvider,
            metadataProviderMap);

        foreach (var tree in treesToAnalyze)
            tree.Accept(addNamespaceUses);
    }

    private (IReadOnlyList<TypeDescriptor>, IReadOnlyList<FunctionDeclaration>) CollectTypesAndFunctions(SemanticTree[] treesToAnalyze)
    {
        var collector = new CollectTypesVisitor(directives);
        foreach (var tree in treesToAnalyze)
            tree.Accept(collector);

        return (collector.Types, collector.Functions);
    }

    private ITypeMetadata CreateTypeArgumentMetadata(
        IGenericMetadata metadata,
        TypeRef genericArgument)
    {
        var argumentMetadata = new TypeArgumentMetadata(
            genericArgument.GetLocation(),
            genericArgument.Name) as ITypeMetadata;

        var argumentExists = metadata.GenericArguments
            .OfType<TypeArgumentMetadata>()
            .Any(x => x.Name == genericArgument.Name);

        if (argumentExists)
        {
            argumentMetadata = TypeArgumentMetadata.Invalid(genericArgument.Name);
            diagnostics.TypeArgumentAlreadyDefined(genericArgument);
        }

        genericArgument.Metadata = argumentMetadata;

        return argumentMetadata;
    }

    private void CreateTypes(IReadOnlyList<TypeDescriptor> types)
    {
        foreach (var symbol in types)
        {
            if (symbol is { IsTypeDeclaration: false, IsGenericDeclaration: false })
                continue;

            var node = (TypeDeclaration)symbol.Node;
            var metadata = new TypeMetadata(node.GetLocation(), node.Name);

            // create a new provider for generic
            if (node.GenericArguments.Count > 0)
                metadataProviderMap.Add(
                    node,
                    new GenericMetadataProvider(metadataProviderMap.Get(node), metadata));

            foreach (var genericArgument in node.GenericArguments)
                metadata.AddGenericArgument(CreateTypeArgumentMetadata(metadata, genericArgument));

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
            var metadataProvider = metadataProviderMap.Get(node);
            var metadataFactory = new MetadataFactory(builtInTypes, metadataProvider);

            foreach (var @interface in node.Interfaces)
            {
                // TODO: support generic interfaces
                var interfaceMetadata = default(InterfaceMetadata);
                var types = metadataProvider.FindTypes(@interface.Name);
                if (types is [AliasMetadata aliasMetadata])
                {
                    interfaceMetadata = aliasMetadata.UnpackAlias() as InterfaceMetadata;

                    if (interfaceMetadata is null)
                    {
                        interfaceMetadata = InterfaceMetadata.Invalid();
                        diagnostics.UnknownType(@interface);
                    }
                }
                else if (types is [_] or [])
                {
                    diagnostics.UnknownType(@interface);
                    interfaceMetadata = InterfaceMetadata.Invalid();
                }
                else
                {
                    diagnostics.MultipleMembersFound(@interface, types);
                    interfaceMetadata = InterfaceMetadata.Invalid();
                }

                type.AddInterface(interfaceMetadata);
                @interface.Metadata = interfaceMetadata;
            }

            foreach (var property in node.Properties)
            {
                var propertyMetadata = metadataFactory.CreatePropertyMetadata(
                    new SourceLocation(root.SourceFile, property.SourceSpan.GetValueOrDefault()),
                    type,
                    property.Name,
                    GetOrCreateType(property.Type),
                    property.Getter?.AccessModifier.ToMetadata(),
                    property.Setter?.AccessModifier.ToMetadata());

                if (!type.GetProperties(property.Name).IsEmpty)
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
                    property.Getter?.Metadata = propertyMetadata.Getter;
                }

                if (propertyMetadata.Setter is not null)
                {
                    type.AddMethod(propertyMetadata.Setter);
                    property.Setter?.Metadata = propertyMetadata.Setter;
                }
            }

            if (node.Constructors.Count > 0)
            {
                foreach (var constructor in node.Constructors)
                {
                    var parameters = GetParameters(root, constructor.Parameters);
                    var functionType = metadataFactory.CreateFunctionType(
                        null,
                        parameters.Select(x => x.Type),
                        builtInTypes.Void);

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
                var functionType = metadataFactory.CreateFunctionType(null, [], builtInTypes.Void);
                type.AddConstructor(new ConstructorMetadata(
                    null,
                    type,
                    AccessModifierMetadata.Public,
                    [],
                    functionType));
            }

            foreach (var methods in node.Methods.GroupBy(x => x.Name))
            {
                foreach (var method in methods)
                {
                    var parameters = GetParameters(root, method.Parameters);
                    var parameterTypes = parameters.Select(x => x.Type).ToArray();
                    var functionType = metadataFactory.CreateFunctionType(
                        null,
                        parameterTypes,
                        GetOrCreateType(method.ReturnType));

                    var methodMetadata = new MethodMetadata(
                        new SourceLocation(root.SourceFile, method.SourceSpan.GetValueOrDefault()),
                        type,
                        method.AccessModifier.ToMetadata(),
                        method.IsStatic,
                        method.Name,
                        parameters,
                        functionType);

                    if (type.GetMethods(method.Name).MatchFunction(parameterTypes).Any())
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

    private void CreateGeneric(IReadOnlyList<TypeDescriptor> types)
    {
        foreach (var symbol in types)
        {
            if (!symbol.IsGenericApplication)
                continue;

            CreateGenericApplication((GenericApplication)symbol.Node);
        }
    }

    private void CreateAliases(IReadOnlyList<TypeDescriptor> types)
    {
        foreach (var symbol in types)
        {
            if (!symbol.IsAlias)
                continue;

            var node = (AliasDeclaration)symbol.Node;
            var metadata = new AliasMetadata(node.GetLocation(), node.Name);

            // create a new provider for generic
            if (node.GenericArguments.Count > 0)
                metadataProviderMap.Add(
                    node,
                    new GenericMetadataProvider(metadataProviderMap.Get(node), metadata));

            foreach (var genericArgument in node.GenericArguments)
                metadata.AddGenericArgument(CreateTypeArgumentMetadata(metadata, genericArgument));

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

    private void CreateFunctions(IReadOnlyList<FunctionDeclaration> functions)
    {
        foreach (var function in functions)
        {
            var metadataProvider = metadataProviderMap.Get(function);
            var metadata = new FunctionMetadata(
                function.GetLocation(),
                function.AccessModifier.ToMetadata(),
                function.Name,
                [],
                null!);

            metadataProvider.AddFunction(metadata);

            function.Metadata = metadata;

            functionsToProcess.Add(function);
        }
    }

    private void PopulateFunctions()
    {
        foreach (var function in functionsToProcess)
        {
            var root = function.GetRoot();
            var metadata = function.Metadata!;
            var metadataProvider = metadataProviderMap.Get(function);
            var metadataFactory = new MetadataFactory(builtInTypes, metadataProvider);

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

            var functionTypeMetadata = metadataFactory.CreateFunctionType(
                null,
                metadata.Parameters.Select(x => x.Type),
                GetOrCreateType(function.ReturnType));

            metadata.Type = functionTypeMetadata;
        }

        foreach (var function in functionsToProcess)
        {
            var metadata = function.Metadata!;
            var metadataProvider = metadataProviderMap.Get(function);
            var functions = metadataProvider.FindFunctions(function.Name);

            // TODO: report all duplicates?
            if (ReferenceEquals(functions[0], metadata))
                continue;

            var actualParameters = metadata.Parameters.Select(x => x.Type).ToArray();
            var isDefined = functions
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
        foreach (var generic in genericToProcess)
        {
            if (generic.ClosedGeneric is not null)
                continue;

            var map = new TypeArgumentMap(builtInTypes, generic);
            map.Map();
        }
    }

    private ITypeMetadata GetOrCreateType(IInlineType inlineType)
    {
        var metadataProvider = metadataProviderMap.Get(inlineType);
        var types = metadataProvider.FindTypes(inlineType.Name);
        var metadata = default(ITypeMetadata);

        if (types is [])
        {
            if (inlineType is ArrayType arrayType)
            {
                metadata = CreateArray(arrayType);
            }
            else if (inlineType is DiscriminatedUnion discriminatedUnion)
            {
                metadata = CreateDiscriminatedUnion(discriminatedUnion);
            }
            else if (inlineType is FunctionType functionType)
            {
                metadata = CreateFunctionType(functionType);
            }
            else if (inlineType is GenericApplication genericApplication)
            {
                metadata = CreateGenericApplication(genericApplication);
            }
            else if (inlineType is Interface @interface)
            {
                metadata = CreateInterface(@interface);
            }
            else if (inlineType is TupleType tupleType)
            {
                metadata = CreateTuple(tupleType);
            }
            else if (inlineType is TypeRef)
            {
                metadata = TypeMetadata.Invalid(inlineType.Name);
                diagnostics.UnknownType(inlineType);
            }
            else
            {
                throw new ArgumentOutOfRangeException(nameof(inlineType));
            }
        }
        else if (types is [var type])
        {
            metadata = type;
        }
        else
        {
            diagnostics.MultipleMembersFound(inlineType, types);
            metadata = TypeMetadata.Invalid(inlineType.Name);
        }

        inlineType.Metadata = metadata;

        return metadata;
    }

    private ArrayMetadata CreateArray(ArrayType arrayType)
    {
        var metadataProvider = metadataProviderMap.Get(arrayType);
        var metadataFactory = new MetadataFactory(builtInTypes, metadataProvider);
        var metadata = metadataFactory.CreateArrayMetadata(
            arrayType.GetLocation(),
            GetOrCreateType(arrayType.ElementType));

        return metadata;
    }

    private DiscriminatedUnionMetadata CreateDiscriminatedUnion(DiscriminatedUnion discriminatedUnion)
    {
        var metadataProvider = metadataProviderMap.Get(discriminatedUnion);
        var metadata = new DiscriminatedUnionMetadata(
            discriminatedUnion.GetLocation(),
            discriminatedUnion.Types.Select(GetOrCreateType));

        return metadataProvider.GetOrDefine(metadata);
    }

    private FunctionTypeMetadata CreateFunctionType(FunctionType functionType)
    {
        var metadataProvider = metadataProviderMap.Get(functionType);
        var metadataFactory = new MetadataFactory(builtInTypes, metadataProvider);

        return metadataFactory.CreateFunctionType(
            functionType.GetLocation(),
            functionType.ParameterTypes.Select(GetOrCreateType),
            GetOrCreateType(functionType.ReturnType));
    }

    private ITypeMetadata CreateGenericApplication(GenericApplication genericApplication)
    {
        var metadataProvider = metadataProviderMap.Get(genericApplication);

        foreach (var argumentNode in genericApplication.TypeArguments)
            GetOrCreateType(argumentNode);

        var openGenericName = genericApplication.GetOpenGenericName();
        var openGeneric = default(IGenericMetadata);

        var types = metadataProvider.FindTypes(openGenericName);
        if (types is [IGenericMetadata genericMetadata])
        {
            openGeneric = genericMetadata;
        }
        else if (types is [_] or [])
        {
            diagnostics.UnknownType(genericApplication);
            openGeneric = TypeMetadata.Invalid(openGenericName);
        }
        else
        {
            diagnostics.MultipleMembersFound(genericApplication, types);
            openGeneric = TypeMetadata.Invalid(openGenericName);
        }

        var metadata = new GenericApplicationMetadata(
            genericApplication.GetLocation(),
            openGeneric,
            genericApplication.TypeArguments.Select(x => x.Metadata!));

        metadata = metadataProvider.GetOrDefine(metadata);

        genericToProcess.Add(metadata);

        return metadata;
    }

    private InterfaceMetadata CreateInterface(Interface @interface)
    {
        var metadataProvider = metadataProviderMap.Get(@interface);
        var metadataFactory = new MetadataFactory(builtInTypes, metadataProvider);
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

            if (!metadata.GetProperties(property.Name).IsEmpty)
            {
                propertyMetadata.MarkAsInvalid();
                diagnostics.InterfacePropertyAlreadyDefined(property);
            }

            metadata.AddProperty(propertyMetadata);
            property.Metadata = propertyMetadata;
        }

        foreach (var methods in @interface.Methods.GroupBy(x => x.Name))
        {
            foreach (var method in methods)
            {
                var parameters = new ITypeMetadata[method.ParameterTypes.Count];
                for (var i = 0; i < method.ParameterTypes.Count; i++)
                    parameters[i] = GetOrCreateType(method.ParameterTypes[i]);

                var functionType = metadataFactory.CreateFunctionType(
                    null,
                    parameters,
                    GetOrCreateType(method.ReturnType));

                // TODO: generic?
                var methodMetadata = new InterfaceMethodMetadata(
                    metadata.Definition! with { Span = method.SourceSpan.GetValueOrDefault() },
                    metadata,
                    method.Name,
                    functionType);

                if (metadata.GetMethods(method.Name).MatchFunction(parameters).Any())
                {
                    methodMetadata.MarkAsInvalid();
                    diagnostics.InterfaceMethodAlreadyDefined(method);
                }

                metadata.AddMethod(methodMetadata);
                method.Metadata = methodMetadata;
            }
        }

        return metadataProvider.GetOrDefine(metadata);
    }

    private TupleMetadata CreateTuple(TupleType tupleType)
    {
        var metadataProvider = metadataProviderMap.Get(tupleType);
        var metadataFactory = new MetadataFactory(builtInTypes, metadataProvider);
        var metadata = metadataFactory.CreateTupleMetadata(
            tupleType.GetLocation(),
            tupleType.Types.Select(GetOrCreateType));

        return metadata;
    }

    public override void VisitArrayType(ArrayType node)
    {
        if (node.Metadata is not null)
            return;

        GetOrCreateType(node);

        base.VisitArrayType(node);
    }

    public override void VisitDiscriminatedUnion(DiscriminatedUnion node)
    {
        if (node.Metadata is not null)
            return;

        GetOrCreateType(node);

        base.VisitDiscriminatedUnion(node);
    }

    public override void VisitFunctionType(FunctionType node)
    {
        if (node.Metadata is not null)
            return;

        GetOrCreateType(node);

        base.VisitFunctionType(node);
    }

    public override void VisitGenericType(GenericApplication node)
    {
        if (node.Metadata is not null)
            return;

        GetOrCreateType(node);

        base.VisitGenericType(node);
    }

    public override void VisitInterface(Interface node)
    {
        if (node.Metadata is not null)
            return;

        GetOrCreateType(node);

        base.VisitInterface(node);
    }

    public override void VisitTupleType(TupleType node)
    {
        if (node.Metadata is not null)
            return;

        GetOrCreateType(node);

        base.VisitTupleType(node);
    }

    public override void VisitTypeRef(TypeRef node)
    {
        if (node.Metadata is not null)
            return;

        GetOrCreateType(node);

        base.VisitTypeRef(node);
    }

    public override void VisitVariable(VariableDeclaration node)
    {
        base.VisitVariable(node);

        Debug.Assert(node.Type.Metadata is not null);

        var symbolTable = symbolTableMap.Get(node);
        var metadata = new VariableMetadata(node.GetLocation(), node.Name, node.Type.Metadata);

        // TODO: optimize? it will query the same symbols in the same scope multiple times
        var symbols = symbolTable.GetId(node.Name);

        // we don't need to check all symbols, just the first one
        // all other will be checked in other passes of the tree
        if (symbols[0].Node != node)
        {
            metadata.MarkAsInvalid();
            diagnostics.VariableAlreadyDefined(node);
        }

        node.Metadata = metadata;
    }

    public string Name
        => nameof(MetadataGenerator);

    public IEnumerable<string> DependsOn
        => [nameof(SymbolFinder)];

    private sealed class NamespaceMetadataGenerator : Visitor
    {
        private readonly FileMetadataProvider rootProvider;
        private readonly MetadataProviderMap map;

        public NamespaceMetadataGenerator(
            ISet<string> directives,
            FileMetadataProvider rootProvider,
            MetadataProviderMap map) : base(directives)
        {
            this.rootProvider = rootProvider;
            this.map = map;
        }

        public override void VisitNamespace(Namespace node)
        {
            var @namespace = rootProvider.Namespace.CreateChild(node.Parts);

            map.Add(node.Parent!, new FileMetadataProvider(@namespace));

            base.VisitNamespace(node);
        }

        public override void VisitTree(SemanticTree node)
        {
            // at this point, we add this file is in a root namespace
            map.Add(node, new FileMetadataProvider(rootProvider.Namespace));

            base.VisitTree(node);
        }
    }

    private sealed class AddNamespaceUses : Visitor
    {
        private readonly SemanticDiagnosticReporter diagnostics;
        private readonly FileMetadataProvider rootProvider;
        private readonly MetadataProviderMap map;

        public AddNamespaceUses(
            ISet<string> directives,
            SemanticDiagnosticReporter diagnostics,
            FileMetadataProvider rootProvider,
            MetadataProviderMap map) : base(directives)
        {
            this.diagnostics = diagnostics;
            this.rootProvider = rootProvider;
            this.map = map;
        }

        public override void VisitUse(Use node)
        {
            var @namespace = rootProvider.Namespace.FindNamespace(node.Parts);
            if (@namespace is null)
            {
                diagnostics.UnknownNamespace(node);
                return;
            }

            // we know it always be FileMetadataProvider
            var provider = (FileMetadataProvider)map.Get(node);
            provider.AddUse(@namespace);

            base.VisitUse(node);
        }
    }

    private sealed class CollectTypesVisitor : Visitor
    {
        private readonly List<TypeDescriptor> types;
        private readonly List<FunctionDeclaration> functions;

        public CollectTypesVisitor(ISet<string> directives) : base(directives)
        {
            types = [];
            functions = [];
        }

        public override void VisitAlias(AliasDeclaration node)
        {
            types.Add(TypeDescriptor.Alias(node));

            base.VisitAlias(node);
        }

        public override void VisitGenericType(GenericApplication node)
        {
            types.Add(TypeDescriptor.GenericApplication(node));

            base.VisitGenericType(node);
        }

        public override void VisitFunction(FunctionDeclaration node)
        {
            functions.Add(node);

            base.VisitFunction(node);
        }

        public override void VisitType(TypeDeclaration node)
        {
            types.Add(
                node.GenericArguments.Count > 0
                    ? TypeDescriptor.GenericDeclaration(node)
                    : TypeDescriptor.TypeDeclaration(node));

            base.VisitType(node);
        }

        public IReadOnlyList<TypeDescriptor> Types
            => types;

        public IReadOnlyList<FunctionDeclaration> Functions
            => functions;
    }

    private enum TypeKind
    {
        TypeDeclaration,
        Alias,
        GenericDeclaration,
        GenericApplication,
    }

    private record TypeDescriptor(TypeKind Kind, string Name, ISemanticNode Node)
    {
        public static TypeDescriptor TypeDeclaration(TypeDeclaration node)
            => new TypeDescriptor(TypeKind.TypeDeclaration, node.Name, node);

        public static TypeDescriptor Alias(AliasDeclaration node)
            => new TypeDescriptor(TypeKind.Alias, node.FullName, node);

        public static TypeDescriptor GenericDeclaration(TypeDeclaration node)
            => new TypeDescriptor(TypeKind.GenericDeclaration, node.FullName, node);

        public static TypeDescriptor GenericApplication(GenericApplication node)
            => new TypeDescriptor(TypeKind.GenericApplication, node.Name, node);

        public bool IsTypeDeclaration
            => Kind == TypeKind.TypeDeclaration;

        public bool IsAlias
            => Kind == TypeKind.Alias;

        public bool IsGenericDeclaration
            => Kind == TypeKind.GenericDeclaration;

        public bool IsGenericApplication
            => Kind == TypeKind.GenericApplication;
    }
}
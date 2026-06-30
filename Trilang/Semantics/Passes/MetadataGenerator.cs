using System.Diagnostics;
using Trilang.Compilation;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Semantics.Providers;

namespace Trilang.Semantics.Passes;

internal class MetadataGenerator : ISemanticPass
{
    private readonly ISet<string> directives;
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly CompilationContext compilationContext;

    private readonly HashSet<TypeDeclaration> typesToProcess;
    private readonly HashSet<AliasDeclaration> aliasToProcess;
    private readonly HashSet<FunctionDeclaration> functionsToProcess;
    private readonly HashSet<GenericApplicationMetadata> genericToProcess;

    public MetadataGenerator(
        ISet<string> directives,
        DiagnosticCollection diagnostics,
        CompilationContext compilationContext)
    {
        this.directives = directives;
        this.diagnostics = diagnostics.ForSemantic();
        this.compilationContext = compilationContext;

        typesToProcess = [];
        aliasToProcess = [];
        functionsToProcess = [];
        genericToProcess = [];
    }

    public void Analyze(Project project)
    {
        var semanticTrees = project.SourceFiles.Select(x => x.SemanticTree!).ToArray();

        var packageNamespace = NamespaceMetadata.CreateForPackage();
        var currentPackage = new PackageMetadata(project.Name, packageNamespace);
        compilationContext.AddPackage(currentPackage);

        foreach (var dependency in project.Dependencies)
        {
            var dependencyPackage = compilationContext.GetPackage(dependency.Name);
            Debug.Assert(dependencyPackage is not null, $"Dependency package '{dependency.Name}' not found");

            currentPackage.AddDependency(dependencyPackage);
        }

        compilationContext.CurrentPackage = currentPackage;

        CreateNamespaces(semanticTrees);
        AddUses(semanticTrees);

        var (types, functions) = CollectTypesAndFunctions(semanticTrees);

        CreateAliases(types);
        CreateTypes(types);
        CreateGeneric(types);
        CreateFunctions(functions);

        PopulateAliases();
        PopulateTypes();
        PopulateFunctions();

        var visitor = new MetadataGeneratorVisitor(directives, this);
        foreach (var semanticTree in semanticTrees)
            semanticTree.Accept(visitor);

        CreateClosedGenericTypes();

        Debug.Assert(packageNamespace.Types.Count == 0, "Package namespace should be empty");
    }

    private void CreateNamespaces(SemanticTree[] treesToAnalyze)
    {
        foreach (var tree in treesToAnalyze)
        {
            var @namespace = compilationContext.CurrentPackage!.Namespace.CreateChild(tree.Namespace.Parts);
            var metadataProvider = new MetadataProvider(compilationContext, @namespace);
            var visitor = new SetMetadataProvider(directives, metadataProvider);

            tree.Accept(visitor);
        }
    }

    private void AddUses(SemanticTree[] treesToAnalyze)
    {
        var addNamespaceUses = new AddNamespaceUses(directives, diagnostics, compilationContext);
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
            var metadataProvider = node.MetadataProvider!;

            // create a new provider for generic
            if (node.IsGeneric)
            {
                metadataProvider = new GenericMetadataProvider(metadataProvider, metadata);

                var visitor = new SetMetadataProvider(directives, metadataProvider);
                node.Accept(visitor);
            }

            foreach (var genericArgument in node.GenericArguments)
                metadata.AddGenericArgument(CreateTypeArgumentMetadata(metadata, genericArgument));

            var query = Query.From(node);
            var result = metadataProvider.QueryTypes(query);
            if (result.IsSuccess || result.IsMultipleTypesFound)
            {
                diagnostics.TypeAlreadyDefined(node);
                metadata.MarkAsInvalid();
            }
            else
            {
                metadataProvider.DefineType(metadata);
            }

            node.Metadata = metadata;

            typesToProcess.Add(node);
        }
    }

    private void PopulateTypes()
    {
        var builtInTypes = compilationContext.BuiltInTypes;

        foreach (var node in typesToProcess)
        {
            var type = node.Metadata!;
            var root = node.GetRoot();
            var metadataProvider = node.MetadataProvider!;
            var metadataFactory = new MetadataFactory(builtInTypes, diagnostics, metadataProvider);

            foreach (var @interface in node.Interfaces)
            {
                // TODO: support generic interfaces
                var interfaceMetadata = default(InterfaceMetadata);
                var result = metadataProvider.QueryTypes(Query.From(@interface));
                if (result is { IsSuccess: true, Types: [AliasMetadata aliasMetadata] })
                {
                    interfaceMetadata = aliasMetadata.UnpackAlias() as InterfaceMetadata;

                    if (interfaceMetadata is null)
                    {
                        interfaceMetadata = InterfaceMetadata.Invalid();
                        diagnostics.UnknownType(@interface);
                    }
                }
                else
                {
                    if (result.IsMultipleTypesFound)
                        diagnostics.MultipleMembersFound(@interface, result.Types);
                    else
                        diagnostics.UnknownType(@interface);

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
                property.Symbol!.Metadata = propertyMetadata;

                if (propertyMetadata.Getter is not null)
                {
                    type.AddMethod(propertyMetadata.Getter);
                    property.Getter?.Metadata = propertyMetadata.Getter;
                    property.Setter?.FieldSymbol!.Metadata = propertyMetadata.Type;
                }

                if (propertyMetadata.Setter is not null)
                {
                    type.AddMethod(propertyMetadata.Setter);
                    property.Setter?.Metadata = propertyMetadata.Setter;
                    property.Setter?.FieldSymbol!.Metadata = propertyMetadata.Type;
                    // the `value` parameter
                    property.Setter?.ValueSymbol!.Metadata = propertyMetadata.Setter.Parameters[0];
                }
            }

            var returnType = default(ITypeMetadata);
            if (node.IsGeneric)
            {
                returnType = metadataFactory.CreateGenericApplication(
                    null,
                    type,
                    node.GenericArguments.Select(x => x.Metadata!).ToArray());
            }
            else
            {
                returnType = type;
            }

            if (node.Constructors.Count > 0)
            {
                foreach (var constructor in node.Constructors)
                {
                    var parameters = GetParameters(root, constructor.Parameters);
                    var functionType = metadataFactory.CreateFunctionType(
                        null,
                        parameters.Select(x => x.Type).ToArray(),
                        returnType);

                    var constructorMetadata = new ConstructorMetadata(
                        new SourceLocation(root.SourceFile, constructor.SourceSpan.GetValueOrDefault()),
                        type,
                        constructor.AccessModifier.ToMetadata(),
                        parameters,
                        functionType);

                    type.AddConstructor(constructorMetadata);
                    constructor.Metadata = constructorMetadata;
                    constructor.ThisSymbol!.Metadata = new ParameterMetadata(
                        null,
                        MemberAccessExpression.This,
                        metadataFactory.CreatePointer(null, type));
                }
            }
            else
            {
                var functionType = metadataFactory.CreateFunctionType(null, [], returnType);
                type.AddConstructor(new ConstructorMetadata(
                    null,
                    type,
                    AccessModifierMetadata.Public,
                    [],
                    functionType));
            }

            foreach (var method in node.Methods)
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
                method.Symbol!.Metadata = methodMetadata;
                method.ThisSymbol!.Metadata = new ParameterMetadata(
                    null,
                    MemberAccessExpression.This,
                    metadataFactory.CreatePointer(null, type));
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
            parameter.Symbol!.Metadata = parameterMetadata;
        }

        return result!;
    }

    private void CreateGeneric(IReadOnlyList<TypeDescriptor> types)
    {
        var builtInTypes = compilationContext.BuiltInTypes;

        foreach (var symbol in types)
        {
            if (!symbol.IsGenericApplication)
                continue;

            var genericApplication = (GenericApplication)symbol.Node;
            ResolveInlineType(genericApplication);

            var metadataProvider = genericApplication.MetadataProvider!;
            var factory = new MetadataFactory(builtInTypes, diagnostics, metadataProvider);
            var metadata = (GenericApplicationMetadata)factory.Create(genericApplication);

            genericApplication.Metadata = metadata;
            genericToProcess.Add(metadata);
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
            var metadataProvider = node.MetadataProvider!;

            // create a new provider for generic
            if (node.IsGeneric)
            {
                metadataProvider = new GenericMetadataProvider(metadataProvider, metadata);

                var visitor = new SetMetadataProvider(directives, metadataProvider);
                node.Accept(visitor);
            }

            foreach (var genericArgument in node.GenericArguments)
                metadata.AddGenericArgument(CreateTypeArgumentMetadata(metadata, genericArgument));

            var query = Query.From(node);
            var result = metadataProvider.QueryTypes(query);
            if (result.IsSuccess || result.IsMultipleTypesFound)
            {
                diagnostics.TypeAlreadyDefined(node);
                metadata.MarkAsInvalid();
            }
            else
            {
                metadataProvider.DefineType(metadata);
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
            var metadataProvider = function.MetadataProvider!;
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
        var builtInTypes = compilationContext.BuiltInTypes;

        foreach (var function in functionsToProcess)
        {
            var root = function.GetRoot();
            var metadata = function.Metadata!;
            var metadataProvider = function.MetadataProvider!;
            var metadataFactory = new MetadataFactory(builtInTypes, diagnostics, metadataProvider);

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
                functionParameter.Symbol!.Metadata = parameter;
                metadata.AddParameter(parameter);
            }

            var functionTypeMetadata = metadataFactory.CreateFunctionType(
                null,
                metadata.Parameters.Select(x => x.Type).ToArray(),
                GetOrCreateType(function.ReturnType));

            metadata.Type = functionTypeMetadata;
        }

        foreach (var function in functionsToProcess)
        {
            var metadata = function.Metadata!;
            var metadataProvider = function.MetadataProvider!;
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

            var map = new TypeArgumentMap(diagnostics, compilationContext, generic);
            map.Map();
        }
    }

    private ITypeMetadata GetOrCreateType(IInlineType inlineType)
    {
        ResolveInlineType(inlineType);

        var builtInTypes = compilationContext.BuiltInTypes;
        var metadataProvider = inlineType.MetadataProvider!;
        var result = metadataProvider.QueryTypes(Query.From(inlineType));
        var metadata = default(ITypeMetadata);

        if (result.IsSuccess)
        {
            metadata = result.Types[0];
        }
        else if (result.IsTypeNotFound)
        {
            var factory = new MetadataFactory(builtInTypes, diagnostics, metadataProvider);
            metadata = factory.Create(inlineType);

            if (metadata is GenericApplicationMetadata genericApplication)
                genericToProcess.Add(genericApplication);
        }
        else
        {
            if (result.IsMultipleTypesFound)
                diagnostics.MultipleMembersFound(inlineType, result.Types);
            else if (result.IsNamespaceNotFound)
                diagnostics.UnknownNamespace(inlineType);

            metadata = TypeMetadata.Invalid(inlineType.Name);
        }

        inlineType.Metadata = metadata;

        return metadata;
    }

    private void ResolveInlineType(IInlineType inlineType)
    {
        if (inlineType is ArrayType arrayType)
        {
            if (arrayType.ElementType.Metadata is null)
                GetOrCreateType(arrayType.ElementType);
        }
        else if (inlineType is DiscriminatedUnion discriminatedUnion)
        {
            foreach (var type in discriminatedUnion.Types)
                if (type.Metadata is null)
                    GetOrCreateType(type);
        }
        else if (inlineType is FunctionType functionType)
        {
            foreach (var parameter in functionType.ParameterTypes)
                if (parameter.Metadata is null)
                    GetOrCreateType(parameter);

            if (functionType.ReturnType.Metadata is null)
                GetOrCreateType(functionType.ReturnType);
        }
        else if (inlineType is GenericApplication genericApplication)
        {
            foreach (var argument in genericApplication.TypeArguments)
                if (argument.Metadata is null)
                    GetOrCreateType(argument);

            var metadataProvider = genericApplication.MetadataProvider!;
            var getOpenGeneric = new GetOpenGeneric(
                genericApplication.Type.Name,
                genericApplication.TypeArguments.Count);
            var result = metadataProvider.QueryTypes(getOpenGeneric);
            var openGeneric = default(IGenericMetadata);
            if (result is { IsSuccess: true, Types: [IGenericMetadata genericMetadata] })
            {
                openGeneric = genericMetadata;
            }
            else
            {
                if (result.IsMultipleTypesFound)
                    diagnostics.MultipleMembersFound(genericApplication, result.Types);
                else if (result.IsNamespaceNotFound)
                    diagnostics.UnknownNamespace(genericApplication);
                else
                    diagnostics.UnknownType(genericApplication);

                openGeneric = TypeMetadata.Invalid(genericApplication.Type.Name);
            }

            genericApplication.Type.Metadata = openGeneric;
        }
        else if (inlineType is Interface @interface)
        {
            foreach (var property in @interface.Properties)
                if (property.Metadata is null)
                    GetOrCreateType(property.Type);

            foreach (var method in @interface.Methods)
            {
                foreach (var parameter in method.ParameterTypes)
                    if (parameter.Metadata is null)
                        GetOrCreateType(parameter);

                if (method.ReturnType.Metadata is null)
                    GetOrCreateType(method.ReturnType);
            }
        }
        else if (inlineType is PointerType pointerType)
        {
            var type = pointerType.Type;
            if (type.Metadata is null)
                GetOrCreateType(type);
        }
        else if (inlineType is TupleType tupleType)
        {
            foreach (var type in tupleType.Types)
                if (type.Metadata is null)
                    GetOrCreateType(type);
        }
    }

    public string Name
        => nameof(MetadataGenerator);

    public IEnumerable<string> DependsOn
        => [nameof(SymbolFinder)];

    private sealed class MetadataGeneratorVisitor : Visitor
    {
        private readonly MetadataGenerator generator;

        public MetadataGeneratorVisitor(ISet<string> directives, MetadataGenerator generator)
            : base(directives)
        {
            this.generator = generator;
        }

        public override void VisitArrayType(ArrayType node)
        {
            if (node.Metadata is not null)
                return;

            generator.GetOrCreateType(node);

            base.VisitArrayType(node);
        }

        public override void VisitDiscriminatedUnion(DiscriminatedUnion node)
        {
            if (node.Metadata is not null)
                return;

            generator.GetOrCreateType(node);

            base.VisitDiscriminatedUnion(node);
        }

        public override void VisitFunctionType(FunctionType node)
        {
            if (node.Metadata is not null)
                return;

            generator.GetOrCreateType(node);

            base.VisitFunctionType(node);
        }

        public override void VisitGenericType(GenericApplication node)
        {
            if (node.Metadata is not null)
                return;

            generator.GetOrCreateType(node);

            base.VisitGenericType(node);
        }

        public override void VisitInterface(Interface node)
        {
            if (node.Metadata is not null)
                return;

            generator.GetOrCreateType(node);

            base.VisitInterface(node);
        }

        public override void VisitPointer(PointerType node)
        {
            if (node.Metadata is not null)
                return;

            generator.GetOrCreateType(node);

            base.VisitPointer(node);
        }

        public override void VisitTupleType(TupleType node)
        {
            if (node.Metadata is not null)
                return;

            generator.GetOrCreateType(node);

            base.VisitTupleType(node);
        }

        public override void VisitTypeRef(TypeRef node)
        {
            if (node.Metadata is not null)
                return;

            generator.GetOrCreateType(node);

            base.VisitTypeRef(node);
        }

        public override void VisitVariable(VariableDeclaration node)
        {
            base.VisitVariable(node);

            Debug.Assert(node.Type.Metadata is not null);

            var metadata = new VariableMetadata(node.GetLocation(), node.Name, node.Type.Metadata);

            node.Metadata = metadata;
            node.Symbol!.Metadata = metadata;
        }
    }

    private sealed class SetMetadataProvider : Visitor
    {
        private readonly IMetadataProvider metadataProvider;

        public SetMetadataProvider(ISet<string> directives, IMetadataProvider metadataProvider)
            : base(directives)
        {
            this.metadataProvider = metadataProvider;
        }

        public override void VisitAlias(AliasDeclaration node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitAlias(node);
        }

        public override void VisitArrayAccess(ArrayAccessExpression node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitArrayAccess(node);
        }

        public override void VisitArrayType(ArrayType node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitArrayType(node);
        }

        public override void VisitBinaryExpression(BinaryExpression node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitBinaryExpression(node);
        }

        public override void VisitBlock(BlockStatement node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitBlock(node);
        }

        public override void VisitBreak(Break node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitBreak(node);
        }

        public override void VisitCall(CallExpression node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitCall(node);
        }

        public override void VisitCast(CastExpression node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitCast(node);
        }

        public override void VisitConstructor(ConstructorDeclaration node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitConstructor(node);
        }

        public override void VisitContinue(Continue node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitContinue(node);
        }

        public override void VisitDiscriminatedUnion(DiscriminatedUnion node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitDiscriminatedUnion(node);
        }

        public override void VisitExpressionBlock(ExpressionBlock node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitExpressionBlock(node);
        }

        public override void VisitExpressionStatement(ExpressionStatement node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitExpressionStatement(node);
        }

        public override void VisitFakeDeclaration(FakeDeclaration node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitFakeDeclaration(node);
        }

        public override void VisitFakeExpression(FakeExpression node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitFakeExpression(node);
        }

        public override void VisitFakeStatement(FakeStatement node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitFakeStatement(node);
        }

        public override void VisitFakeType(FakeType node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitFakeType(node);
        }

        public override void VisitFunction(FunctionDeclaration node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitFunction(node);
        }

        public override void VisitFunctionType(FunctionType node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitFunctionType(node);
        }

        public override void VisitGenericType(GenericApplication node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitGenericType(node);
        }

        public override void VisitGenericExpression(GenericExpression node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitGenericExpression(node);
        }

        public override void VisitGoTo(GoTo node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitGoTo(node);
        }

        public override void VisitIfDirective(IfDirective node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitIfDirective(node);
        }

        public override void VisitIf(IfStatement node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitIf(node);
        }

        public override void VisitInterface(Interface node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitInterface(node);
        }

        public override void VisitInterfaceProperty(InterfaceProperty node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitInterfaceProperty(node);
        }

        public override void VisitInterfaceMethod(InterfaceMethod node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitInterfaceMethod(node);
        }

        public override void VisitIsExpression(IsExpression node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitIsExpression(node);
        }

        public override void VisitLabel(Label node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitLabel(node);
        }

        public override void VisitLiteral(LiteralExpression node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitLiteral(node);
        }

        public override void VisitMemberAccess(MemberAccessExpression node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitMemberAccess(node);
        }

        public override void VisitMethod(MethodDeclaration node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitMethod(node);
        }

        public override void VisitNamespace(Namespace node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitNamespace(node);
        }

        public override void VisitNewObject(NewObjectExpression node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitNewObject(node);
        }

        public override void VisitNull(NullExpression node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitNull(node);
        }

        public override void VisitParameter(Parameter node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitParameter(node);
        }

        public override void VisitPointer(PointerType node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitPointer(node);
        }

        public override void VisitProperty(PropertyDeclaration node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitProperty(node);
        }

        public override void VisitGetter(PropertyGetter node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitGetter(node);
        }

        public override void VisitSetter(PropertySetter node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitSetter(node);
        }

        public override void VisitReturn(ReturnStatement node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitReturn(node);
        }

        public override void VisitTree(SemanticTree node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitTree(node);
        }

        public override void VisitTuple(TupleExpression node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitTuple(node);
        }

        public override void VisitTupleType(TupleType node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitTupleType(node);
        }

        public override void VisitType(TypeDeclaration node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitType(node);
        }

        public override void VisitTypeRef(TypeRef node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitTypeRef(node);
        }

        public override void VisitUnaryExpression(UnaryExpression node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitUnaryExpression(node);
        }

        public override void VisitUse(Use node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitUse(node);
        }

        public override void VisitVariable(VariableDeclaration node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitVariable(node);
        }

        public override void VisitWhile(While node)
        {
            node.MetadataProvider = metadataProvider;

            base.VisitWhile(node);
        }
    }

    private sealed class AddNamespaceUses : Visitor
    {
        private readonly SemanticDiagnosticReporter diagnostics;
        private readonly CompilationContext compilationContext;

        public AddNamespaceUses(
            ISet<string> directives,
            SemanticDiagnosticReporter diagnostics,
            CompilationContext compilationContext)
            : base(directives)
        {
            this.diagnostics = diagnostics;
            this.compilationContext = compilationContext;
        }

        public override void VisitUse(Use node)
        {
            var namespaceResult = compilationContext.FindNamespace(node.Package, node.Parts);
            if (!namespaceResult.IsSuccess)
            {
                if (namespaceResult.IsPackageNotFound)
                    diagnostics.UnknownPackage(node);
                else
                    diagnostics.UnknownNamespace(node);

                return;
            }

            // we know it always be MetadataProvider
            var provider = (MetadataProvider)node.MetadataProvider!;
            provider.AddUse(namespaceResult.Namespace);

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
                node.IsGeneric
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

    private record TypeDescriptor(TypeKind Kind, ISemanticNode Node)
    {
        public static TypeDescriptor TypeDeclaration(TypeDeclaration node)
            => new TypeDescriptor(TypeKind.TypeDeclaration, node);

        public static TypeDescriptor Alias(AliasDeclaration node)
            => new TypeDescriptor(TypeKind.Alias, node);

        public static TypeDescriptor GenericDeclaration(TypeDeclaration node)
            => new TypeDescriptor(TypeKind.GenericDeclaration, node);

        public static TypeDescriptor GenericApplication(GenericApplication node)
            => new TypeDescriptor(TypeKind.GenericApplication, node);

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
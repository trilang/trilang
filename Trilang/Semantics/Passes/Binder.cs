using System.Diagnostics;
using Trilang.Compilation;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Semantics.Providers;
using Trilang.Semantics.TypeMatchers;
using Trilang.Semantics.TypeMatchers.AggregateResults;
using static Trilang.Semantics.Model.UnaryExpressionKind;

namespace Trilang.Semantics.Passes;

internal class Binder : ISemanticPass
{
    private readonly ISet<string> directives;
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly CompilationContext compilationContext;

    public Binder(
        ISet<string> directives,
        DiagnosticCollection diagnostics,
        CompilationContext compilationContext)
    {
        this.directives = directives;
        this.diagnostics = diagnostics.ForSemantic();
        this.compilationContext = compilationContext;
    }

    public void Analyze(Project project)
    {
        var transformer = new BinderTransformer(directives, diagnostics, compilationContext);
        foreach (var sourceFile in project.SourceFiles)
            sourceFile.SemanticTree = (SemanticTree)sourceFile.SemanticTree!.Transform(transformer);
    }

    public string Name => nameof(Binder);

    public IEnumerable<string> DependsOn =>
    [
        nameof(CompoundAssignmentTargetValidation),
        nameof(CyclicAlias),
        nameof(MetadataGenerator),
        nameof(SymbolFinder),
        nameof(VariableDuplicate),
    ];

    private sealed class BinderTransformer : Transformer
    {
        private readonly SemanticDiagnosticReporter diagnostics;
        private readonly CompilationContext compilationContext;
        private readonly TypeMatcher typeMatcher;
        private readonly UnaryTypeMatcher unaryTypeMatcher;
        private readonly BinaryTypeMatcher binaryTypeMatcher;
        private readonly AggregateResolver aggregateResolver;

        public BinderTransformer(
            ISet<string> directives,
            SemanticDiagnosticReporter diagnostics,
            CompilationContext compilationContext)
            : base(directives, compilationContext.BuiltInTypes)
        {
            this.diagnostics = diagnostics;
            this.compilationContext = compilationContext;

            typeMatcher = new TypeMatcher(builtInTypes);
            unaryTypeMatcher = new UnaryTypeMatcher(typeMatcher);
            binaryTypeMatcher = new BinaryTypeMatcher(typeMatcher);
            aggregateResolver = new AggregateResolver(typeMatcher);
        }

        private void ResolveSingle(IExpression member)
        {
            if (member is IAccessExpression { Reference: AggregateMetadata aggregate } accessExpression)
            {
                var result = aggregateResolver.ResolveSingle(aggregate);
                if (result is NoMatch<IMetadata>)
                    diagnostics.NoMemberFound(accessExpression);
                else if (result is MultipleMatches<IMetadata> multipleMembers)
                    diagnostics.MultipleMembersFound(accessExpression, multipleMembers.Candidates);

                accessExpression.Reference = result.Member;
            }
        }

        private (IAccessExpression, bool, IExpression[]) ResolveFunction(
            CallExpression node,
            IAccessExpression member,
            IExpression[] parameters)
        {
            var isChanged = false;
            var aggregate = (AggregateMetadata)member.Reference!;
            var parameterTypes = parameters.Select(x => x.ReturnTypeMetadata!).ToArray();
            var result = aggregateResolver.ResolveFunction(aggregate, parameterTypes);
            if (result is ExtraArgument<IFunctionMetadata> extraArgument)
            {
                foreach (var position in extraArgument.Positions)
                    diagnostics.ExtraArgument(parameters[position]);
            }
            else if (result is MissingArgument<IFunctionMetadata> missingArgument)
            {
                foreach (var position in missingArgument.Positions)
                    diagnostics.MissingArgument(node, missingArgument.Match.Type.ParameterTypes[position]);
            }
            else if (result is ArgumentMismatch<IFunctionMetadata> argumentMismatch)
            {
                foreach (var detail in argumentMismatch.Mismatches)
                    diagnostics.TypeMismatch(parameters[detail.Position], detail.Expected, detail.Actual);
            }
            else if (result is NotFunction notFunction)
            {
                diagnostics.ExpectedFunction(member, notFunction.Candidate);
            }
            else if (result is NoMatch<IFunctionMetadata>)
            {
                diagnostics.NoSuitableOverload(member);
            }
            else if (result is MultipleMatches<IFunctionMetadata> multipleOverloads)
            {
                diagnostics.MultipleCandidates(member, multipleOverloads.Candidates);
            }
            else if (result is ConversionMatch<IFunctionMetadata> conversionMatch)
            {
                foreach (var conversion in conversionMatch.Conversions)
                {
                    parameters[conversion.Position] = new CastExpression(
                        null,
                        GetInlineType(node, conversion.Target),
                        parameters[conversion.Position])
                    {
                        SymbolTable = node.SymbolTable,
                        MetadataProvider = node.MetadataProvider,
                    };

                    isChanged = true;
                }
            }

            member.Reference = result.Member;

            return (member, isChanged, parameters);
        }

        private void ResolveFunction(IExpression node, FunctionTypeMetadata functionType)
        {
            if (node is not IAccessExpression accessExpression)
                return;

            var parameters = functionType.ParameterTypes;
            var aggregate = (AggregateMetadata)accessExpression.Reference!;
            var result = aggregateResolver.ResolveFunction(aggregate, parameters);
            if (result is ExtraArgument<IFunctionMetadata> extraArgument)
            {
                foreach (var position in extraArgument.Positions)
                    diagnostics.ExtraArgument(accessExpression, parameters[position]);
            }
            else if (result is MissingArgument<IFunctionMetadata> missingArgument)
            {
                foreach (var position in missingArgument.Positions)
                    diagnostics.MissingArgument(node, missingArgument.Match.Type.ParameterTypes[position]);
            }
            else if (result is ArgumentMismatch<IFunctionMetadata> argumentMismatch)
            {
                foreach (var detail in argumentMismatch.Mismatches)
                    diagnostics.TypeMismatch(accessExpression, detail.Expected, detail.Actual);
            }
            else if (result is NoMatch<IFunctionMetadata> or ConversionMatch<IFunctionMetadata>)
            {
                diagnostics.NoSuitableOverload(accessExpression);
            }
            else if (result is MultipleMatches<IFunctionMetadata> multipleMatches)
            {
                diagnostics.MultipleCandidates(accessExpression, multipleMatches.Candidates);
            }

            accessExpression.Reference = result.Member;
        }

        private IGenericMetadata ResolveGeneric(GenericExpression node)
        {
            var aggregate = node.Member.Reference as AggregateMetadata;
            var result = aggregateResolver.ResolveGeneric(aggregate!, node.GenericArguments.Count);
            if (result is ExtraArgument<IGenericMetadata> extraArgument)
            {
                foreach (var position in extraArgument.Positions)
                    diagnostics.ExtraArgument(node.GenericArguments[position]);
            }
            else if (result is MissingArgument<IGenericMetadata> missingArgument)
            {
                foreach (var position in missingArgument.Positions)
                    diagnostics.MissingArgument(node, missingArgument.Match.GenericArguments[position]);
            }
            else if (result is NotGeneric)
            {
                diagnostics.NonGenericMember(node);
            }
            else if (result is NoMatch<IGenericMetadata>)
            {
                diagnostics.NoSuitableOverload(node.Member);
            }
            else if (result is MultipleMatches<IGenericMetadata> multipleMatches)
            {
                diagnostics.MultipleCandidates(node.Member, multipleMatches.Candidates);
            }

            node.Member.Reference = result.Member;

            return result.Member;
        }

        private void ResolveExpression(IExpression expression, IMetadata? returnType)
        {
            if (returnType is FunctionTypeMetadata functionType)
                ResolveFunction(expression, functionType);
            else
                ResolveSingle(expression);
        }

        private IInlineType GetInlineType(ISemanticNode node, ITypeMetadata type)
        {
            if (type is AliasMetadata aliasMetadata)
            {
                var (package, parts) = GetNamespaceParts(aliasMetadata);

                return aliasMetadata.IsGeneric
                    ? throw new InvalidOperationException("Cannot get inline type of generic alias")
                    : new TypeRef(null, package, parts)
                    {
                        SymbolTable = node.SymbolTable,
                        MetadataProvider = node.MetadataProvider,
                    };
            }

            if (type is ArrayMetadata arrayMetadata)
                return new ArrayType(null, GetInlineType(node, arrayMetadata.ItemMetadata!))
                {
                    SymbolTable = node.SymbolTable,
                    MetadataProvider = node.MetadataProvider,
                };

            if (type is DiscriminatedUnionMetadata discriminatedUnionMetadata)
                return new DiscriminatedUnion(
                    null,
                    discriminatedUnionMetadata.Types.Select(x => GetInlineType(node, x)).ToArray())
                {
                    SymbolTable = node.SymbolTable,
                    MetadataProvider = node.MetadataProvider,
                };

            if (type is FunctionTypeMetadata functionTypeMetadata)
                return new FunctionType(
                    null,
                    functionTypeMetadata.ParameterTypes.Select(x => GetInlineType(node, x)).ToArray(),
                    GetInlineType(node, functionTypeMetadata.ReturnType))
                {
                    SymbolTable = node.SymbolTable,
                    MetadataProvider = node.MetadataProvider,
                };

            if (type is GenericApplicationMetadata genericApplicationMetadata)
                return new GenericApplication(
                    null,
                    (TypeRef)GetInlineType(node, genericApplicationMetadata.OpenGeneric),
                    genericApplicationMetadata.Arguments.Select(x => GetInlineType(node, x)).ToArray())
                {
                    SymbolTable = node.SymbolTable,
                    MetadataProvider = node.MetadataProvider,
                };

            if (type is InterfaceMetadata interfaceMetadata)
                return new Interface(
                    null,
                    interfaceMetadata.Properties
                        .Select(x => new InterfaceProperty(
                            null,
                            x.Name,
                            GetInlineType(node, x.Type),
                            x.GetterModifier.ToSemanticModel(),
                            x.SetterModifier.ToSemanticModel())
                        {
                            SymbolTable = node.SymbolTable,
                            MetadataProvider = node.MetadataProvider,
                        })
                        .ToArray(),
                    interfaceMetadata.Methods
                        .Select(x => new InterfaceMethod(
                            null,
                            x.Name,
                            x.Type.ParameterTypes.Select(x => GetInlineType(node, x)).ToArray(),
                            GetInlineType(node, x.Type.ReturnType))
                        {
                            SymbolTable = node.SymbolTable,
                            MetadataProvider = node.MetadataProvider,
                        })
                        .ToArray())
                {
                    SymbolTable = node.SymbolTable,
                    MetadataProvider = node.MetadataProvider,
                };

            if (type is PointerMetadata pointerMetadata)
                return new PointerType(null, GetInlineType(node, pointerMetadata.Type))
                {
                    SymbolTable = node.SymbolTable,
                    MetadataProvider = node.MetadataProvider,
                };

            if (type is TupleMetadata tupleMetadata)
                return new TupleType(null, tupleMetadata.Types.Select(x => GetInlineType(node, x)).ToArray())
                {
                    SymbolTable = node.SymbolTable,
                    MetadataProvider = node.MetadataProvider,
                };

            if (type is TypeArgumentMetadata typeArgumentMetadata)
                return new TypeRef(null, null, [typeArgumentMetadata.Name])
                {
                    SymbolTable = node.SymbolTable,
                    MetadataProvider = node.MetadataProvider,
                };

            if (type is TypeMetadata typeMetadata)
            {
                var (package, parts) = GetNamespaceParts(typeMetadata);

                return typeMetadata.IsGeneric
                    ? throw new InvalidOperationException("Cannot get inline type of generic type")
                    : new TypeRef(null, package, parts)
                    {
                        SymbolTable = node.SymbolTable,
                        MetadataProvider = node.MetadataProvider,
                    };
            }

            throw new ArgumentOutOfRangeException(nameof(type));
        }

        private (string?, IReadOnlyList<string>) GetNamespaceParts(INamedMetadata type)
        {
            var parts = new List<string>();

            var current = type.Namespace as NamespaceMetadata;
            while (current is not null)
            {
                parts.Add(current.Name);
                current = current.Parent;
            }

            parts.Add(type.Name);

            var packageName = current?.Package.Name;

            return (packageName, parts);
        }

        public override ISemanticNode TransformArrayAccess(ArrayAccessExpression node)
        {
            node = (ArrayAccessExpression)base.TransformArrayAccess(node);

            ResolveSingle(node.Member);
            ResolveSingle(node.Index);

            if (node.Member.ReturnTypeMetadata is ArrayMetadata { IsInvalid: false } typeArray)
            {
                Debug.Assert(typeArray.ItemMetadata is not null);
                node.Reference = typeArray;
                node.ReturnTypeMetadata = typeArray.ItemMetadata;
            }
            else if (node.Member.Reference is TypeMetadata type)
            {
                var provider = node.MetadataProvider!;
                var factory = new MetadataFactory(builtInTypes, diagnostics, provider);
                var array = factory.CreateArrayMetadata(node.GetLocation(), type);
                var pointer = factory.CreatePointer(null, array);
                node.Reference = array;
                node.ReturnTypeMetadata = pointer;
            }
            else
            {
                diagnostics.ExpectedArray(node.Member);
                node.Reference = ArrayMetadata.Invalid();
                node.ReturnTypeMetadata = ArrayMetadata.Invalid();
            }

            if (!Equals(node.Index.ReturnTypeMetadata, builtInTypes.I32))
                diagnostics.TypeMismatch(node.Index, builtInTypes.I32, node.Index.ReturnTypeMetadata);

            return node;
        }

        public override ISemanticNode TransformBinaryExpression(BinaryExpression node)
        {
            var left = (IExpression)node.Left.Transform(this);
            var right = (IExpression)node.Right.Transform(this);

            ResolveSingle(left);
            ResolveSingle(right);

            var result = binaryTypeMatcher.Match(
                node.Kind,
                left.ReturnTypeMetadata,
                right.ReturnTypeMetadata);

            if (result is SuccessMatch)
            {
                if (node.Kind.IsArithmetic() ||
                    node.Kind.IsBitwise() ||
                    node.Kind.IsAssignment() ||
                    node.Kind.IsArithmeticCompoundAssignment() ||
                    node.Kind.IsBitwiseCompoundAssignment())
                {
                    node.ReturnTypeMetadata = left.ReturnTypeMetadata;
                }
                else if (node.Kind.IsConditional() || node.Kind.IsEquality() || node.Kind.IsRelational())
                {
                    node.ReturnTypeMetadata = builtInTypes.Bool;
                }
            }
            else if (result is InvalidMatch)
            {
                node.ReturnTypeMetadata = TypeMetadata.InvalidType;
            }
            else if (result is FailedMatch)
            {
                node.ReturnTypeMetadata = TypeMetadata.InvalidType;
                diagnostics.IncompatibleBinaryOperand(node);
            }
            else if (result is BinaryImplicitConversion binaryImplicitConversion)
            {
                if (binaryImplicitConversion.Left is not null)
                {
                    left = new CastExpression(null, GetInlineType(node, binaryImplicitConversion.Left), left)
                    {
                        SymbolTable = node.SymbolTable,
                        MetadataProvider = node.MetadataProvider,
                    };
                    node.ReturnTypeMetadata = binaryImplicitConversion.Left;
                }
                else if (binaryImplicitConversion.Right is not null)
                {
                    right = new CastExpression(null, GetInlineType(node, binaryImplicitConversion.Right), right)
                    {
                        SymbolTable = node.SymbolTable,
                        MetadataProvider = node.MetadataProvider,
                    };
                    node.ReturnTypeMetadata = binaryImplicitConversion.Right;
                }
            }

            if (left == node.Left && right == node.Right)
                return node;

            return new BinaryExpression(node.SourceSpan, node.Kind, left, right)
            {
                ReturnTypeMetadata = node.ReturnTypeMetadata,
                SymbolTable = node.SymbolTable,
                MetadataProvider = node.MetadataProvider,
            };
        }

        public override ISemanticNode TransformCall(CallExpression node)
        {
            var member = (IAccessExpression)node.Member.Transform(this);
            var (isChanged, parameters) = TransformNodes(node.Parameters);

            if (member.Reference!.IsInvalid)
            {
                member.Reference = InvalidMemberMetadata.Instance;
            }
            else
            {
                // resolve call parameters
                // it tries to "pre-resolve" the function of the call
                // to help resolve the correct overload in parameters.
                // for example:
                //
                // public type Point {
                //     public constructor(callback: (i32) => void) { }
                // }
                //
                // public test(p: i32): void { }
                // public test(p: bool): void { }
                //
                // public main(): void {
                //     var a: Point = Point(test);
                // }
                var aggregate = (AggregateMetadata)member.Reference;
                var function = TryResolveSingleFunction(aggregate);
                for (var i = 0; i < parameters.Length; i++)
                {
                    var parameter = parameters[i];
                    var returnType = function is not null && i < function.Type.ParameterTypes.Count
                        ? function.Type.ParameterTypes[i]
                        : null;

                    ResolveExpression(parameter, returnType);
                }

                (member, isChanged, parameters) = ResolveFunction(node, member, parameters);
            }

            if (member == node.Member && !isChanged)
                return node;

            return new CallExpression(node.SourceSpan, member, parameters)
            {
                SymbolTable = node.SymbolTable,
                MetadataProvider = node.MetadataProvider,
            };
        }

        private static IFunctionMetadata? TryResolveSingleFunction(AggregateMetadata aggregate)
        {
            if (aggregate.Members.Count != 1)
                return null;

            var member = aggregate.Members[0];
            if (member is INamedMetadata named)
            {
                var ctor = named.GetMembers(ConstructorMetadata.Name);
                if (ctor.Members.Count != 1)
                    return null;

                member = ctor.Members[0];
            }

            if (member is IFunctionMetadata function)
                return function;

            return null;
        }

        public override ISemanticNode TransformCast(CastExpression node)
        {
            node = (CastExpression)base.TransformCast(node);

            ResolveExpression(node.Expression, node.ReturnTypeMetadata);

            return node;
        }

        public override ISemanticNode TransformExpressionBlock(ExpressionBlock node)
        {
            Debug.Fail("Expression blocks are not supported");
            return node;
        }

        public override ISemanticNode TransformExpressionStatement(ExpressionStatement node)
        {
            node = (ExpressionStatement)base.TransformExpressionStatement(node);

            ResolveSingle(node.Expression);

            return node;
        }

        public override ISemanticNode TransformGenericExpression(GenericExpression node)
        {
            node = (GenericExpression)base.TransformGenericExpression(node);

            var result = ResolveGeneric(node);
            if (result.IsInvalid)
                return node;

            var provider = node.MetadataProvider!;
            var factory = new MetadataFactory(builtInTypes, diagnostics, provider);
            var genericApplication = factory.CreateGenericApplication(
                node.GetLocation(),
                result,
                node.GenericArguments.Select(x => x.Metadata!).ToArray());

            if (genericApplication.ClosedGeneric is null)
            {
                var map = new TypeArgumentMap(diagnostics, compilationContext, genericApplication);
                map.Map();
            }

            node.Member.Reference = new AggregateMetadata([
                genericApplication.ClosedGeneric!
            ]);

            return node;
        }

        public override ISemanticNode TransformIf(IfStatement node)
        {
            node = (IfStatement)base.TransformIf(node);

            ResolveSingle(node.Condition);

            if (!Equals(node.Condition.ReturnTypeMetadata, builtInTypes.Bool))
            {
                diagnostics.TypeMismatch(
                    node.Condition,
                    builtInTypes.Bool,
                    node.Condition.ReturnTypeMetadata);
            }

            return node;
        }

        public override ISemanticNode TransformIsExpression(IsExpression node)
        {
            node = (IsExpression)base.TransformIsExpression(node);

            ResolveSingle(node.Expression);

            return node;
        }

        public override ISemanticNode TransformLiteral(LiteralExpression node)
        {
            node = (LiteralExpression)base.TransformLiteral(node);

            node.ReturnTypeMetadata = node.Kind switch
            {
                LiteralExpressionKind.I8 => builtInTypes.I8,
                LiteralExpressionKind.I16 => builtInTypes.I16,
                LiteralExpressionKind.I32 => builtInTypes.I32,
                LiteralExpressionKind.I64 => builtInTypes.I64,
                LiteralExpressionKind.U8 => builtInTypes.U8,
                LiteralExpressionKind.U16 => builtInTypes.U16,
                LiteralExpressionKind.U32 => builtInTypes.U32,
                LiteralExpressionKind.U64 => builtInTypes.U64,
                LiteralExpressionKind.F32 => builtInTypes.F32,
                LiteralExpressionKind.F64 => builtInTypes.F64,
                LiteralExpressionKind.Boolean => builtInTypes.Bool,
                LiteralExpressionKind.String => builtInTypes.String,
                LiteralExpressionKind.Char => builtInTypes.Char,

                _ => throw new ArgumentOutOfRangeException(nameof(node.Kind), $"Unsupported literal expression kind: {node.Kind}"),
            };

            return node;
        }

        public override ISemanticNode TransformMemberAccess(MemberAccessExpression node)
        {
            if (node.IsFirstMember)
            {
                VisitFirstMemberAccess(node);
                return node;
            }

            node = VisitNestedMemberAccess(node);
            Debug.Assert(node.Reference is not null);

            return node;
        }

        private void VisitFirstMemberAccess(MemberAccessExpression node)
        {
            var symbols = node.SymbolTable!.GetId(node.Name);
            if (symbols is not [])
            {
                node.Reference = new AggregateMetadata(symbols.Select(x => x.Metadata!));
                return;
            }

            // static access
            var metadataProvider = node.MetadataProvider!;
            var function = metadataProvider.FindFunctions(node.Name);
            var types = metadataProvider.QueryTypes(new ByName(node.Name));
            var openGenerics = metadataProvider.QueryTypes(new GetOpenGeneric(node.Name));
            var members = function.Cast<IMetadata>()
                .Concat(types.Types)
                .Concat(openGenerics.Types)
                .ToArray();

            if (members.Length > 0)
                node.Reference = new AggregateMetadata(members);

            // resolve namespaces for fully-qualified names
            if (node.Reference is null)
            {
                // TODO: add support of package prefix `::`
                var namespaceResult = compilationContext.FindNamespace([node.Name]);
                if (namespaceResult.IsSuccess)
                    node.Reference = namespaceResult.Namespace;
            }
        }

        private MemberAccessExpression VisitNestedMemberAccess(MemberAccessExpression node)
        {
            var member = (IExpression)node.Member!.Transform(this);

            if (member is MemberAccessExpression { Reference: NamespaceMetadata ns })
            {
                var provider = new MetadataProvider(compilationContext, ns);
                var result = provider.QueryTypes(new ByName(node.Name));
                if (result.IsSuccess)
                {
                    node.Reference = new AggregateMetadata([result.Types[0]]);
                    return node;
                }

                var @namespace = ns.GetNamespace(node.Name);
                if (@namespace is not null)
                {
                    node.Reference = @namespace;
                    return node;
                }

                diagnostics.UnknownSymbol(node);
                node.Reference = InvalidMemberMetadata.Instance;
                return node;
            }

            ResolveSingle(member);

            var returnTypeMetadata = member.ReturnTypeMetadata!;
            node.Reference = returnTypeMetadata.IsInvalid
                ? InvalidMemberMetadata.Instance
                : returnTypeMetadata.GetMembers(node.Name);

            if (node.Member == member)
                return node;

            return new MemberAccessExpression(node.SourceSpan, member, node.Name)
            {
                AccessKind = node.AccessKind,
                Reference = node.Reference,
                SymbolTable = node.SymbolTable,
                MetadataProvider = node.MetadataProvider,
            };
        }

        public override ISemanticNode TransformNewObject(NewObjectExpression node)
        {
            node = (NewObjectExpression)base.TransformNewObject(node);

            if (node.Member is CallExpression call)
            {
                if (call.Reference!.IsInvalid)
                {
                    node.ReturnTypeMetadata = TypeMetadata.InvalidType;
                    return node;
                }

                if (call.Reference is not ConstructorMetadata ctor)
                {
                    node.ReturnTypeMetadata = TypeMetadata.InvalidType;
                    diagnostics.ExpectedConstructor(node.Member, call.Reference!);

                    return node;
                }

                var provider = node.MetadataProvider!;
                var metadataFactory = new MetadataFactory(builtInTypes, diagnostics, provider);
                var pointer = metadataFactory.CreatePointer(null, ctor.Type.ReturnType);
                node.ReturnTypeMetadata = pointer;

                node.Metadata = ctor;
            }
            else if (node.Member is ArrayAccessExpression arrayAccess)
            {
                if (arrayAccess.Reference is not ArrayMetadata)
                {
                    node.ReturnTypeMetadata = TypeMetadata.InvalidType;

                    return node;
                }

                node.ReturnTypeMetadata = arrayAccess.ReturnTypeMetadata;
                node.Metadata = null; // TODO: array ctor?
            }
            else
            {
                ResolveSingle(node.Member);
                node.ReturnTypeMetadata = TypeMetadata.InvalidType;
                diagnostics.ExpectedCtorOrArray(node);
            }

            return node;
        }

        public override ISemanticNode TransformReturn(ReturnStatement node)
        {
            node = (ReturnStatement)base.TransformReturn(node);

            if (node.Expression is not null)
                ResolveSingle(node.Expression);

            var expressionType = node.Expression?.ReturnTypeMetadata ?? builtInTypes.Void;
            if (expressionType.IsInvalid)
                return node;

            var method = node.FindInParent<MethodDeclaration>();
            if (method is not null)
            {
                var methodReturnType = method.Metadata!.Type.ReturnType;
                if (!methodReturnType.IsInvalid && !Equals(methodReturnType, expressionType))
                    diagnostics.ReturnTypeMismatch(node, methodReturnType, expressionType);

                return node;
            }

            var constructor = node.FindInParent<ConstructorDeclaration>();
            if (constructor is not null)
            {
                if (!Equals(builtInTypes.Void, expressionType))
                    diagnostics.ReturnTypeMismatch(node, builtInTypes.Void, expressionType);

                return node;
            }

            var function = node.FindInParent<FunctionDeclaration>();
            if (function is not null)
            {
                var functionReturnType = function.Metadata!.Type.ReturnType;
                if (!functionReturnType.IsInvalid && !Equals(functionReturnType, expressionType))
                    diagnostics.ReturnTypeMismatch(node, functionReturnType, expressionType);

                return node;
            }

            var getter = node.FindInParent<PropertyGetter>();
            if (getter is not null)
            {
                var getterReturnType = ((PropertyDeclaration)getter.Parent!).Metadata!.Type;
                if (!getterReturnType.IsInvalid && !Equals(getterReturnType, expressionType))
                    diagnostics.ReturnTypeMismatch(node, getterReturnType, expressionType);

                return node;
            }

            var setter = node.FindInParent<PropertySetter>();
            if (setter is not null)
            {
                if (!Equals(builtInTypes.Void, expressionType))
                    diagnostics.ReturnTypeMismatch(node, builtInTypes.Void, expressionType);

                return node;
            }

            return node;
        }

        public override ISemanticNode TransformTuple(TupleExpression node)
        {
            node = (TupleExpression)base.TransformTuple(node);

            foreach (var expression in node.Expressions)
                ResolveSingle(expression);

            var metadataProvider = node.MetadataProvider!;
            var metadataFactory = new MetadataFactory(builtInTypes, diagnostics, metadataProvider);

            // we can't generate metadata for this tuple in GenerateMetadata
            // because we don't know the types of the expressions yet
            var types = node.Expressions.Select(x => x.ReturnTypeMetadata!).ToArray();
            var tuple = metadataFactory.CreateTupleMetadata(null, types);

            node.ReturnTypeMetadata = tuple;

            return node;
        }

        public override ISemanticNode TransformUnaryExpression(UnaryExpression node)
        {
            node = (UnaryExpression)base.TransformUnaryExpression(node);

            ResolveSingle(node.Operand);

            var result = unaryTypeMatcher.Match(node.Kind, node.Operand.ReturnTypeMetadata);
            if (result is FailedMatch)
            {
                node.ReturnTypeMetadata = TypeMetadata.InvalidType;
                diagnostics.IncompatibleUnaryOperand(node);
                return node;
            }

            if (result is not SuccessMatch)
                return node;

            if (node.Kind is UnaryMinus or UnaryPlus or BitwiseNot)
            {
                node.ReturnTypeMetadata = node.Operand.ReturnTypeMetadata;
            }
            else if (node.Kind is LogicalNot)
            {
                node.ReturnTypeMetadata = builtInTypes.Bool;
            }
            else if (node.Kind == AddressOf)
            {
                var provider = node.MetadataProvider!;
                var metadataFactory = new MetadataFactory(builtInTypes, diagnostics, provider);
                var pointer = metadataFactory.CreatePointer(null, node.Operand.ReturnTypeMetadata!);
                node.ReturnTypeMetadata = pointer;
            }
            else if (node is { Kind: Dereference, Operand.ReturnTypeMetadata: PointerMetadata pointer })
            {
                node.ReturnTypeMetadata = pointer.Type;
            }

            return node;
        }

        public override ISemanticNode TransformVariable(VariableDeclaration node)
        {
            var type = (IInlineType)node.Type.Transform(this);
            var expression = (IExpression)node.Expression.Transform(this);

            Debug.Assert(expression.ReturnTypeMetadata is not null);
            Debug.Assert(type.Metadata is not null);

            ResolveExpression(expression, type.Metadata);

            var result = typeMatcher.Match(expression.ReturnTypeMetadata, type.Metadata);
            if (result is FailedMatch)
            {
                diagnostics.TypeMismatch(expression, type.Metadata, expression.ReturnTypeMetadata);
            }
            else if (result is ImplicitConversion implicitConversion)
            {
                expression = new CastExpression(null, GetInlineType(node, implicitConversion.Target), expression)
                {
                    SymbolTable = node.SymbolTable,
                    MetadataProvider = node.MetadataProvider,
                };
            }

            if (type == node.Type && expression == node.Expression)
                return node;

            return new VariableDeclaration(node.SourceSpan, node.Name, type, expression)
            {
                Metadata = node.Metadata,
                SymbolTable = node.SymbolTable,
                MetadataProvider = node.MetadataProvider,
            };
        }

        public override ISemanticNode TransformWhile(While node)
        {
            node = (While)base.TransformWhile(node);

            ResolveSingle(node.Condition);

            if (!Equals(node.Condition.ReturnTypeMetadata, builtInTypes.Bool))
            {
                diagnostics.TypeMismatch(
                    node.Condition,
                    builtInTypes.Bool,
                    node.Condition.ReturnTypeMetadata);
            }

            return node;
        }
    }
}
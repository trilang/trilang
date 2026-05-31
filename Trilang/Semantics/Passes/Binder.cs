using System.Diagnostics;
using Trilang.Compilation;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Metadata.Aggregate;
using Trilang.Semantics.Model;
using Trilang.Semantics.Providers;
using static Trilang.Semantics.Model.BinaryExpressionKind;
using static Trilang.Semantics.Model.UnaryExpressionKind;

namespace Trilang.Semantics.Passes;

internal class Binder : ISemanticPass
{
    private readonly ISet<string> directives;
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly SymbolTableMap symbolTableMap;
    private readonly MetadataProviderMap metadataProviderMap;
    private readonly CompilationContext compilationContext;

    public Binder(
        ISet<string> directives,
        DiagnosticCollection diagnostics,
        SymbolTableMap symbolTableMap,
        MetadataProviderMap metadataProviderMap,
        CompilationContext compilationContext)
    {
        this.directives = directives;
        this.diagnostics = diagnostics.ForSemantic();
        this.symbolTableMap = symbolTableMap;
        this.metadataProviderMap = metadataProviderMap;
        this.compilationContext = compilationContext;
    }

    public void Analyze(Project project)
    {
        var semanticTrees = project.SourceFiles.Select(x => x.SemanticTree!);
        var visitor = new TypeCheckerVisitor(
            directives,
            diagnostics,
            symbolTableMap,
            metadataProviderMap,
            compilationContext);

        foreach (var tree in semanticTrees)
            tree.Accept(visitor);
    }

    public string Name => nameof(Binder);

    public IEnumerable<string> DependsOn => [nameof(MetadataGenerator)];

    private sealed class TypeCheckerVisitor : Visitor
    {
        private readonly SemanticDiagnosticReporter diagnostics;
        private readonly SymbolTableMap symbolTableMap;
        private readonly MetadataProviderMap metadataProviderMap;
        private readonly CompilationContext compilationContext;
        private readonly BuiltInTypes builtInTypes;

        public TypeCheckerVisitor(
            ISet<string> directives,
            SemanticDiagnosticReporter diagnostics,
            SymbolTableMap symbolTableMap,
            MetadataProviderMap metadataProviderMap,
            CompilationContext compilationContext)
            : base(directives)
        {
            this.diagnostics = diagnostics;
            this.symbolTableMap = symbolTableMap;
            this.metadataProviderMap = metadataProviderMap;
            this.compilationContext = compilationContext;
            builtInTypes = compilationContext.BuiltInTypes;
        }

        private void ResolveSingle(IExpression member)
        {
            if (member is IAccessExpression { Reference: AggregateMetadata aggregate } accessExpression)
            {
                var result = aggregate.ResolveSingle();
                if (result is NoMatch<IMetadata>)
                    diagnostics.NoMemberFound(accessExpression);
                else if (result is MultipleMatches<IMetadata> multipleMembers)
                    diagnostics.MultipleMembersFound(accessExpression, multipleMembers.Candidates);

                accessExpression.Reference = result.Member;
            }
        }

        private void ResolveFunction(CallExpression node)
        {
            var aggregate = (AggregateMetadata)node.Member.Reference!;
            var result = aggregate.ResolveFunction(node.Parameters.Select(x => x.ReturnTypeMetadata!).ToArray());
            if (result is ExtraArgument<IFunctionMetadata> extraArgument)
            {
                foreach (var position in extraArgument.Positions)
                    diagnostics.ExtraArgument(node.Parameters[position]);
            }
            else if (result is MissingArgument<IFunctionMetadata> missingArgument)
            {
                foreach (var position in missingArgument.Positions)
                    diagnostics.MissingArgument(node, missingArgument.Match.Type.ParameterTypes[position]);
            }
            else if (result is ArgumentMismatch<IFunctionMetadata> argumentMismatch)
            {
                foreach (var detail in argumentMismatch.Mismatches)
                    diagnostics.TypeMismatch(node.Parameters[detail.Position], detail.Expected, detail.Actual);
            }
            else if (result is NotFunction notFunction)
            {
                diagnostics.ExpectedFunction(node.Member, notFunction.Candidate);
            }
            else if (result is NoMatch<IFunctionMetadata>)
            {
                diagnostics.NoSuitableOverload(node.Member);
            }
            else if (result is MultipleMatches<IFunctionMetadata> multipleOverloads)
            {
                diagnostics.MultipleCandidates(node.Member, multipleOverloads.Candidates);
            }

            node.Member.Reference = result.Member;
        }

        private void ResolveFunction(IExpression node, FunctionTypeMetadata functionType)
        {
            if (node is not IAccessExpression accessExpression)
                return;

            var parameters = functionType.ParameterTypes;
            var aggregate = (AggregateMetadata)accessExpression.Reference!;
            var result = aggregate.ResolveFunction(parameters);
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
            else if (result is NoMatch<IFunctionMetadata>)
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
            var result = aggregate!.ResolveGeneric(node.GenericArguments.Count);
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

        public override void VisitCast(CastExpression node)
        {
            base.VisitCast(node);

            ResolveExpression(node.Expression, node.ReturnTypeMetadata);
        }

        public override void VisitArrayAccess(ArrayAccessExpression node)
        {
            base.VisitArrayAccess(node);

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
                var provider = metadataProviderMap.Get(node);
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
        }

        public override void VisitBinaryExpression(BinaryExpression node)
        {
            base.VisitBinaryExpression(node);

            Debug.Assert(node.Kind != BinaryExpressionKind.Unknown);
            Debug.Assert(node.Left.ReturnTypeMetadata is not null);
            Debug.Assert(node.Right.ReturnTypeMetadata is not null);

            ResolveSingle(node.Left);
            ResolveSingle(node.Right);

            // TODO: more complex logic
            if (node.Kind is Addition or Subtraction or Multiplication or Division or Modulus &&
                ((Equals(node.Left.ReturnTypeMetadata, builtInTypes.I8) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.I8)) ||
                 (Equals(node.Left.ReturnTypeMetadata, builtInTypes.I16) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.I16)) ||
                 (Equals(node.Left.ReturnTypeMetadata, builtInTypes.I32) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.I32)) ||
                 (Equals(node.Left.ReturnTypeMetadata, builtInTypes.I64) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.I64)) ||
                 (Equals(node.Left.ReturnTypeMetadata, builtInTypes.U8) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.U8)) ||
                 (Equals(node.Left.ReturnTypeMetadata, builtInTypes.U16) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.U16)) ||
                 (Equals(node.Left.ReturnTypeMetadata, builtInTypes.U32) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.U32)) ||
                 (Equals(node.Left.ReturnTypeMetadata, builtInTypes.U64) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.U64)) ||
                 (Equals(node.Left.ReturnTypeMetadata, builtInTypes.F32) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.F32)) ||
                 (Equals(node.Left.ReturnTypeMetadata, builtInTypes.F64) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.F64))))
            {
                node.ReturnTypeMetadata = node.Left.ReturnTypeMetadata;
            }
            else if (node.Kind is BitwiseAnd or BitwiseOr or BitwiseXor &&
                     ((Equals(node.Left.ReturnTypeMetadata, builtInTypes.I8) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.I8)) ||
                      (Equals(node.Left.ReturnTypeMetadata, builtInTypes.I16) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.I16)) ||
                      (Equals(node.Left.ReturnTypeMetadata, builtInTypes.I32) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.I32)) ||
                      (Equals(node.Left.ReturnTypeMetadata, builtInTypes.I64) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.I64)) ||
                      (Equals(node.Left.ReturnTypeMetadata, builtInTypes.U8) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.U8)) ||
                      (Equals(node.Left.ReturnTypeMetadata, builtInTypes.U16) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.U16)) ||
                      (Equals(node.Left.ReturnTypeMetadata, builtInTypes.U32) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.U32)) ||
                      (Equals(node.Left.ReturnTypeMetadata, builtInTypes.U64) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.U64))))
            {
                node.ReturnTypeMetadata = node.Left.ReturnTypeMetadata;
            }
            else if (node.Kind is ConditionalAnd or ConditionalOr &&
                     Equals(node.Left.ReturnTypeMetadata, builtInTypes.Bool) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.Bool))
            {
                node.ReturnTypeMetadata = builtInTypes.Bool;
            }
            else if (node.Kind is Equality or Inequality or
                         LessThan or LessThanOrEqual or
                         GreaterThan or GreaterThanOrEqual &&
                     ((Equals(node.Left.ReturnTypeMetadata, builtInTypes.I8) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.I8)) ||
                      (Equals(node.Left.ReturnTypeMetadata, builtInTypes.I16) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.I16)) ||
                      (Equals(node.Left.ReturnTypeMetadata, builtInTypes.I32) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.I32)) ||
                      (Equals(node.Left.ReturnTypeMetadata, builtInTypes.I64) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.I64)) ||
                      (Equals(node.Left.ReturnTypeMetadata, builtInTypes.U8) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.U8)) ||
                      (Equals(node.Left.ReturnTypeMetadata, builtInTypes.U16) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.U16)) ||
                      (Equals(node.Left.ReturnTypeMetadata, builtInTypes.U32) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.U32)) ||
                      (Equals(node.Left.ReturnTypeMetadata, builtInTypes.U64) && Equals(node.Right.ReturnTypeMetadata, builtInTypes.U64))))
            {
                node.ReturnTypeMetadata = builtInTypes.Bool;
            }
            else if (node.Kind is Assignment or AdditionAssignment or
                         SubtractionAssignment or MultiplicationAssignment or
                         DivisionAssignment or ModulusAssignment &&
                     node.Left is MemberAccessExpression or ArrayAccessExpression &&
                     Equals(node.Left.ReturnTypeMetadata, node.Right.ReturnTypeMetadata))
            {
                node.ReturnTypeMetadata = node.Right.ReturnTypeMetadata;
            }
            else if (node.Kind is AdditionAssignment or SubtractionAssignment or MultiplicationAssignment or DivisionAssignment or ModulusAssignment &&
                     node.Left is MemberAccessExpression &&
                     (Equals(node.Right.ReturnTypeMetadata, builtInTypes.I8) ||
                      Equals(node.Right.ReturnTypeMetadata, builtInTypes.I16) ||
                      Equals(node.Right.ReturnTypeMetadata, builtInTypes.I32) ||
                      Equals(node.Right.ReturnTypeMetadata, builtInTypes.I64) ||
                      Equals(node.Right.ReturnTypeMetadata, builtInTypes.U8) ||
                      Equals(node.Right.ReturnTypeMetadata, builtInTypes.U16) ||
                      Equals(node.Right.ReturnTypeMetadata, builtInTypes.U32) ||
                      Equals(node.Right.ReturnTypeMetadata, builtInTypes.U64) ||
                      Equals(node.Right.ReturnTypeMetadata, builtInTypes.F32) ||
                      Equals(node.Right.ReturnTypeMetadata, builtInTypes.F64)))
            {
                node.ReturnTypeMetadata = node.Right.ReturnTypeMetadata;
            }
            else if (node.Kind is BitwiseAndAssignment or BitwiseOrAssignment or BitwiseXorAssignment &&
                     node.Left is MemberAccessExpression &&
                     (Equals(node.Right.ReturnTypeMetadata, builtInTypes.I8) ||
                      Equals(node.Right.ReturnTypeMetadata, builtInTypes.I16) ||
                      Equals(node.Right.ReturnTypeMetadata, builtInTypes.I32) ||
                      Equals(node.Right.ReturnTypeMetadata, builtInTypes.I64) ||
                      Equals(node.Right.ReturnTypeMetadata, builtInTypes.U8) ||
                      Equals(node.Right.ReturnTypeMetadata, builtInTypes.U16) ||
                      Equals(node.Right.ReturnTypeMetadata, builtInTypes.U32) ||
                      Equals(node.Right.ReturnTypeMetadata, builtInTypes.U64)))
            {
                node.ReturnTypeMetadata = node.Right.ReturnTypeMetadata;
            }
            else
            {
                node.ReturnTypeMetadata = TypeMetadata.InvalidType;
                diagnostics.IncompatibleBinaryOperand(node);
            }
        }

        public override void VisitCall(CallExpression node)
        {
            base.VisitCall(node);

            if (node.Member.Reference!.IsInvalid)
            {
                node.Member.Reference = InvalidMemberMetadata.Instance;
                return;
            }

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
            var aggregate = (AggregateMetadata)node.Member.Reference;
            var function = TryResolveSingleFunction(aggregate);
            for (var i = 0; i < node.Parameters.Count; i++)
            {
                var parameter = node.Parameters[i];
                var returnType = function is not null && i < function.Type.ParameterTypes.Count
                    ? function.Type.ParameterTypes[i]
                    : null;

                ResolveExpression(parameter, returnType);
            }

            ResolveFunction(node);
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

        public override void VisitExpressionBlock(ExpressionBlock node)
            => Debug.Fail("Expression blocks are not supported");

        public override void VisitExpressionStatement(ExpressionStatement node)
        {
            base.VisitExpressionStatement(node);

            ResolveSingle(node.Expression);
        }

        public override void VisitGenericExpression(GenericExpression node)
        {
            base.VisitGenericExpression(node);

            var result = ResolveGeneric(node);
            if (result.IsInvalid)
                return;

            var provider = metadataProviderMap.Get(node);
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
        }

        public override void VisitIf(IfStatement node)
        {
            base.VisitIf(node);

            ResolveSingle(node.Condition);

            if (!Equals(node.Condition.ReturnTypeMetadata, builtInTypes.Bool))
                diagnostics.TypeMismatch(node.Condition, builtInTypes.Bool, node.Condition.ReturnTypeMetadata);
        }

        public override void VisitIsExpression(IsExpression node)
        {
            base.VisitIsExpression(node);

            ResolveSingle(node.Expression);
        }

        public override void VisitLiteral(LiteralExpression node)
        {
            base.VisitLiteral(node);

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
        }

        public override void VisitMemberAccess(MemberAccessExpression node)
        {
            if (node.IsFirstMember)
            {
                VisitFirstMemberAccess(node);
                return;
            }

            VisitNestedMemberAccess(node);

            Debug.Assert(node.Reference is not null);
        }

        private void VisitFirstMemberAccess(MemberAccessExpression node)
        {
            var metadataProvider = metadataProviderMap.Get(node);
            var symbolTable = symbolTableMap.Get(node);
            var symbols = symbolTable.GetId(node.Name);
            if (symbols is not [])
            {
                if (node.IsThis)
                {
                    var type = ((TypeDeclaration)symbols[0].Node).Metadata!;
                    var metadataFactory = new MetadataFactory(builtInTypes, diagnostics, metadataProvider);
                    var pointer = metadataFactory.CreatePointer(null, type);
                    node.Reference = new AggregateMetadata([
                        new ParameterMetadata(null, MemberAccessExpression.This, pointer)
                    ]);

                    return;
                }

                node.Reference = new AggregateMetadata(symbols.Select(x => GetSymbolMetadata(x.Node)));

                return;
            }

            // static access
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

        private void VisitNestedMemberAccess(MemberAccessExpression node)
        {
            node.Member!.Accept(this);

            if (node.Member is MemberAccessExpression { Reference: NamespaceMetadata ns })
            {
                var provider = new MetadataProvider(compilationContext, ns);
                var result = provider.QueryTypes(new ByName(node.Name));
                if (result.IsSuccess)
                {
                    node.Reference = new AggregateMetadata([result.Types[0]]);
                    return;
                }

                var @namespace = ns.GetNamespace(node.Name);
                if (@namespace is not null)
                {
                    node.Reference = @namespace;
                    return;
                }

                diagnostics.UnknownSymbol(node);
                node.Reference = InvalidMemberMetadata.Instance;
                return;
            }

            ResolveSingle(node.Member);

            var returnTypeMetadata = node.Member.ReturnTypeMetadata!;
            node.Reference = returnTypeMetadata.IsInvalid
                ? InvalidMemberMetadata.Instance
                : returnTypeMetadata.GetMembers(node.Name);
        }

        private static IMetadata GetSymbolMetadata(ISemanticNode node)
            => node switch
            {
                PropertyDeclaration propertyDeclarationNode
                    => propertyDeclarationNode.Metadata!,

                VariableDeclaration variableStatementNode
                    => variableStatementNode.Metadata!,

                Parameter parameterNode
                    => parameterNode.Metadata!,

                MethodDeclaration methodNode
                    => methodNode.Metadata!,

                _ => throw new InvalidOperationException(),
            };

        public override void VisitNewObject(NewObjectExpression node)
        {
            base.VisitNewObject(node);

            if (node.Member is CallExpression call)
            {
                if (call.Reference!.IsInvalid)
                {
                    node.ReturnTypeMetadata = TypeMetadata.InvalidType;
                    return;
                }

                if (call.Reference is not ConstructorMetadata ctor)
                {
                    node.ReturnTypeMetadata = TypeMetadata.InvalidType;
                    diagnostics.ExpectedConstructor(node.Member, call.Reference!);

                    return;
                }

                var provider = metadataProviderMap.Get(node);
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

                    return;
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
        }

        public override void VisitReturn(ReturnStatement node)
        {
            base.VisitReturn(node);

            if (node.Expression is not null)
                ResolveSingle(node.Expression);

            var expressionType = node.Expression?.ReturnTypeMetadata ?? builtInTypes.Void;
            if (expressionType.IsInvalid)
                return;

            var method = node.FindInParent<MethodDeclaration>();
            if (method is not null)
            {
                var methodReturnType = method.Metadata!.Type.ReturnType;
                if (!methodReturnType.IsInvalid && !Equals(methodReturnType, expressionType))
                    diagnostics.ReturnTypeMismatch(node, methodReturnType, expressionType);

                return;
            }

            var constructor = node.FindInParent<ConstructorDeclaration>();
            if (constructor is not null)
            {
                if (!Equals(builtInTypes.Void, expressionType))
                    diagnostics.ReturnTypeMismatch(node, builtInTypes.Void, expressionType);

                return;
            }

            var function = node.FindInParent<FunctionDeclaration>();
            if (function is not null)
            {
                var functionReturnType = function.Metadata!.Type.ReturnType;
                if (!functionReturnType.IsInvalid && !Equals(functionReturnType, expressionType))
                    diagnostics.ReturnTypeMismatch(node, functionReturnType, expressionType);

                return;
            }

            var getter = node.FindInParent<PropertyGetter>();
            if (getter is not null)
            {
                var getterReturnType = ((PropertyDeclaration)getter.Parent!).Metadata!.Type;
                if (!getterReturnType.IsInvalid && !Equals(getterReturnType, expressionType))
                    diagnostics.ReturnTypeMismatch(node, getterReturnType, expressionType);

                return;
            }

            var setter = node.FindInParent<PropertySetter>();
            if (setter is not null)
            {
                if (!Equals(builtInTypes.Void, expressionType))
                    diagnostics.ReturnTypeMismatch(node, builtInTypes.Void, expressionType);

                return;
            }
        }

        public override void VisitTuple(TupleExpression node)
        {
            base.VisitTuple(node);

            foreach (var expression in node.Expressions)
                ResolveSingle(expression);

            var metadataProvider = metadataProviderMap.Get(node);
            var metadataFactory = new MetadataFactory(builtInTypes, diagnostics, metadataProvider);

            // we can't generate metadata for this tuple in GenerateMetadata
            // because we don't know the types of the expressions yet
            var types = node.Expressions.Select(x => x.ReturnTypeMetadata!).ToArray();
            var tuple = metadataFactory.CreateTupleMetadata(null, types);

            node.ReturnTypeMetadata = tuple;
        }

        public override void VisitUnaryExpression(UnaryExpression node)
        {
            base.VisitUnaryExpression(node);

            Debug.Assert(node.Kind != UnaryExpressionKind.Unknown);
            Debug.Assert(node.Operand.ReturnTypeMetadata is not null);

            ResolveSingle(node.Operand);

            // TODO: more complex logic
            if (node.Kind == UnaryMinus &&
                (Equals(node.Operand.ReturnTypeMetadata, builtInTypes.I8) ||
                 Equals(node.Operand.ReturnTypeMetadata, builtInTypes.I16) ||
                 Equals(node.Operand.ReturnTypeMetadata, builtInTypes.I32) ||
                 Equals(node.Operand.ReturnTypeMetadata, builtInTypes.I64) ||
                 Equals(node.Operand.ReturnTypeMetadata, builtInTypes.U8) ||
                 Equals(node.Operand.ReturnTypeMetadata, builtInTypes.U16) ||
                 Equals(node.Operand.ReturnTypeMetadata, builtInTypes.U32) ||
                 Equals(node.Operand.ReturnTypeMetadata, builtInTypes.U64) ||
                 Equals(node.Operand.ReturnTypeMetadata, builtInTypes.F32) ||
                 Equals(node.Operand.ReturnTypeMetadata, builtInTypes.F64)))
            {
                node.ReturnTypeMetadata = node.Operand.ReturnTypeMetadata;
            }
            else if (node.Kind == UnaryPlus &&
                     (Equals(node.Operand.ReturnTypeMetadata, builtInTypes.I8) ||
                      Equals(node.Operand.ReturnTypeMetadata, builtInTypes.I16) ||
                      Equals(node.Operand.ReturnTypeMetadata, builtInTypes.I32) ||
                      Equals(node.Operand.ReturnTypeMetadata, builtInTypes.I64) ||
                      Equals(node.Operand.ReturnTypeMetadata, builtInTypes.U8) ||
                      Equals(node.Operand.ReturnTypeMetadata, builtInTypes.U16) ||
                      Equals(node.Operand.ReturnTypeMetadata, builtInTypes.U32) ||
                      Equals(node.Operand.ReturnTypeMetadata, builtInTypes.U64) ||
                      Equals(node.Operand.ReturnTypeMetadata, builtInTypes.F32) ||
                      Equals(node.Operand.ReturnTypeMetadata, builtInTypes.F64)))
            {
                node.ReturnTypeMetadata = node.Operand.ReturnTypeMetadata;
            }
            else if (node.Kind == LogicalNot && Equals(node.Operand.ReturnTypeMetadata, builtInTypes.Bool))
            {
                node.ReturnTypeMetadata = builtInTypes.Bool;
            }
            else if (node.Kind == BitwiseNot &&
                     (Equals(node.Operand.ReturnTypeMetadata, builtInTypes.I8) ||
                      Equals(node.Operand.ReturnTypeMetadata, builtInTypes.I16) ||
                      Equals(node.Operand.ReturnTypeMetadata, builtInTypes.I32) ||
                      Equals(node.Operand.ReturnTypeMetadata, builtInTypes.I64) ||
                      Equals(node.Operand.ReturnTypeMetadata, builtInTypes.U8) ||
                      Equals(node.Operand.ReturnTypeMetadata, builtInTypes.U16) ||
                      Equals(node.Operand.ReturnTypeMetadata, builtInTypes.U32) ||
                      Equals(node.Operand.ReturnTypeMetadata, builtInTypes.U64)))
            {
                node.ReturnTypeMetadata = node.Operand.ReturnTypeMetadata;
            }
            else if (node.Kind == AddressOf)
            {
                var provider = metadataProviderMap.Get(node);
                var metadataFactory = new MetadataFactory(builtInTypes, diagnostics, provider);
                var pointer = metadataFactory.CreatePointer(null, node.Operand.ReturnTypeMetadata);
                node.ReturnTypeMetadata = pointer;
            }
            else if (node is { Kind: Dereference, Operand.ReturnTypeMetadata: PointerMetadata pointer })
            {
                node.ReturnTypeMetadata = pointer.Type;
            }
            else
            {
                node.ReturnTypeMetadata = TypeMetadata.InvalidType;
                diagnostics.IncompatibleUnaryOperand(node);
            }
        }

        public override void VisitVariable(VariableDeclaration node)
        {
            base.VisitVariable(node);

            Debug.Assert(node.Expression.ReturnTypeMetadata is not null);
            Debug.Assert(node.Type.Metadata is not null);

            ResolveExpression(node.Expression, node.Type.Metadata);

            if (!node.Expression.ReturnTypeMetadata.IsInvalid &&
                !node.Type.Metadata.IsInvalid &&
                !node.Expression.ReturnTypeMetadata.Equals(node.Type.Metadata))
                diagnostics.TypeMismatch(node.Expression, node.Type.Metadata, node.Expression.ReturnTypeMetadata);
        }

        public override void VisitWhile(While node)
        {
            base.VisitWhile(node);

            ResolveSingle(node.Condition);

            if (!Equals(node.Condition.ReturnTypeMetadata, builtInTypes.Bool))
                diagnostics.TypeMismatch(node.Condition, builtInTypes.Bool, node.Condition.ReturnTypeMetadata);
        }
    }
}
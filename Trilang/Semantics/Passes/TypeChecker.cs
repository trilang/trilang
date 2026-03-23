using System.Diagnostics;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Semantics.Providers;
using static Trilang.Semantics.Model.BinaryExpressionKind;
using static Trilang.Semantics.Model.UnaryExpressionKind;

namespace Trilang.Semantics.Passes;

internal class TypeChecker : ISemanticPass
{
    private readonly ISet<string> directives;
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly SymbolTableMap symbolTableMap;
    private readonly MetadataProviderMap metadataProviderMap;
    private readonly NamespaceMetadata rootNamespace;
    private readonly BuiltInTypes builtInTypes;

    public TypeChecker(ISet<string> directives,
        SemanticDiagnosticReporter diagnostics,
        SymbolTableMap symbolTableMap,
        MetadataProviderMap metadataProviderMap,
        NamespaceMetadata rootNamespace,
        BuiltInTypes builtInTypes)
    {
        this.directives = directives;
        this.diagnostics = diagnostics;
        this.symbolTableMap = symbolTableMap;
        this.metadataProviderMap = metadataProviderMap;
        this.rootNamespace = rootNamespace;
        this.builtInTypes = builtInTypes;
    }

    public void Analyze(IEnumerable<SemanticTree> semanticTrees)
    {
        var visitor = new TypeCheckerVisitor(
            directives,
            diagnostics,
            symbolTableMap,
            metadataProviderMap,
            rootNamespace,
            builtInTypes);

        foreach (var tree in semanticTrees)
            tree.Accept(visitor);
    }

    public string Name => nameof(TypeChecker);

    public IEnumerable<string> DependsOn => [nameof(MetadataGenerator)];

    private sealed class TypeCheckerVisitor : Visitor
    {
        private readonly SemanticDiagnosticReporter diagnostics;
        private readonly SymbolTableMap symbolTableMap;
        private readonly MetadataProviderMap metadataProviderMap;
        private readonly NamespaceMetadata rootNamespace;
        private readonly BuiltInTypes builtInTypes;

        public TypeCheckerVisitor(ISet<string> directives,
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
        }

        public override void VisitArrayAccess(ArrayAccessExpression node)
        {
            base.VisitArrayAccess(node);

            if (node.Member.ReturnTypeMetadata is ArrayMetadata { IsInvalid: false } typeArray)
            {
                Debug.Assert(typeArray.ItemMetadata is not null);
                node.ReturnTypeMetadata = typeArray.ItemMetadata;
            }
            else
            {
                diagnostics.ExpectedArray(node.Member);
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

            Debug.Assert(node.Member.ReturnTypeMetadata is not null);

            ResolveFunction(node.Member, node.Parameters.Select(x => x.ReturnTypeMetadata!));

            if (node.Member.ReturnTypeMetadata is FunctionTypeMetadata function)
            {
                var expectedCount = function.ParameterTypes.Count;
                var actualCount = node.Parameters.Count;

                if (actualCount > expectedCount)
                    for (var i = expectedCount; i < actualCount; i++)
                        diagnostics.ExtraArgument(node.Parameters[i]);

                if (actualCount < expectedCount)
                    for (var i = actualCount; i < expectedCount; i++)
                        diagnostics.MissingArgument(node, function.ParameterTypes[i]);

                for (var i = 0; i < Math.Min(expectedCount, actualCount); i++)
                {
                    var parameter = node.Parameters[i];
                    var expected = function.ParameterTypes[i];
                    if (expected.IsInvalid)
                        continue;

                    ResolveFunction(parameter, expected);
                    var actual = parameter.ReturnTypeMetadata!;
                    if (actual.IsInvalid)
                        continue;

                    if (!expected.Equals(actual))
                        diagnostics.TypeMismatch(parameter, expected, actual);
                }
            }
            else if (!node.Member.ReturnTypeMetadata.IsInvalid)
            {
                diagnostics.ExpectedFunction(node.Member);
            }
        }

        private void ResolveFunction(IExpression node, ITypeMetadata expectedType)
        {
            expectedType = expectedType.UnpackAlias()!;

            if (expectedType is not FunctionTypeMetadata functionType)
                return;

            ResolveFunction(node, functionType.ParameterTypes);
        }

        private void ResolveFunction(IExpression node, IEnumerable<ITypeMetadata> actualParameters)
        {
            if (node is not MemberAccessExpression member)
                return;

            if (member.Reference is not AggregateMetadata aggregate)
                return;

            var functions = aggregate.Members.OfType<IFunctionMetadata>().ToArray();
            if (functions.Length == 1)
            {
                member.Reference = functions[0];
            }
            else
            {
                var candidates = aggregate.MatchFunction(actualParameters).ToArray();
                if (candidates.Length == 0)
                {
                    member.Reference = new InvalidMemberMetadata(member.Name);

                    diagnostics.NoSuitableOverload(member);
                }
                else
                {
                    member.Reference = candidates[0];

                    if (candidates.Length > 1)
                        diagnostics.MultipleOverloads(member);
                }
            }
        }

        public override void VisitExpressionBlock(ExpressionBlock node)
            => Debug.Fail("Expression blocks are not supported");

        public override void VisitIf(IfStatement node)
        {
            base.VisitIf(node);

            if (!Equals(node.Condition.ReturnTypeMetadata, builtInTypes.Bool))
                diagnostics.TypeMismatch(node.Condition, builtInTypes.Bool, node.Condition.ReturnTypeMetadata);
        }

        public override void VisitLiteral(LiteralExpression node)
        {
            base.VisitLiteral(node);

            node.ReturnTypeMetadata = node.Kind switch
            {
                // TODO: other types
                LiteralExpressionKind.Integer => builtInTypes.I32,
                LiteralExpressionKind.Float => builtInTypes.F64,
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
            var symbolTable = symbolTableMap.Get(node);
            var symbols = symbolTable.GetId(node.Name);
            if (symbols is not [])
            {
                if (node.IsThis)
                {
                    node.Reference = new ParameterMetadata(
                        null,
                        MemberAccessExpression.This,
                        ((TypeDeclaration)symbols[0].Node).Metadata!);

                    return;
                }

                node.Reference = symbols switch
                {
                    [var symbol] => GetSymbolMetadata(symbol.Node),

                    _ => new AggregateMetadata(symbols.Select(x => GetSymbolMetadata(x.Node))),
                };

                return;
            }

            // static access
            var metadataProvider = metadataProviderMap.Get(node);
            var function = metadataProvider.FindFunctions(node.Name);
            var result = metadataProvider.QueryTypes(Query.From(node.Name));
            var members = function.Cast<IMetadata>().Concat(result.Types).ToArray();

            node.Reference = members.Length switch
            {
                1 => members[0],
                > 1 => new AggregateMetadata(members),
                _ => null,
            };

            // resolve namespaces for fully-qualified names
            if (node.Reference is null)
            {
                var foundNamespace = rootNamespace.FindNamespace([node.Name]);
                if (foundNamespace is not null)
                    node.Reference = foundNamespace;
            }
        }

        private void VisitNestedMemberAccess(MemberAccessExpression node)
        {
            node.Member!.Accept(this);

            if (node.Member is MemberAccessExpression { Reference: NamespaceMetadata ns })
            {
                var type = ns.FindType(node.Name) as IMetadata;
                if (type is null)
                {
                    diagnostics.UnknownSymbol(node);
                    type = new InvalidMemberMetadata(node.Name);
                }

                node.Reference = type;
                return;
            }

            var returnTypeMetadata = node.Member.ReturnTypeMetadata!;
            var memberMetadata = returnTypeMetadata.GetMember(node.Name);
            if (memberMetadata is null)
            {
                memberMetadata = new InvalidMemberMetadata(node.Name);

                if (!returnTypeMetadata.IsInvalid)
                    diagnostics.UnknownMember(node, returnTypeMetadata);
            }

            node.Reference = memberMetadata;
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

        public override void VisitNewArray(NewArrayExpression node)
        {
            base.VisitNewArray(node);

            node.ReturnTypeMetadata = node.Type.Metadata;
        }

        public override void VisitNewObject(NewObjectExpression node)
        {
            base.VisitNewObject(node);

            var metadata = node.Type.Metadata;
            if (metadata is GenericApplicationMetadata generic)
                metadata = generic.ClosedGeneric;

            node.Metadata = GetConstructorMetadata(node, metadata);
        }

        private ConstructorMetadata GetConstructorMetadata(
            NewObjectExpression node,
            ITypeMetadata? metadata)
        {
            // TODO: get ctor via GetMember?
            if (metadata is not TypeMetadata type || type.IsValueType)
            {
                diagnostics.CantCreateObject(node);

                return ConstructorMetadata.Invalid();
            }

            var parameters = node.Parameters.Select(x => x.ReturnTypeMetadata!).ToList();
            var ctor = type.GetConstructor(parameters);
            if (ctor is null)
            {
                ctor = ConstructorMetadata.Invalid();
                diagnostics.UnknownConstructor(node, parameters);
            }

            return ctor;
        }

        public override void VisitReturn(ReturnStatement node)
        {
            base.VisitReturn(node);

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
                var setterReturnType = ((PropertyDeclaration)setter.Parent!).Metadata!.Type;
                if (!setterReturnType.IsInvalid && !Equals(setterReturnType, expressionType))
                    diagnostics.ReturnTypeMismatch(node, setterReturnType, expressionType);

                return;
            }
        }

        public override void VisitTuple(TupleExpression node)
        {
            base.VisitTuple(node);

            var metadataProvider = metadataProviderMap.Get(node);
            var metadataFactory = new MetadataFactory(builtInTypes, diagnostics, metadataProvider);

            // we can't generate metadata for this tuple in GenerateMetadata
            // because we don't know the types of the expressions yet
            var types = node.Expressions.Select(x => x.ReturnTypeMetadata!);
            var tuple = metadataFactory.CreateTupleMetadata(null, types);
            tuple = metadataProvider.GetOrDefine(tuple);

            node.ReturnTypeMetadata = tuple;
        }

        public override void VisitUnaryExpression(UnaryExpression node)
        {
            base.VisitUnaryExpression(node);

            Debug.Assert(node.Kind != UnaryExpressionKind.Unknown);
            Debug.Assert(node.Operand.ReturnTypeMetadata is not null);

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

            ResolveFunction(node.Expression, node.Type.Metadata);

            if (!node.Expression.ReturnTypeMetadata.IsInvalid &&
                !node.Type.Metadata.IsInvalid &&
                !node.Expression.ReturnTypeMetadata.Equals(node.Type.Metadata))
                diagnostics.TypeMismatch(node.Expression, node.Type.Metadata, node.Expression.ReturnTypeMetadata);
        }

        public override void VisitWhile(While node)
        {
            base.VisitWhile(node);

            if (!Equals(node.Condition.ReturnTypeMetadata, builtInTypes.Bool))
                diagnostics.TypeMismatch(node.Condition, builtInTypes.Bool, node.Condition.ReturnTypeMetadata);
        }
    }
}
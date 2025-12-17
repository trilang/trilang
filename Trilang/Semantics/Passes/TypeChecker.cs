using System.Diagnostics;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using static Trilang.Metadata.TypeMetadata;
using static Trilang.Semantics.Model.BinaryExpressionKind;
using static Trilang.Semantics.Model.UnaryExpressionKind;

namespace Trilang.Semantics.Passes;

internal class TypeChecker : Visitor, ISemanticPass
{
    private SemanticDiagnosticReporter diagnostics = null!;
    private IEnumerable<string> directives = null!;
    private SymbolTableMap symbolTableMap = null!;
    private MetadataProviderMap metadataProviderMap = null!;

    public void Analyze(IEnumerable<SemanticTree> semanticTrees, SemanticPassContext context)
    {
        diagnostics = context.Diagnostics;
        directives = context.Directives;
        symbolTableMap = context.SymbolTableMap!;
        metadataProviderMap = context.TypeProviderMap!;

        foreach (var tree in semanticTrees)
            tree.Accept(this);
    }

    protected override void VisitArrayAccessExit(ArrayAccessExpression node)
    {
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

        if (!Equals(node.Index.ReturnTypeMetadata, I32))
            diagnostics.TypeMismatch(node.Index, I32, node.Index.ReturnTypeMetadata);
    }

    protected override void VisitBinaryExpressionExit(BinaryExpression node)
    {
        Debug.Assert(node.Kind != BinaryExpressionKind.Unknown);
        Debug.Assert(node.Left.ReturnTypeMetadata is not null);
        Debug.Assert(node.Right.ReturnTypeMetadata is not null);

        // TODO: more complex logic
        if (node.Kind is Addition or Subtraction or Multiplication or Division or Modulus &&
            ((Equals(node.Left.ReturnTypeMetadata, I8) && Equals(node.Right.ReturnTypeMetadata, I8)) ||
             (Equals(node.Left.ReturnTypeMetadata, I16) && Equals(node.Right.ReturnTypeMetadata, I16)) ||
             (Equals(node.Left.ReturnTypeMetadata, I32) && Equals(node.Right.ReturnTypeMetadata, I32)) ||
             (Equals(node.Left.ReturnTypeMetadata, I64) && Equals(node.Right.ReturnTypeMetadata, I64)) ||
             (Equals(node.Left.ReturnTypeMetadata, U8) && Equals(node.Right.ReturnTypeMetadata, U8)) ||
             (Equals(node.Left.ReturnTypeMetadata, U16) && Equals(node.Right.ReturnTypeMetadata, U16)) ||
             (Equals(node.Left.ReturnTypeMetadata, U32) && Equals(node.Right.ReturnTypeMetadata, U32)) ||
             (Equals(node.Left.ReturnTypeMetadata, U64) && Equals(node.Right.ReturnTypeMetadata, U64)) ||
             (Equals(node.Left.ReturnTypeMetadata, F32) && Equals(node.Right.ReturnTypeMetadata, F32)) ||
             (Equals(node.Left.ReturnTypeMetadata, F64) && Equals(node.Right.ReturnTypeMetadata, F64))))
        {
            node.ReturnTypeMetadata = node.Left.ReturnTypeMetadata;
        }
        else if (node.Kind is BitwiseAnd or BitwiseOr or BitwiseXor &&
                 ((Equals(node.Left.ReturnTypeMetadata, I8) && Equals(node.Right.ReturnTypeMetadata, I8)) ||
                  (Equals(node.Left.ReturnTypeMetadata, I16) && Equals(node.Right.ReturnTypeMetadata, I16)) ||
                  (Equals(node.Left.ReturnTypeMetadata, I32) && Equals(node.Right.ReturnTypeMetadata, I32)) ||
                  (Equals(node.Left.ReturnTypeMetadata, I64) && Equals(node.Right.ReturnTypeMetadata, I64)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U8) && Equals(node.Right.ReturnTypeMetadata, U8)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U16) && Equals(node.Right.ReturnTypeMetadata, U16)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U32) && Equals(node.Right.ReturnTypeMetadata, U32)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U64) && Equals(node.Right.ReturnTypeMetadata, U64))))
        {
            node.ReturnTypeMetadata = node.Left.ReturnTypeMetadata;
        }
        else if (node.Kind is ConditionalAnd or ConditionalOr &&
                 Equals(node.Left.ReturnTypeMetadata, Bool) && Equals(node.Right.ReturnTypeMetadata, Bool))
        {
            node.ReturnTypeMetadata = Bool;
        }
        else if (node.Kind is Equality or Inequality or
                     LessThan or LessThanOrEqual or
                     GreaterThan or GreaterThanOrEqual &&
                 ((Equals(node.Left.ReturnTypeMetadata, I8) && Equals(node.Right.ReturnTypeMetadata, I8)) ||
                  (Equals(node.Left.ReturnTypeMetadata, I16) && Equals(node.Right.ReturnTypeMetadata, I16)) ||
                  (Equals(node.Left.ReturnTypeMetadata, I32) && Equals(node.Right.ReturnTypeMetadata, I32)) ||
                  (Equals(node.Left.ReturnTypeMetadata, I64) && Equals(node.Right.ReturnTypeMetadata, I64)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U8) && Equals(node.Right.ReturnTypeMetadata, U8)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U16) && Equals(node.Right.ReturnTypeMetadata, U16)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U32) && Equals(node.Right.ReturnTypeMetadata, U32)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U64) && Equals(node.Right.ReturnTypeMetadata, U64))))
        {
            node.ReturnTypeMetadata = Bool;
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
                 (Equals(node.Right.ReturnTypeMetadata, I8) ||
                  Equals(node.Right.ReturnTypeMetadata, I16) ||
                  Equals(node.Right.ReturnTypeMetadata, I32) ||
                  Equals(node.Right.ReturnTypeMetadata, I64) ||
                  Equals(node.Right.ReturnTypeMetadata, U8) ||
                  Equals(node.Right.ReturnTypeMetadata, U16) ||
                  Equals(node.Right.ReturnTypeMetadata, U32) ||
                  Equals(node.Right.ReturnTypeMetadata, U64) ||
                  Equals(node.Right.ReturnTypeMetadata, F32) ||
                  Equals(node.Right.ReturnTypeMetadata, F64)))
        {
            node.ReturnTypeMetadata = node.Right.ReturnTypeMetadata;
        }
        else if (node.Kind is BitwiseAndAssignment or BitwiseOrAssignment or BitwiseXorAssignment &&
                 node.Left is MemberAccessExpression &&
                 (Equals(node.Right.ReturnTypeMetadata, I8) ||
                  Equals(node.Right.ReturnTypeMetadata, I16) ||
                  Equals(node.Right.ReturnTypeMetadata, I32) ||
                  Equals(node.Right.ReturnTypeMetadata, I64) ||
                  Equals(node.Right.ReturnTypeMetadata, U8) ||
                  Equals(node.Right.ReturnTypeMetadata, U16) ||
                  Equals(node.Right.ReturnTypeMetadata, U32) ||
                  Equals(node.Right.ReturnTypeMetadata, U64)))
        {
            node.ReturnTypeMetadata = node.Right.ReturnTypeMetadata;
        }
        else
        {
            node.ReturnTypeMetadata = InvalidType;
            diagnostics.IncompatibleBinaryOperand(node);
        }
    }

    protected override void VisitCallExit(CallExpression node)
    {
        Debug.Assert(node.Member.ReturnTypeMetadata is not null);

        ResolveFunctionGroup(node.Member, node.Parameters.Select(x => x.ReturnTypeMetadata!));

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

                ResolveFunctionGroup(parameter, expected);
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

    public override void VisitExpressionBlock(ExpressionBlock node)
        => Debug.Fail("Expression blocks are not supported");

    public override void VisitIfDirective(IfDirective node)
    {
        if (directives.Contains(node.DirectiveName))
        {
            foreach (var then in node.Then)
                then.Accept(this);
        }
        else
        {
            foreach (var @else in node.Else)
                @else.Accept(this);
        }
    }

    protected override void VisitIfExit(IfStatement node)
    {
        if (!Equals(node.Condition.ReturnTypeMetadata, Bool))
            diagnostics.TypeMismatch(node.Condition, Bool, node.Condition.ReturnTypeMetadata);
    }

    protected override void VisitLiteralExit(LiteralExpression node)
    {
        node.ReturnTypeMetadata = node.Kind switch
        {
            // TODO: other types
            LiteralExpressionKind.Integer => I32,
            LiteralExpressionKind.Float => F64,
            LiteralExpressionKind.Boolean => Bool,
            LiteralExpressionKind.String => TypeMetadata.String,
            LiteralExpressionKind.Char => TypeMetadata.Char,

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

        Debug.Assert(node.ReturnTypeMetadata is not null);
    }

    private void VisitFirstMemberAccess(MemberAccessExpression node)
    {
        var symbolTable = symbolTableMap.Get(node);
        var symbol = symbolTable.GetId(node.Name);
        if (symbol is not null)
        {
            var symbolNode = symbol.Nodes[0];
            if (node.IsThis)
            {
                node.Reference = new ParameterMetadata(
                    null,
                    MemberAccessExpression.This,
                    ((TypeDeclaration)symbolNode).Metadata!);

                return;
            }

            node.Reference = symbolNode switch
            {
                PropertyDeclaration propertyDeclarationNode
                    => propertyDeclarationNode.Metadata,

                VariableDeclaration variableStatementNode
                    => variableStatementNode.Metadata,

                Parameter parameterNode
                    => parameterNode.Metadata,

                FunctionDeclaration functionNode
                    => functionNode.Metadata!.Group,

                MethodDeclaration methodNode
                    => methodNode.Metadata!.Group,

                _ => throw new ArgumentException("Unknown symbol"),
            };

            return;
        }

        // static access
        var typeProvider = metadataProviderMap.Get(node);
        node.Reference = typeProvider.GetType(node.Name);
    }

    private void VisitNestedMemberAccess(MemberAccessExpression node)
    {
        node.Member!.Accept(this);

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

    private void ResolveFunctionGroup(IExpression node, ITypeMetadata expectedType)
    {
        expectedType = expectedType.UnpackAlias()!;

        if (expectedType is not FunctionTypeMetadata functionType)
            return;

        ResolveFunctionGroup(node, functionType.ParameterTypes);
    }

    private void ResolveFunctionGroup(IExpression node, IEnumerable<ITypeMetadata> actualParameters)
    {
        if (node is not MemberAccessExpression member)
            return;

        if (member.Reference is not FunctionGroupMetadata group)
            return;

        if (group.Functions.Count == 1)
        {
            member.Reference = group.Functions[0];
        }
        else
        {
            var candidates = group.Match(actualParameters).ToArray();
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

    protected override void VisitNewArrayExit(NewArrayExpression node)
        => node.ReturnTypeMetadata = node.Type.Metadata;

    protected override void VisitNewObjectExit(NewObjectExpression node)
    {
        if (node.Type.Metadata is not TypeMetadata type || type.IsValueType)
        {
            node.Metadata = ConstructorMetadata.Invalid();
            diagnostics.CantCreateObject(node);
        }
        else
        {
            var parameters = node.Parameters.Select(x => x.ReturnTypeMetadata!).ToList();
            var ctor = type.GetConstructor(parameters);
            if (ctor is null)
            {
                ctor = ConstructorMetadata.Invalid();
                diagnostics.UnknownConstructor(node, parameters);
            }

            node.Metadata = ctor;
        }
    }

    protected override void VisitReturnExit(ReturnStatement node)
    {
        var expressionType = node.Expression?.ReturnTypeMetadata ?? TypeMetadata.Void;
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
            if (!Equals(TypeMetadata.Void, expressionType))
                diagnostics.ReturnTypeMismatch(node, TypeMetadata.Void, expressionType);

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

    protected override void VisitTupleExit(TupleExpression node)
    {
        var typeProvider = metadataProviderMap.Get(node);

        // we can't generate metadata for this tuple in GenerateMetadata
        // because we don't know the types of the expressions yet
        var types = node.Expressions.Select(x => x.ReturnTypeMetadata!);
        var tuple = new TupleMetadata(null, types);
        var existingTuple = typeProvider.GetType(tuple.ToString());
        if (existingTuple is null)
        {
            typeProvider.DefineType(tuple.ToString(), tuple);
            existingTuple = tuple;
        }

        node.ReturnTypeMetadata = existingTuple;
    }

    protected override void VisitTypeNodeExit(TypeRef node)
    {
        if (node.Metadata is not null)
            return;

        var typeProvider = metadataProviderMap.Get(node);

        node.Metadata = node.PopulateMetadata(typeProvider, diagnostics);
    }

    protected override void VisitUnaryExpressionExit(UnaryExpression node)
    {
        Debug.Assert(node.Kind != UnaryExpressionKind.Unknown);
        Debug.Assert(node.Operand.ReturnTypeMetadata is not null);

        // TODO: more complex logic
        if (node.Kind == UnaryMinus &&
            (Equals(node.Operand.ReturnTypeMetadata, I8) ||
             Equals(node.Operand.ReturnTypeMetadata, I16) ||
             Equals(node.Operand.ReturnTypeMetadata, I32) ||
             Equals(node.Operand.ReturnTypeMetadata, I64) ||
             Equals(node.Operand.ReturnTypeMetadata, U8) ||
             Equals(node.Operand.ReturnTypeMetadata, U16) ||
             Equals(node.Operand.ReturnTypeMetadata, U32) ||
             Equals(node.Operand.ReturnTypeMetadata, U64) ||
             Equals(node.Operand.ReturnTypeMetadata, F32) ||
             Equals(node.Operand.ReturnTypeMetadata, F64)))
        {
            node.ReturnTypeMetadata = node.Operand.ReturnTypeMetadata;
        }
        else if (node.Kind == UnaryPlus &&
                 (Equals(node.Operand.ReturnTypeMetadata, I8) ||
                  Equals(node.Operand.ReturnTypeMetadata, I16) ||
                  Equals(node.Operand.ReturnTypeMetadata, I32) ||
                  Equals(node.Operand.ReturnTypeMetadata, I64) ||
                  Equals(node.Operand.ReturnTypeMetadata, U8) ||
                  Equals(node.Operand.ReturnTypeMetadata, U16) ||
                  Equals(node.Operand.ReturnTypeMetadata, U32) ||
                  Equals(node.Operand.ReturnTypeMetadata, U64) ||
                  Equals(node.Operand.ReturnTypeMetadata, F32) ||
                  Equals(node.Operand.ReturnTypeMetadata, F64)))
        {
            node.ReturnTypeMetadata = node.Operand.ReturnTypeMetadata;
        }
        else if (node.Kind == LogicalNot && Equals(node.Operand.ReturnTypeMetadata, Bool))
        {
            node.ReturnTypeMetadata = Bool;
        }
        else if (node.Kind == BitwiseNot &&
                 (Equals(node.Operand.ReturnTypeMetadata, I8) ||
                  Equals(node.Operand.ReturnTypeMetadata, I16) ||
                  Equals(node.Operand.ReturnTypeMetadata, I32) ||
                  Equals(node.Operand.ReturnTypeMetadata, I64) ||
                  Equals(node.Operand.ReturnTypeMetadata, U8) ||
                  Equals(node.Operand.ReturnTypeMetadata, U16) ||
                  Equals(node.Operand.ReturnTypeMetadata, U32) ||
                  Equals(node.Operand.ReturnTypeMetadata, U64)))
        {
            node.ReturnTypeMetadata = node.Operand.ReturnTypeMetadata;
        }
        else
        {
            node.ReturnTypeMetadata = InvalidType;
            diagnostics.IncompatibleUnaryOperand(node);
        }
    }

    protected override void VisitVariableExit(VariableDeclaration node)
    {
        Debug.Assert(node.Expression.ReturnTypeMetadata is not null);
        Debug.Assert(node.Type.Metadata is not null);

        ResolveFunctionGroup(node.Expression, node.Type.Metadata);

        if (!node.Expression.ReturnTypeMetadata.IsInvalid &&
            !node.Type.Metadata.IsInvalid &&
            !node.Expression.ReturnTypeMetadata.Equals(node.Type.Metadata))
            diagnostics.TypeMismatch(node.Expression, node.Type.Metadata, node.Expression.ReturnTypeMetadata);
    }

    protected override void VisitWhileExit(While node)
    {
        if (!Equals(node.Condition.ReturnTypeMetadata, Bool))
            diagnostics.TypeMismatch(node.Condition, Bool, node.Condition.ReturnTypeMetadata);
    }

    public string Name => nameof(TypeChecker);

    public IEnumerable<string> DependsOn => [nameof(MetadataGenerator)];
}
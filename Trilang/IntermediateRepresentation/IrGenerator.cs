using System.Diagnostics;
using Trilang.Metadata;
using Trilang.Parsing.Ast;
using static Trilang.Parsing.Ast.BinaryExpressionKind;

namespace Trilang.IntermediateRepresentation;

public class IrGenerator
{
    private readonly SsaTransformer ssaTransformer;
    private readonly TypeLayoutGenerator layoutGenerator;
    private readonly IrDiscoveryPhase discoveryPhase;

    public IrGenerator()
    {
        ssaTransformer = new SsaTransformer();
        layoutGenerator = new TypeLayoutGenerator();
        discoveryPhase = new IrDiscoveryPhase();
    }

    public IReadOnlyList<IrFunction> Generate(
        IEnumerable<ITypeMetadata> types,
        IReadOnlyList<SyntaxTree> syntaxTrees)
    {
        var typeMetadata = types as ITypeMetadata[] ?? types.ToArray();
        layoutGenerator.Generate(typeMetadata);

        var functions = new List<IrFunction>();
        var functionsToGenerate = discoveryPhase.Discover(typeMetadata, syntaxTrees);

        foreach (var (metadata, body) in functionsToGenerate)
            functions.Add(GenerateFunction(metadata, body));

        return functions;
    }

    private IrFunction GenerateFunction(IFunctionMetadata method, BlockStatementNode body)
    {
        var builder = new IrBuilder();

        var parameterIndex = 0;
        if (!method.IsStatic)
            builder.LoadParameter(MemberAccessExpressionNode.This, method.DeclaringType, parameterIndex++);

        foreach (var parameter in method.Parameters)
            builder.LoadParameter(parameter.Name, parameter.Type, parameterIndex++);

        GenerateBlock(builder, body);

        var code = builder.Build();
        ssaTransformer.Transform(code);

        return IrFunction.FromMethod(method, code);
    }

    private void GenerateStatement(IrBuilder builder, IStatementNode statementNode)
    {
        if (statementNode is BlockStatementNode blockStatementNode)
            GenerateBlock(builder, blockStatementNode);
        else if (statementNode is BreakNode)
            Debug.Fail("The 'break' statement is not supported. It is replaced with 'goto' in lowering.");
        else if (statementNode is ContinueNode)
            Debug.Fail("The 'continue' statement is not supported. It is replaced with 'goto' in lowering.");
        else if (statementNode is ExpressionStatementNode expressionStatementNode)
            GenerateExpression(builder, expressionStatementNode.Expression);
        else if (statementNode is GoToNode goToNode)
            GenerateGoTo(builder, goToNode);
        else if (statementNode is IfStatementNode ifStatementNode)
            GenerateIf(builder, ifStatementNode);
        else if (statementNode is LabelNode labelNode)
            GenerateLabel(builder, labelNode);
        else if (statementNode is ReturnStatementNode returnStatementNode)
            GenerateReturn(builder, returnStatementNode);
        else if (statementNode is VariableDeclarationStatementNode variableDeclarationStatementNode)
            GenerateVariableDeclaration(builder, variableDeclarationStatementNode);
        else if (statementNode is WhileNode)
            Debug.Fail("The 'while' statement is not supported. It is replaced with 'if' in lowering.");
        else
            throw new ArgumentOutOfRangeException(nameof(statementNode));
    }

    private void GenerateBlock(IrBuilder builder, BlockStatementNode node)
    {
        foreach (var statement in node.Statements)
            GenerateStatement(builder, statement);
    }

    private void GenerateGoTo(IrBuilder builder, GoToNode node)
    {
        var label = node.Label;
        var block = builder.FindBlock(label) ??
                    builder.CreateBlock(label);

        builder.Jump(block);
    }

    private void GenerateIf(IrBuilder builder, IfStatementNode node)
    {
        if (node.Then.Statements is not [GoToNode thenGoTo])
            throw new Exception("The 'if' statement must have a 'goto' statement as the 'then' branch.");

        if (node.Else!.Statements is not [GoToNode elseGoTo])
            throw new Exception("The 'if' statement must have a 'goto' statement as the 'else' branch.");

        var condition = GenerateExpression(builder, node.Condition)!.Value;
        condition = builder.Deref(condition, node.Condition.ReturnTypeMetadata!);

        var thenBlock = builder.FindBlock(thenGoTo.Label) ??
                        builder.CreateBlock(thenGoTo.Label);
        var elseBlock = builder.FindBlock(elseGoTo.Label) ??
                        builder.CreateBlock(elseGoTo.Label);

        builder.Branch(condition, thenBlock, elseBlock);
    }

    private void GenerateLabel(IrBuilder builder, LabelNode node)
    {
        var label = node.Name;
        var block = builder.FindBlock(label) ??
                    builder.CreateBlock(node.Name);

        builder.UseBlock(block);
    }

    private void GenerateReturn(IrBuilder builder, ReturnStatementNode node)
    {
        var expression = node.Expression;
        if (expression is null)
        {
            builder.Return();

            return;
        }

        var register = GenerateExpression(builder, expression)!.Value;
        register = builder.Deref(register, expression.ReturnTypeMetadata!);

        builder.Return(register);
    }

    private void GenerateVariableDeclaration(IrBuilder builder, VariableDeclarationStatementNode node)
    {
        var expression = node.Expression;
        var expressionRegister = GenerateExpression(builder, expression);
        expressionRegister = builder.Deref(expressionRegister!.Value, expression.ReturnTypeMetadata!);

        var register = builder.Move(expressionRegister.Value);
        builder.AddDefinition(node.Name, register);
    }

    private Register? GenerateExpression(IrBuilder builder, IExpressionNode node)
        => node switch
        {
            ArrayAccessExpressionNode arrayAccessExpressionNode
                => GenerateArrayAccess(builder, arrayAccessExpressionNode),

            AsExpressionNode asExpressionNode
                => throw new NotImplementedException(),

            BinaryExpressionNode binaryExpressionNode
                => GenerateBinaryExpression(builder, binaryExpressionNode),

            CallExpressionNode callExpressionNode
                => GenerateCall(builder, callExpressionNode),

            CastExpressionNode castExpressionNode
                => throw new NotImplementedException(),

            ExpressionBlockNode expressionBlockNode
                => GenerateExpressionBlock(builder, expressionBlockNode),

            LiteralExpressionNode literalExpressionNode
                => GenerateLiteral(builder, literalExpressionNode),

            MemberAccessExpressionNode memberAccessExpressionNode
                => GenerateMemberAccess(builder, memberAccessExpressionNode),

            NewArrayExpressionNode newArrayExpressionNode
                => GenerateNewArray(builder, newArrayExpressionNode),

            NewObjectExpressionNode newObjectExpressionNode
                => GenerateNewObject(builder, newObjectExpressionNode),

            NullExpressionNode nullExpressionNode
                => GenerateNull(builder, nullExpressionNode),

            TupleExpressionNode tupleExpressionNode
                => throw new NotImplementedException(),

            UnaryExpressionNode unaryExpressionNode
                => GenerateUnaryExpression(builder, unaryExpressionNode),

            _ => throw new ArgumentOutOfRangeException(nameof(node)),
        };

    private Register GenerateArrayAccess(IrBuilder builder, ArrayAccessExpressionNode node)
    {
        var array = GenerateExpression(builder, node.Member)!.Value;
        var index = GenerateExpression(builder, node.Index)!.Value;
        index = builder.Deref(index, node.Index.ReturnTypeMetadata!);

        return builder.GetElementPointer(array, index);
    }

    private Register GenerateBinaryExpression(IrBuilder builder, BinaryExpressionNode node)
    {
        if (node.Kind is AdditionAssignment
            or SubtractionAssignment
            or MultiplicationAssignment
            or DivisionAssignment
            or ModulusAssignment
            or BitwiseAndAssignment
            or BitwiseOrAssignment
            or BitwiseXorAssignment)
            Debug.Fail($"Compound assignment operators ({node.Kind}) should have been lowered to simple assignment on the previous stage.");

        var left = GenerateExpression(builder, node.Left)!.Value;
        var right = GenerateExpression(builder, node.Right)!.Value;
        right = builder.Deref(right, node.Right.ReturnTypeMetadata!);

        if (node.Kind == Assignment)
        {
            if (left.Type is TypePointerMetadata)
            {
                builder.Store(left, right);

                return right;
            }

            var register = builder.Move(left, right);
            var variable = builder.CurrentBlock.FindVariable(left);
            if (variable is not null)
                builder.AddAssignment(variable, register);

            return register;
        }

        left = builder.Deref(left, node.Left.ReturnTypeMetadata!);
        if (node.Kind == Addition)
        {
            return builder.Add(node.ReturnTypeMetadata!, left, right);
        }

        if (node.Kind == Subtraction)
        {
            return builder.Sub(node.ReturnTypeMetadata!, left, right);
        }

        if (node.Kind == Multiplication)
        {
            return builder.Mul(node.ReturnTypeMetadata!, left, right);
        }

        if (node.Kind == Division)
        {
            return builder.Div(node.ReturnTypeMetadata!, left, right);
        }

        if (node.Kind == Modulus)
        {
            return builder.Mod(node.ReturnTypeMetadata!, left, right);
        }

        if (node.Kind == BitwiseAnd)
        {
            return builder.And(node.ReturnTypeMetadata!, left, right);
        }

        if (node.Kind == BitwiseOr)
        {
            return builder.Or(node.ReturnTypeMetadata!, left, right);
        }

        if (node.Kind == BitwiseXor)
        {
            return builder.Xor(node.ReturnTypeMetadata!, left, right);
        }

        if (node.Kind == ConditionalAnd)
        {
            throw new NotImplementedException();
        }

        if (node.Kind == ConditionalOr)
        {
            throw new NotImplementedException();
        }

        if (node.Kind == Equality)
        {
            return builder.Eq(node.ReturnTypeMetadata!, left, right);
        }

        if (node.Kind == Inequality)
        {
            return builder.Ne(node.ReturnTypeMetadata!, left, right);
        }

        if (node.Kind == LessThan)
        {
            return builder.Lt(node.ReturnTypeMetadata!, left, right);
        }

        if (node.Kind == LessThanOrEqual)
        {
            return builder.Le(node.ReturnTypeMetadata!, left, right);
        }

        if (node.Kind == GreaterThan)
        {
            return builder.Gt(node.ReturnTypeMetadata!, left, right);
        }

        if (node.Kind == GreaterThanOrEqual)
        {
            return builder.Ge(node.ReturnTypeMetadata!, left, right);
        }

        throw new Exception("Unknown binary expression kind.");
    }

    private Register GenerateCall(IrBuilder builder, CallExpressionNode node)
    {
        var delegatePointerRegister = GenerateExpression(builder, node.Member)!.Value;
        var isStatic = false;
        if (node.Member is MemberAccessExpressionNode { Reference: IFunctionMetadata function })
            isStatic = function.IsStatic;

        return GenerateCall(builder, delegatePointerRegister, node.Parameters, isStatic);
    }

    private Register GenerateCall(
        IrBuilder builder,
        Register delegatePointerRegister,
        IReadOnlyList<IExpressionNode> parameters,
        bool isStatic)
    {
        var parameterRegisters = new Register[parameters.Count];

        var delegateRegister = builder.Load(delegatePointerRegister);

        for (var i = 0; i < parameters.Count; i++)
        {
            var parameter = parameters[i];
            var register = GenerateExpression(builder, parameter)!.Value;
            register = builder.Deref(register, parameter.ReturnTypeMetadata!);

            parameterRegisters[i] = register;
        }

        return builder.Call(delegateRegister, parameterRegisters, isStatic);
    }

    private Register GenerateExpressionBlock(IrBuilder builder, ExpressionBlockNode node)
    {
        for (var i = 0; i < node.Statements.Count - 1; i++)
            GenerateStatement(builder, node.Statements[i]);

        // `ExpressionBlockNode` is generated by the compiler,
        // and the last statement should be an expression
        var lastExpression = (ExpressionStatementNode)node.Statements[^1];
        var result = GenerateExpression(builder, lastExpression.Expression)!.Value;

        return result;
    }

    private Register GenerateLiteral(IrBuilder builder, LiteralExpressionNode node)
        => builder.LoadConst(node.ReturnTypeMetadata!, node.Value);

    private Register? GenerateMemberAccess(IrBuilder builder, MemberAccessExpressionNode node)
    {
        if (node.IsFirstMember)
        {
            return node.Reference switch
            {
                FunctionMetadata
                    => builder.GetMemberPointer(node.Reference),

                TypeMetadata
                    => null,

                _ => builder.CurrentBlock.FindAssignment(node.Name) ??
                     throw new IrException($"Undefined variable '{node.Name}'.")
            };
        }

        var source = GenerateExpression(builder, node.Member);
        var result = builder.GetMemberPointer(node.Reference!, source);

        return result;
    }

    private Register GenerateNewArray(IrBuilder builder, NewArrayExpressionNode node)
    {
        var size = GenerateExpression(builder, node.Size)!.Value;
        size = builder.Deref(size, node.Size.ReturnTypeMetadata!);

        return builder.ArrayAlloc((TypeArrayMetadata)node.Type.Metadata!, size);
    }

    private Register GenerateNewObject(IrBuilder builder, NewObjectExpressionNode node)
    {
        var memory = builder.Alloc(node.ReturnTypeMetadata!);
        var member = builder.GetMemberPointer(node.Metadata!, memory);

        return GenerateCall(builder, member, node.Parameters, false);
    }

    private Register GenerateNull(IrBuilder builder, NullExpressionNode node)
        => builder.LoadConst(node.ReturnTypeMetadata, null);

    private Register GenerateUnaryExpression(IrBuilder builder, UnaryExpressionNode node)
    {
        var operand = GenerateExpression(builder, node.Operand)!.Value;
        operand = builder.Deref(operand, node.Operand.ReturnTypeMetadata!);

        return node.Kind switch
        {
            UnaryExpressionKind.UnaryMinus
                => builder.Neg(node.ReturnTypeMetadata!, operand),

            UnaryExpressionKind.UnaryPlus
                => operand,

            UnaryExpressionKind.LogicalNot or UnaryExpressionKind.BitwiseNot
                => builder.Not(node.ReturnTypeMetadata!, operand),

            _ => throw new NotImplementedException(),
        };
    }
}
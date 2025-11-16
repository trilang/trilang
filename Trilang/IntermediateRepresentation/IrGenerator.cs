using System.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using static Trilang.Semantics.Model.BinaryExpressionKind;

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
        IEnumerable<SemanticTree> semanticTrees)
    {
        var typeMetadata = types as ITypeMetadata[] ?? types.ToArray();
        layoutGenerator.Generate(typeMetadata);

        var functions = new List<IrFunction>();
        var functionsToGenerate = discoveryPhase.Discover(typeMetadata, semanticTrees);

        foreach (var (metadata, body) in functionsToGenerate)
        {
            var irFunction = metadata switch
            {
                FunctionMetadata function => GenerateFunction(function, body),
                MethodMetadata method => GenerateMethod(method, body),
                ConstructorMetadata ctor => GenerateConstructor(ctor, body),
                _ => throw new ArgumentOutOfRangeException(nameof(metadata)),
            };

            functions.Add(irFunction);
        }

        return functions;
    }

    private IrFunction GenerateFunction(FunctionMetadata function, BlockStatement body)
    {
        var builder = new IrBuilder();

        for (var i = 0; i < function.Parameters.Count; i++)
        {
            var parameter = function.Parameters[i];
            builder.LoadParameter(parameter.Name, parameter.Type, i);
        }

        GenerateBlock(builder, body);

        var code = builder.Build();
        ssaTransformer.Transform(code);

        return IrFunction.FromFunction(function, code);
    }

    private IrFunction GenerateMethod(MethodMetadata function, BlockStatement body)
    {
        var builder = new IrBuilder();

        var parameterIndex = 0;
        if (!function.IsStatic)
            builder.LoadParameter(MemberAccessExpression.This, function.DeclaringType, parameterIndex++);

        foreach (var parameter in function.Parameters)
            builder.LoadParameter(parameter.Name, parameter.Type, parameterIndex++);

        GenerateBlock(builder, body);

        var code = builder.Build();
        ssaTransformer.Transform(code);

        return IrFunction.FromMethod(function, code);
    }

    private IrFunction GenerateConstructor(ConstructorMetadata function, BlockStatement body)
    {
        var builder = new IrBuilder();

        var parameterIndex = 0;
        builder.LoadParameter(MemberAccessExpression.This, function.DeclaringType, parameterIndex++);

        foreach (var parameter in function.Parameters)
            builder.LoadParameter(parameter.Name, parameter.Type, parameterIndex++);

        GenerateBlock(builder, body);

        var code = builder.Build();
        ssaTransformer.Transform(code);

        return IrFunction.FromConstructor(function, code);
    }

    private void GenerateStatement(IrBuilder builder, IStatement statementNode)
    {
        if (statementNode is BlockStatement blockStatementNode)
            GenerateBlock(builder, blockStatementNode);
        else if (statementNode is Break)
            Debug.Fail("The 'break' statement is not supported. It is replaced with 'goto' in lowering.");
        else if (statementNode is Continue)
            Debug.Fail("The 'continue' statement is not supported. It is replaced with 'goto' in lowering.");
        else if (statementNode is ExpressionStatement expressionStatementNode)
            GenerateExpression(builder, expressionStatementNode.Expression);
        else if (statementNode is GoTo goToNode)
            GenerateGoTo(builder, goToNode);
        else if (statementNode is IfStatement ifStatementNode)
            GenerateIf(builder, ifStatementNode);
        else if (statementNode is Label labelNode)
            GenerateLabel(builder, labelNode);
        else if (statementNode is ReturnStatement returnStatementNode)
            GenerateReturn(builder, returnStatementNode);
        else if (statementNode is VariableDeclaration variableDeclarationStatementNode)
            GenerateVariableDeclaration(builder, variableDeclarationStatementNode);
        else if (statementNode is While)
            Debug.Fail("The 'while' statement is not supported. It is replaced with 'if' in lowering.");
        else
            throw new ArgumentOutOfRangeException(nameof(statementNode));
    }

    private void GenerateBlock(IrBuilder builder, BlockStatement node)
    {
        foreach (var statement in node.Statements)
            GenerateStatement(builder, statement);
    }

    private void GenerateGoTo(IrBuilder builder, GoTo node)
    {
        var label = node.Label;
        var block = builder.FindBlock(label) ??
                    builder.CreateBlock(label);

        builder.Jump(block);
    }

    private void GenerateIf(IrBuilder builder, IfStatement node)
    {
        if (node.Then.Statements is not [GoTo thenGoTo])
            throw new Exception("The 'if' statement must have a 'goto' statement as the 'then' branch.");

        if (node.Else!.Statements is not [GoTo elseGoTo])
            throw new Exception("The 'if' statement must have a 'goto' statement as the 'else' branch.");

        var condition = GenerateExpression(builder, node.Condition)!.Value;
        condition = builder.Deref(condition, node.Condition.ReturnTypeMetadata!);

        var thenBlock = builder.FindBlock(thenGoTo.Label) ??
                        builder.CreateBlock(thenGoTo.Label);
        var elseBlock = builder.FindBlock(elseGoTo.Label) ??
                        builder.CreateBlock(elseGoTo.Label);

        builder.Branch(condition, thenBlock, elseBlock);
    }

    private void GenerateLabel(IrBuilder builder, Label node)
    {
        var label = node.Name;
        var block = builder.FindBlock(label) ??
                    builder.CreateBlock(node.Name);

        builder.UseBlock(block);
    }

    private void GenerateReturn(IrBuilder builder, ReturnStatement node)
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

    private void GenerateVariableDeclaration(IrBuilder builder, VariableDeclaration node)
    {
        var expression = node.Expression;
        var expressionRegister = GenerateExpression(builder, expression);
        expressionRegister = builder.Deref(expressionRegister!.Value, expression.ReturnTypeMetadata!);

        var register = builder.Move(expressionRegister.Value);
        builder.AddDefinition(node.Name, register);
    }

    private Register? GenerateExpression(IrBuilder builder, IExpression node)
        => node switch
        {
            ArrayAccessExpression arrayAccessExpressionNode
                => GenerateArrayAccess(builder, arrayAccessExpressionNode),

            BinaryExpression binaryExpressionNode
                => GenerateBinaryExpression(builder, binaryExpressionNode),

            CallExpression callExpressionNode
                => GenerateCall(builder, callExpressionNode),

            CastExpression castExpressionNode
                => GenerateCast(builder, castExpressionNode),

            ExpressionBlock expressionBlockNode
                => GenerateExpressionBlock(builder, expressionBlockNode),

            IsExpression isExpressionNode
                => GenerateIsExpression(builder, isExpressionNode),

            LiteralExpression literalExpressionNode
                => GenerateLiteral(builder, literalExpressionNode),

            MemberAccessExpression memberAccessExpressionNode
                => GenerateMemberAccess(builder, memberAccessExpressionNode),

            NewArrayExpression newArrayExpressionNode
                => GenerateNewArray(builder, newArrayExpressionNode),

            NewObjectExpression newObjectExpressionNode
                => GenerateNewObject(builder, newObjectExpressionNode),

            NullExpression nullExpressionNode
                => GenerateNull(builder, nullExpressionNode),

            TupleExpression tupleExpressionNode
                => GenerateTuple(builder, tupleExpressionNode),

            UnaryExpression unaryExpressionNode
                => GenerateUnaryExpression(builder, unaryExpressionNode),

            _ => throw new ArgumentOutOfRangeException(nameof(node)),
        };

    private Register GenerateArrayAccess(IrBuilder builder, ArrayAccessExpression node)
    {
        var array = GenerateExpression(builder, node.Member)!.Value;
        var index = GenerateExpression(builder, node.Index)!.Value;
        index = builder.Deref(index, node.Index.ReturnTypeMetadata!);

        return builder.GetElementPointer(array, index);
    }

    private Register GenerateBinaryExpression(IrBuilder builder, BinaryExpression node)
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

        throw new IrException("Unknown binary expression kind.");
    }

    private Register GenerateCall(IrBuilder builder, CallExpression node)
    {
        var delegatePointerRegister = GenerateExpression(builder, node.Member)!.Value;

        var isStatic = false;
        var functionType = default(FunctionTypeMetadata);
        if (node.Member is MemberAccessExpression member)
        {
            if (member.Reference is FunctionMetadata function)
            {
                isStatic = true;
                functionType = function.Type;
            }
            else if (member.Reference is MethodMetadata method)
            {
                isStatic = method.IsStatic;
                functionType = method.Type;
            }
            else if (member.Reference is ConstructorMetadata constructor)
            {
                isStatic = false;
                functionType = constructor.Type;
            }
            else if (member.Reference is InterfaceMethodMetadata interfaceMethod)
            {
                functionType = interfaceMethod.Type;
            }
            else if (member.Reference is ParameterMetadata parameter)
            {
                functionType = (FunctionTypeMetadata)parameter.Type;
            }
            else if (member.Reference is VariableMetadata variable)
            {
                functionType = (FunctionTypeMetadata)variable.Type;
            }
            else
            {
                throw new IrException("Unknown member type.");
            }
        }

        return GenerateCall(builder, delegatePointerRegister, node.Parameters, isStatic, functionType!);
    }

    private Register GenerateCall(
        IrBuilder builder,
        Register delegateRegister,
        IReadOnlyList<IExpression> parameters,
        bool isStatic,
        FunctionTypeMetadata functionType)
    {
        delegateRegister = builder.Deref(delegateRegister, functionType);

        var parameterRegisters = new Register[parameters.Count];
        for (var i = 0; i < parameters.Count; i++)
        {
            var parameter = parameters[i];
            var register = GenerateExpression(builder, parameter)!.Value;
            register = builder.Deref(register, parameter.ReturnTypeMetadata!);

            parameterRegisters[i] = register;
        }

        return builder.Call(delegateRegister, parameterRegisters, isStatic);
    }

    private Register GenerateCast(IrBuilder builder, CastExpression node)
    {
        var expression = GenerateExpression(builder, node.Expression)!.Value;
        var result = builder.Cast(expression, node.Type.Metadata!);

        return result;
    }

    private Register GenerateExpressionBlock(IrBuilder builder, ExpressionBlock node)
    {
        for (var i = 0; i < node.Statements.Count - 1; i++)
            GenerateStatement(builder, node.Statements[i]);

        // `ExpressionBlockNode` is generated by the compiler,
        // and the last statement will always be an expression
        var lastExpression = (ExpressionStatement)node.Statements[^1];
        var result = GenerateExpression(builder, lastExpression.Expression)!.Value;

        return result;
    }

    private Register GenerateIsExpression(IrBuilder builder, IsExpression node)
    {
        var expression = GenerateExpression(builder, node.Expression)!.Value;
        var result = builder.Is(expression, node.Type.Metadata!);

        return result;
    }

    private Register GenerateLiteral(IrBuilder builder, LiteralExpression node)
        => builder.LoadConst(node.ReturnTypeMetadata!, node.Value);

    private Register? GenerateMemberAccess(IrBuilder builder, MemberAccessExpression node)
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

    private Register GenerateNewArray(IrBuilder builder, NewArrayExpression node)
    {
        var size = GenerateExpression(builder, node.Size)!.Value;
        size = builder.Deref(size, node.Size.ReturnTypeMetadata!);

        return builder.ArrayAlloc((ArrayMetadata)node.Type.Metadata!, size);
    }

    private Register GenerateNewObject(IrBuilder builder, NewObjectExpression node)
    {
        var constructorMetadata = node.Metadata!;

        var memory = builder.Alloc(node.ReturnTypeMetadata!);
        var member = builder.GetMemberPointer(constructorMetadata, memory);

        GenerateCall(builder, member, node.Parameters, false, constructorMetadata.Type);

        return memory;
    }

    private Register GenerateNull(IrBuilder builder, NullExpression node)
        => builder.LoadConst(node.ReturnTypeMetadata, null);

    private Register GenerateTuple(IrBuilder builder, TupleExpression node)
    {
        throw new NotImplementedException();
    }

    private Register GenerateUnaryExpression(IrBuilder builder, UnaryExpression node)
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

            _ => throw new IrException("Unknown unary expression kind."),
        };
    }
}
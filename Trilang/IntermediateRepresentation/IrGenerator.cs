using System.Diagnostics;
using Trilang.Metadata;
using Trilang.Parsing.Ast;
using static Trilang.Parsing.Ast.BinaryExpressionKind;

namespace Trilang.IntermediateRepresentation;

public class IrGenerator
{
    private readonly SsaTransformer ssaTransformer;

    public IrGenerator()
        => ssaTransformer = new SsaTransformer();

    public IReadOnlyList<IrFunction> Generate(IReadOnlyList<SyntaxTree> syntaxTrees)
    {
        var functions = new List<IrFunction>();
        foreach (var syntaxTree in syntaxTrees)
            functions.AddRange(Generate(syntaxTree));

        return functions;
    }

    private IReadOnlyList<IrFunction> Generate(SyntaxTree tree)
    {
        var functions = new List<IrFunction>();

        foreach (var declaration in tree.Declarations)
            functions.AddRange(GenerateDeclaration(declaration));

        return functions;
    }

    private IReadOnlyList<IrFunction> GenerateDeclaration(IDeclarationNode declaration)
        => declaration switch
        {
            FunctionDeclarationNode functionDeclarationNode
                => [GenerateFunction(functionDeclarationNode)],

            TypeAliasDeclarationNode typeAliasDeclarationNode
                => throw new NotImplementedException(),

            TypeDeclarationNode typeDeclarationNode
                => GenerateFunctionFromType(typeDeclarationNode),

            _ => throw new ArgumentOutOfRangeException(nameof(declaration)),
        };

    private IrFunction GenerateFunction(FunctionDeclarationNode node)
    {
        var builder = new IrBuilder();
        builder.LoadParameters(node.Parameters);
        GenerateBlock(builder, node.Body);

        var code = builder.Build();
        ssaTransformer.Transform(code);

        return IrFunction.FromFunction(node, code);
    }

    private IReadOnlyList<IrFunction> GenerateFunctionFromType(TypeDeclarationNode typeDeclarationNode)
    {
        var functions = new List<IrFunction>();

        foreach (var method in typeDeclarationNode.Methods)
            functions.Add(GenerateMethod(method));

        foreach (var constructor in typeDeclarationNode.Constructors)
            functions.Add(GenerateConstructor(constructor));

        foreach (var property in typeDeclarationNode.Properties)
        {
            // getter/setter aren't null because they are generated on the lowering stage
            functions.Add(GenerateGetter(property.Getter!));
            functions.Add(GenerateSetter(property.Setter!));
        }

        return functions;
    }

    private IrFunction GenerateMethod(MethodDeclarationNode node)
    {
        var builder = new IrBuilder();
        builder.LoadParameters(node.Parameters);
        GenerateBlock(builder, node.Body);

        var code = builder.Build();
        ssaTransformer.Transform(code);

        return IrFunction.FromMethod(node, code);
    }

    private IrFunction GenerateConstructor(ConstructorDeclarationNode node)
    {
        var builder = new IrBuilder();
        builder.LoadParameters(node.Parameters);
        GenerateBlock(builder, node.Body);

        var code = builder.Build();
        ssaTransformer.Transform(code);

        return IrFunction.FromConstructor(node, code);
    }

    private IrFunction GenerateGetter(PropertyGetterNode node)
    {
        var builder = new IrBuilder();
        builder.LoadParameters(node.Parameters);
        GenerateBlock(builder, node.Body!);

        var code = builder.Build();
        ssaTransformer.Transform(code);

        return IrFunction.FromGetter(node, code);
    }

    private IrFunction GenerateSetter(PropertySetterNode node)
    {
        var builder = new IrBuilder();
        builder.LoadParameters(node.Parameters);
        GenerateBlock(builder, node.Body!);

        var code = builder.Build();
        ssaTransformer.Transform(code);

        return IrFunction.FromSetter(node, code);
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

        var condition = GenerateExpression(builder, node.Condition);
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
        var expression = node.Expression is not null
            ? GenerateExpression(builder, node.Expression)
            : (Register?)null;

        builder.Return(expression);
    }

    private void GenerateVariableDeclaration(IrBuilder builder, VariableDeclarationStatementNode node)
    {
        var expression = GenerateExpression(builder, node.Expression);
        var register = builder.Move(expression);
        builder.AddDefinition(node.Name, register);
    }

    private Register GenerateExpression(IrBuilder builder, IExpressionNode node)
        => node switch
        {
            ArrayAccessExpressionNode arrayAccessExpressionNode
                => GenerateArrayAccess(builder, arrayAccessExpressionNode),

            AsExpressionNode asExpressionNode
                => throw new NotImplementedException(),

            BinaryExpressionNode binaryExpressionNode
                => GenerateBinaryExpression(builder, binaryExpressionNode),

            CallExpressionNode callExpressionNode
                => throw new NotImplementedException(),

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
        var array = GenerateExpression(builder, node.Member);
        var index = GenerateExpression(builder, node.Index);

        return builder.ArrayElement(array, index);
    }

    private Register GenerateBinaryExpression(IrBuilder builder, BinaryExpressionNode node)
    {
        var left = GenerateExpression(builder, node.Left);
        var right = GenerateExpression(builder, node.Right);

        if (node.Kind == Addition)
        {
            return builder.Add(left, right);
        }

        if (node.Kind == Subtraction)
        {
            return builder.Sub(left, right);
        }

        if (node.Kind == Multiplication)
        {
            return builder.Mul(left, right);
        }

        if (node.Kind == Division)
        {
            return builder.Div(left, right);
        }

        if (node.Kind == Modulus)
        {
            return builder.Mod(left, right);
        }

        if (node.Kind == BitwiseAnd)
        {
            return builder.And(left, right);
        }

        if (node.Kind == BitwiseOr)
        {
            return builder.Or(left, right);
        }

        if (node.Kind == BitwiseXor)
        {
            return builder.Xor(left, right);
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
            return builder.Eq(left, right);
        }

        if (node.Kind == Inequality)
        {
            return builder.Ne(left, right);
        }

        if (node.Kind == LessThan)
        {
            return builder.Lt(left, right);
        }

        if (node.Kind == LessThanOrEqual)
        {
            return builder.Le(left, right);
        }

        if (node.Kind == GreaterThan)
        {
            return builder.Gt(left, right);
        }

        if (node.Kind == GreaterThanOrEqual)
        {
            return builder.Ge(left, right);
        }

        if (node.Kind == Assignment)
        {
            var register = builder.Move(left, right);
            var variable = builder.CurrentBlock.FindVariable(left);
            if (variable is not null)
                builder.AddAssignment(variable, register);

            return register;
        }

        if (node.Kind == AdditionAssignment)
        {
            var source = builder.Add(left, right);
            var register = builder.Move(left, source);

            var variable = builder.CurrentBlock.FindVariable(left);
            if (variable is not null)
                builder.AddAssignment(variable, register);

            return register;
        }

        if (node.Kind == SubtractionAssignment)
        {
            var source = builder.Sub(left, right);
            var register = builder.Move(left, source);

            var variable = builder.CurrentBlock.FindVariable(left);
            if (variable is not null)
                builder.AddAssignment(variable, register);

            return register;
        }

        if (node.Kind == MultiplicationAssignment)
        {
            var source = builder.Mul(left, right);
            var register = builder.Move(left, source);

            var variable = builder.CurrentBlock.FindVariable(left);
            if (variable is not null)
                builder.AddAssignment(variable, register);

            return register;
        }

        if (node.Kind == DivisionAssignment)
        {
            var source = builder.Div(left, right);
            var register = builder.Move(left, source);

            var variable = builder.CurrentBlock.FindVariable(left);
            if (variable is not null)
                builder.AddAssignment(variable, register);

            return register;
        }

        if (node.Kind == ModulusAssignment)
        {
            var source = builder.Mod(left, right);
            var register = builder.Move(left, source);

            var variable = builder.CurrentBlock.FindVariable(left);
            if (variable is not null)
                builder.AddAssignment(variable, register);

            return register;
        }

        if (node.Kind == BitwiseAndAssignment)
        {
            var source = builder.And(left, right);
            var register = builder.Move(left, source);

            var variable = builder.CurrentBlock.FindVariable(left);
            if (variable is not null)
                builder.AddAssignment(variable, register);

            return register;
        }

        if (node.Kind == BitwiseOrAssignment)
        {
            var source = builder.Or(left, right);
            var register = builder.Move(left, source);

            var variable = builder.CurrentBlock.FindVariable(left);
            if (variable is not null)
                builder.AddAssignment(variable, register);

            return register;
        }

        if (node.Kind == BitwiseXorAssignment)
        {
            var source = builder.Xor(left, right);
            var register = builder.Move(left, source);

            var variable = builder.CurrentBlock.FindVariable(left);
            if (variable is not null)
                builder.AddAssignment(variable, register);

            return register;
        }

        throw new Exception("Unknown binary expression kind.");
    }

    private Register GenerateLiteral(IrBuilder builder, LiteralExpressionNode node)
        => builder.Load(node.Value);

    private Register GenerateMemberAccess(IrBuilder builder, MemberAccessExpressionNode node)
    {
        if (node.Member is null)
        {
            var assignment = builder.CurrentBlock.FindAssignment(node.Name);
            if (assignment is not null)
                return assignment.Value;
        }

        throw new NotImplementedException();
    }

    private Register GenerateNewArray(IrBuilder builder, NewArrayExpressionNode node)
    {
        var size = GenerateExpression(builder, node.Size);

        return builder.NewArray((TypeArrayMetadata)node.Type.Metadata!, size);
    }

    private Register GenerateNewObject(IrBuilder builder, NewObjectExpressionNode node)
    {
        var parameters = new Register[node.Parameters.Count];
        for (var i = 0; i < node.Parameters.Count; i++)
            parameters[i] = GenerateExpression(builder, node.Parameters[i]);

        return builder.NewObject(node.Metadata!, parameters);
    }

    private Register GenerateNull(IrBuilder builder, NullExpressionNode _)
        => builder.Load(null);

    private Register GenerateUnaryExpression(IrBuilder builder, UnaryExpressionNode node)
    {
        var operand = GenerateExpression(builder, node.Operand);

        return node.Kind switch
        {
            UnaryExpressionKind.UnaryMinus
                => builder.Neg(operand),

            UnaryExpressionKind.UnaryPlus
                => operand,

            UnaryExpressionKind.LogicalNot or UnaryExpressionKind.BitwiseNot
                => builder.Not(operand),

            _ => throw new NotImplementedException(),
        };
    }
}
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using static Trilang.Parsing.Ast.BinaryExpressionKind;

namespace Trilang.IntermediateRepresentation;

internal class IrGenerator : Visitor
{
    private readonly Dictionary<ISyntaxNode, Register> registers;
    private readonly HashSet<IrFunction> functions;
    private IrBuilder builder;

    public IrGenerator()
    {
        registers = [];
        functions = [];
        builder = new IrBuilder();
    }

    private void LoadParameters(IEnumerable<ParameterNode> parameters)
    {
        foreach (var (index, parameter) in parameters.Index())
        {
            var register = builder.LoadParameter(index);
            registers[parameter] = register;
        }
    }

    protected override void VisitFunctionEnter(FunctionDeclarationNode node)
    {
        builder = new IrBuilder();
        LoadParameters(node.Parameters);
    }

    protected override void VisitFunctionExit(FunctionDeclarationNode node)
    {
        registers.Clear();

        var function = IrFunction.FromFunction(node, builder.Build());
        functions.Add(function);
    }

    protected override void VisitMethodEnter(MethodDeclarationNode node)
    {
        builder = new IrBuilder();
        LoadParameters(node.Parameters);
    }

    protected override void VisitMethodExit(MethodDeclarationNode node)
    {
        registers.Clear();

        var function = IrFunction.FromMethod(node, builder.Build());
        functions.Add(function);
    }

    protected override void VisitConstructorEnter(ConstructorDeclarationNode node)
    {
        builder = new IrBuilder();
        LoadParameters(node.Parameters);
    }

    protected override void VisitConstructorExit(ConstructorDeclarationNode node)
    {
        registers.Clear();

        var function = IrFunction.FromConstructor(node, builder.Build());
        functions.Add(function);
    }

    protected override void VisitSetterEnter(PropertySetterNode node)
    {
        builder = new IrBuilder();
        LoadParameters(node.Parameters);
        // TODO: add field/value
    }

    protected override void VisitSetterExit(PropertySetterNode node)
    {
        registers.Clear();

        var function = IrFunction.FromSetter(node, builder.Build());
        functions.Add(function);
    }

    protected override void VisitGetterEnter(PropertyGetterNode node)
    {
        builder = new IrBuilder();
        LoadParameters(node.Parameters);
        // TODO: add field/value
    }

    protected override void VisitGetterExit(PropertyGetterNode node)
    {
        registers.Clear();

        var function = IrFunction.FromGetter(node, builder.Build());
        functions.Add(function);
    }

    // -----

    protected override void VisitArrayAccessExit(ArrayAccessExpressionNode node)
    {
        if (!registers.TryGetValue(node.Member, out var arrayRegister))
            throw new Exception("Array not found.");

        if (!registers.TryGetValue(node.Index, out var indexRegister))
            throw new Exception("Index not found.");

        var register = builder.ArrayElement(arrayRegister, indexRegister);
        registers[node] = register;
    }

    protected override void VisitBinaryExpressionExit(BinaryExpressionNode node)
    {
        if (!registers.TryGetValue(node.Left, out var left))
            throw new Exception("Left operand not found.");

        if (!registers.TryGetValue(node.Right, out var right))
            throw new Exception("Right operand not found.");

        var register = node.Kind switch
        {
            Addition => builder.Add(left, right),
            Subtraction => builder.Sub(left, right),
            Multiplication => builder.Mul(left, right),
            Division => builder.Div(left, right),
            Modulus => builder.Mod(left, right),

            BitwiseAnd => builder.And(left, right),
            BitwiseOr => builder.Or(left, right),
            BitwiseXor => builder.Xor(left, right),

            _ => throw new Exception("Unknown binary expression kind."),
        };

        registers[node] = register;
    }

    protected override void VisitCallExit(CallExpressionNode node)
    {
        throw new NotImplementedException();
    }

    protected override void VisitLiteralExit(LiteralExpressionNode node)
    {
        var register = builder.Load(node.Value);
        registers[node] = register;
    }

    protected override void VisitNewArrayExit(NewArrayExpressionNode node)
    {
        if (!registers.TryGetValue(node.Size, out var sizeRegister))
            throw new Exception("Size not found.");

        var register = builder.NewArray((TypeArrayMetadata)node.Type.Metadata!, sizeRegister);
        registers[node] = register;
    }

    protected override void VisitNewObjectExit(NewObjectExpressionNode node)
    {
        var parameters = node.Parameters
            .Select(p => registers[p])
            .ToArray();

        var register = builder.NewObject(node.Metadata!, parameters);
        registers[node] = register;
    }

    protected override void VisitNullExit(NullExpressionNode node)
    {
        var register = builder.Load(null);
        registers[node] = register;
    }

    protected override void VisitMemberAccessExit(MemberAccessExpressionNode node)
    {
        // TODO:
        if (node.Member is null)
        {
            var register = node.Reference switch
            {
                VariableDeclarationStatementNode variableStatementNode
                    => registers[variableStatementNode],

                ParameterNode parameterNode
                    => registers[parameterNode],

                FunctionDeclarationNode functionNode
                    => throw new NotImplementedException(),

                TypeDeclarationNode typeDeclarationNode
                    => throw new NotImplementedException(),

                ITypeMetadata type
                    => throw new NotImplementedException(),

                _ => throw new Exception(),
            };

            registers[node] = register;
        }
        else
        {
            throw new NotImplementedException();
        }
    }

    protected override void VisitReturnExit(ReturnStatementNode node)
    {
        var register = default(Register);
        if (node.Expression is not null && !registers.TryGetValue(node.Expression, out register))
            throw new Exception("Expression not found.");

        builder.Return(register);
    }

    protected override void VisitVariableExit(VariableDeclarationStatementNode node)
    {
        if (!registers.TryGetValue(node.Expression, out var expRegister))
            throw new Exception("Expression not found.");

        registers[node] = expRegister;
    }

    protected override void VisitUnaryExpressionExit(UnaryExpressionNode node)
    {
        if (!registers.TryGetValue(node.Operand, out var operand))
            throw new Exception("Operand not found.");

        var register = node.Kind switch
        {
            UnaryExpressionKind.UnaryMinus
                => builder.Neg(operand),

            UnaryExpressionKind.LogicalNot or UnaryExpressionKind.BitwiseNot
                => builder.Not(operand),

            _ => throw new NotImplementedException(),
        };
        registers[node] = register;
    }

    public IReadOnlyCollection<IrFunction> Functions
        => functions;
}
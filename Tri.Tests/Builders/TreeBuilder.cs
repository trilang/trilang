using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Symbols;

namespace Tri.Tests.Builders;

internal sealed class TreeBuilder : ISyntaxTreeBuilder
{
    private readonly List<FunctionDeclarationNode> functions;
    private readonly SymbolTable symbolTable;

    public TreeBuilder()
    {
        functions = [];
        symbolTable = new SymbolTable();
    }

    public ISyntaxTreeBuilder DefineFunction(string name, Action<IFunctionBuilder> action)
    {
        var builder = new FunctionBuilder(symbolTable.CreateChild(), name);

        action(builder);

        var function = builder.Build();
        functions.Add(function);

        if (!symbolTable.TryAddFunction(new FunctionSymbol(function)))
            throw new Exception();

        function.SymbolTable = symbolTable;

        return this;
    }

    public SyntaxTree Build()
        => new SyntaxTree(functions) { SymbolTable = symbolTable };

    private sealed class FunctionBuilder : IFunctionBuilder
    {
        private readonly SymbolTable symbolTable;

        private readonly string functionName;
        private readonly List<FunctionParameterNode> parameters;
        private string returnType;
        private BlockStatementNode? body;

        public FunctionBuilder(SymbolTable symbolTable, string functionName)
        {
            this.symbolTable = symbolTable;
            this.functionName = functionName;
            parameters = [];
            returnType = "void";
        }

        public IFunctionBuilder DefineParameter(string name, string type)
        {
            var parameter = new FunctionParameterNode(name, type) { SymbolTable = symbolTable };
            parameters.Add(parameter);

            if (!symbolTable.TryAddVariable(new VariableSymbol(parameter)))
                throw new Exception();

            parameter.SymbolTable = symbolTable;

            return this;
        }

        public IFunctionBuilder ReturnType(string type)
        {
            returnType = type;

            return this;
        }

        public IFunctionBuilder DefineBody(Action<IBlockBuilder> action)
        {
            var builder = new BlockBuilder(symbolTable);

            action(builder);
            body = builder.Build();

            return this;
        }

        public FunctionDeclarationNode Build()
            => FunctionDeclarationNode.Create(
                functionName,
                parameters,
                returnType,
                body ?? throw new ArgumentNullException());
    }

    private sealed class BlockBuilder : IBlockBuilder
    {
        private readonly SymbolTable symbolTable;
        private readonly List<IStatementNode> statements;

        public BlockBuilder(SymbolTable symbolTable)
        {
            this.symbolTable = symbolTable;
            statements = [];
        }

        public IBlockBuilder DefineVariable(string name, string type, Action<IExpressionBuilder> action)
        {
            var builder = new ExpressionBuilder(symbolTable);
            action(builder);

            var variable = new VariableDeclarationStatementNode(name, type, builder.Build())
            {
                SymbolTable = symbolTable
            };
            statements.Add(variable);

            if (!symbolTable.TryAddVariable(new VariableSymbol(variable)))
                throw new Exception();

            return this;
        }

        public IBlockBuilder Return(Action<IExpressionBuilder> action)
        {
            var builder = new ExpressionBuilder(symbolTable);
            action(builder);

            var returnNode = new ReturnStatementNode(builder.Build()) { SymbolTable = symbolTable };
            statements.Add(returnNode);

            return this;
        }

        public IBlockBuilder If(
            Action<IExpressionBuilder> condition,
            Action<IBlockBuilder> then,
            Action<IBlockBuilder>? @else = null)
        {
            var conditionBuilder = new ExpressionBuilder(symbolTable);
            condition(conditionBuilder);

            var thenBuilder = new BlockBuilder(symbolTable.CreateChild());
            then(thenBuilder);

            BlockBuilder? elseBuilder = null;
            if (@else is not null)
            {
                elseBuilder = new BlockBuilder(symbolTable.CreateChild());
                @else(elseBuilder);
            }

            var ifStatement = new IfStatementNode(
                conditionBuilder.Build(),
                thenBuilder.Build(),
                elseBuilder?.Build())
            {
                SymbolTable = symbolTable
            };
            statements.Add(ifStatement);

            return this;
        }

        public BlockStatementNode Build()
            => new BlockStatementNode(statements) { SymbolTable = symbolTable };
    }

    private sealed class ExpressionBuilder : IExpressionBuilder
    {
        private readonly SymbolTable symbolTable;
        private readonly Stack<IExpressionNode> stack;

        public ExpressionBuilder(SymbolTable symbolTable)
        {
            this.symbolTable = symbolTable;
            stack = [];
        }

        public IExpressionBuilder Number(int number)
        {
            var literal = LiteralExpressionNode.Number(number);
            literal.SymbolTable = symbolTable;

            stack.Push(literal);

            return this;
        }

        public IExpressionBuilder True()
        {
            var literal = LiteralExpressionNode.True();
            literal.SymbolTable = symbolTable;

            stack.Push(literal);

            return this;
        }

        public IExpressionBuilder False()
        {
            var literal = LiteralExpressionNode.False();
            literal.SymbolTable = symbolTable;

            stack.Push(literal);

            return this;
        }

        public IExpressionBuilder Char(char c)
        {
            var literal = LiteralExpressionNode.Char(c);
            literal.SymbolTable = symbolTable;

            stack.Push(literal);

            return this;
        }

        public IExpressionBuilder String(string str)
        {
            var literal = LiteralExpressionNode.String(str);
            literal.SymbolTable = symbolTable;

            stack.Push(literal);

            return this;
        }

        public IExpressionBuilder Variable(string name)
        {
            var variable = new VariableExpressionNode(name) { SymbolTable = symbolTable };
            stack.Push(variable);

            return this;
        }

        private IExpressionBuilder Unary(UnaryExpressionKind kind)
        {
            var plus = new UnaryExpressionNode(kind, stack.Pop()) { SymbolTable = symbolTable };
            stack.Push(plus);

            return this;
        }

        public IExpressionBuilder UnaryPlus()
            => Unary(UnaryExpressionKind.UnaryPlus);

        public IExpressionBuilder UnaryMinus()
            => Unary(UnaryExpressionKind.UnaryMinus);

        public IExpressionBuilder LogicalNot()
            => Unary(UnaryExpressionKind.LogicalNot);

        private IExpressionBuilder BinaryExpression(BinaryExpressionKind kind)
        {
            var right = stack.Pop();
            var left = stack.Pop();
            var exp = new BinaryExpressionNode(kind, left, right) { SymbolTable = symbolTable };
            stack.Push(exp);

            return this;
        }

        public IExpressionBuilder Add()
            => BinaryExpression(BinaryExpressionKind.Addition);

        public IExpressionBuilder Sub()
            => BinaryExpression(BinaryExpressionKind.Subtraction);

        public IExpressionBuilder Mul()
            => BinaryExpression(BinaryExpressionKind.Multiplication);

        public IExpressionBuilder Div()
            => BinaryExpression(BinaryExpressionKind.Division);

        public IExpressionBuilder Call(string name)
        {
            var parameters = new IExpressionNode[stack.Count];
            for (var i = stack.Count - 1; i >= 0; i--)
                parameters[i] = stack.Pop();

            var call = new CallExpressionNode(name, parameters) { SymbolTable = symbolTable };
            stack.Push(call);

            return this;
        }

        public IExpressionNode Build()
        {
            if (stack.Count != 1)
                throw new Exception();

            return stack.Pop();
        }
    }
}
using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Semantics;
using Trilang.Symbols;

namespace Tri.Tests.Builders;

internal sealed class TreeBuilder : ISyntaxTreeBuilder
{
    private readonly List<IDeclarationNode> declaration;
    private readonly ISymbolTable symbolTable;

    public TreeBuilder()
    {
        declaration = [];
        symbolTable = new RootSymbolTable();
    }

    public ISyntaxTreeBuilder DefineFunction(string name, Action<IFunctionBuilder> action)
    {
        var builder = new FunctionBuilder(symbolTable.CreateChild(), name);

        action(builder);

        var function = builder.Build();
        declaration.Add(function);

        if (!symbolTable.TryAddFunction(new FunctionSymbol(function)))
            throw new Exception();

        function.SymbolTable = symbolTable;

        return this;
    }

    public ISyntaxTreeBuilder DefineType(string name, Action<ITypeBuilder> action)
    {
        var builder = new TypeBuilder(symbolTable, name);
        action(builder);

        var type = builder.Build();
        declaration.Add(type);

        if (!symbolTable.TryAddType(TypeSymbol.Type(name, type)))
            throw new Exception();

        type.SymbolTable = symbolTable;

        return this;
    }

    public SyntaxTree Build()
        => new SyntaxTree(declaration) { SymbolTable = symbolTable };

    private sealed class FunctionBuilder : IFunctionBuilder
    {
        private readonly ISymbolTable symbolTable;

        private readonly string functionName;
        private readonly List<ParameterNode> parameters;
        private TypeNode returnType;
        private BlockStatementNode? body;

        public FunctionBuilder(ISymbolTable symbolTable, string functionName)
        {
            this.symbolTable = symbolTable;
            this.functionName = functionName;
            parameters = [];
            returnType = TypeNode.Create("void");
        }

        public IFunctionBuilder DefineParameter(string name, string type)
            => DefineParameter(name, TypeNode.Create(type));

        public IFunctionBuilder DefineParameter(string name, TypeNode type)
        {
            var parameter = new ParameterNode(name, type) { SymbolTable = symbolTable };
            parameters.Add(parameter);

            if (!symbolTable.TryAddVariable(new VariableSymbol(parameter)))
                throw new Exception();

            parameter.SymbolTable = symbolTable;

            return this;
        }

        public IFunctionBuilder ReturnType(string type)
        {
            returnType = TypeNode.Create(type);

            return this;
        }

        public IFunctionBuilder Body(Action<IBlockBuilder> action)
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

    private sealed class TypeBuilder : ITypeBuilder
    {
        private readonly ISymbolTable symbolTable;
        private readonly string typeName;
        private readonly List<FieldDeclarationNode> fields;
        private readonly List<MethodDeclarationNode> methods;
        private AccessModifier accessModifier;

        public TypeBuilder(ISymbolTable symbolTable, string typeName)
        {
            this.symbolTable = symbolTable;
            this.typeName = typeName;
            fields = [];
            methods = [];
            accessModifier = Trilang.Parsing.Ast.AccessModifier.Public;
        }

        public ITypeBuilder AccessModifier(AccessModifier modifier)
        {
            this.accessModifier = modifier;

            return this;
        }

        public ITypeBuilder DefineField(string name, string type, Action<IFieldBuilder>? action = null)
        {
            var builder = new FieldBuilder(name, type);
            action?.Invoke(builder);

            var field = builder.Build();
            fields.Add(field);

            field.SymbolTable = symbolTable;

            return this;
        }

        public ITypeBuilder DefineMethod(string name, Action<IMethodBuilder> action)
        {
            var builder = new MethodBuilder(symbolTable.CreateChild(), name);

            action(builder);

            var method = builder.Build();
            methods.Add(method);

            method.SymbolTable = symbolTable;

            return this;
        }

        public TypeDeclarationNode Build()
            => new TypeDeclarationNode(accessModifier, typeName, fields, methods);
    }

    private sealed class FieldBuilder : IFieldBuilder
    {
        private readonly string name;
        private readonly string type;
        private AccessModifier accessModifier;

        public FieldBuilder(string name, string type)
        {
            this.name = name;
            this.type = type;
            accessModifier = Trilang.Parsing.Ast.AccessModifier.Public;
        }

        public IFieldBuilder AccessModifier(AccessModifier modifier)
        {
            this.accessModifier = modifier;

            return this;
        }

        public FieldDeclarationNode Build()
            => new FieldDeclarationNode(accessModifier, name, TypeNode.Create(type));
    }

    private sealed class MethodBuilder : IMethodBuilder
    {
        private readonly ISymbolTable symbolTable;

        private readonly string functionName;
        private readonly List<ParameterNode> parameters;
        private AccessModifier accessModifier;
        private TypeNode returnType;
        private BlockStatementNode? body;

        public MethodBuilder(ISymbolTable symbolTable, string functionName)
        {
            this.symbolTable = symbolTable;
            this.functionName = functionName;
            accessModifier = Trilang.Parsing.Ast.AccessModifier.Public;
            parameters = [];
            returnType = TypeNode.Create("void");
        }

        public IMethodBuilder AccessModifier(AccessModifier modifier)
        {
            this.accessModifier = modifier;

            return this;
        }

        public IMethodBuilder DefineParameter(string name, string type)
            => DefineParameter(name, TypeNode.Create(type));

        public IMethodBuilder DefineParameter(string name, TypeNode type)
        {
            var parameter = new ParameterNode(name, type) { SymbolTable = symbolTable };
            parameters.Add(parameter);

            if (!symbolTable.TryAddVariable(new VariableSymbol(parameter)))
                throw new Exception();

            parameter.SymbolTable = symbolTable;

            return this;
        }

        public IMethodBuilder ReturnType(string type)
        {
            returnType = TypeNode.Create(type);

            return this;
        }

        public IMethodBuilder Body(Action<IBlockBuilder>? action = null)
        {
            var builder = new BlockBuilder(symbolTable);

            action?.Invoke(builder);
            body = builder.Build();

            return this;
        }

        public MethodDeclarationNode Build()
            => new MethodDeclarationNode(
                accessModifier,
                functionName,
                parameters,
                returnType,
                body ?? throw new ArgumentNullException());
    }

    private sealed class BlockBuilder : IBlockBuilder
    {
        private readonly ISymbolTable symbolTable;
        private readonly List<IStatementNode> statements;

        public BlockBuilder(ISymbolTable symbolTable)
        {
            this.symbolTable = symbolTable;
            statements = [];
        }

        public IBlockBuilder DefineVariable(string name, string type, Action<IExpressionBuilder> action)
        {
            var builder = new ExpressionBuilder(symbolTable);
            action(builder);

            var variable = new VariableDeclarationStatementNode(name, TypeNode.Create(type), builder.Build())
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

        public IBlockBuilder Statement(Action<IExpressionBuilder> action)
        {
            var builder = new ExpressionBuilder(symbolTable);
            action(builder);

            var statement = new ExpressionStatementNode(builder.Build()) { SymbolTable = symbolTable };
            statements.Add(statement);

            return this;
        }

        public IBlockBuilder Block(Action<IBlockBuilder> action)
        {
            var builder = new BlockBuilder(symbolTable.CreateChild());
            action(builder);

            var block = builder.Build();
            statements.Add(block);

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

        public IBlockBuilder While(Action<IExpressionBuilder> condition, Action<IBlockBuilder> body)
        {
            var conditionBuilder = new ExpressionBuilder(symbolTable);
            condition(conditionBuilder);

            var bodyBuilder = new BlockBuilder(symbolTable.CreateChild());
            body(bodyBuilder);

            var whileStatement = new WhileNode(conditionBuilder.Build(), bodyBuilder.Build())
            {
                SymbolTable = symbolTable
            };
            statements.Add(whileStatement);

            return this;
        }

        public BlockStatementNode Build()
            => new BlockStatementNode(statements) { SymbolTable = symbolTable };
    }

    private sealed class ExpressionBuilder : IExpressionBuilder
    {
        private readonly ISymbolTable symbolTable;
        private readonly Stack<IExpressionNode> stack;

        public ExpressionBuilder(ISymbolTable symbolTable)
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
            var variable = new MemberAccessExpressionNode(name) { SymbolTable = symbolTable };
            stack.Push(variable);

            return this;
        }

        public IExpressionBuilder Unary(UnaryExpressionKind kind)
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

        public IExpressionBuilder BinaryExpression(BinaryExpressionKind kind)
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

        public IExpressionBuilder Assign()
            => BinaryExpression(BinaryExpressionKind.Assignment);

        public IExpressionBuilder AddAssign()
            => BinaryExpression(BinaryExpressionKind.AdditionAssignment);

        public IExpressionBuilder SubAssign()
            => BinaryExpression(BinaryExpressionKind.SubtractionAssignment);

        public IExpressionBuilder MulAssign()
            => BinaryExpression(BinaryExpressionKind.MultiplicationAssignment);

        public IExpressionBuilder DivAssign()
            => BinaryExpression(BinaryExpressionKind.DivisionAssignment);

        public IExpressionBuilder ModAssign()
            => BinaryExpression(BinaryExpressionKind.ModulusAssignment);

        public IExpressionBuilder AndAssign()
            => BinaryExpression(BinaryExpressionKind.BitwiseAndAssignment);

        public IExpressionBuilder OrAssign()
            => BinaryExpression(BinaryExpressionKind.BitwiseOrAssignment);

        public IExpressionBuilder XorAssign()
            => BinaryExpression(BinaryExpressionKind.BitwiseXorAssignment);

        public IExpressionBuilder Call(string name)
        {
            var parameters = new IExpressionNode[stack.Count];
            for (var i = stack.Count - 1; i >= 0; i--)
                parameters[i] = stack.Pop();

            var call = new CallExpressionNode(new MemberAccessExpressionNode(name), parameters)
            {
                SymbolTable = symbolTable
            };
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
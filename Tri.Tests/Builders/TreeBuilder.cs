using Trilang.Parsing.Ast;
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

    public ISyntaxTreeBuilder DefineType(string name, Action<ITypeBuilder>? action = null)
    {
        var builder = new TypeBuilder(symbolTable, name);
        action?.Invoke(builder);

        var type = builder.Build();
        declaration.Add(type);

        if (!symbolTable.TryAddType(TypeSymbol.Type(name, type)))
            throw new Exception();

        type.SymbolTable = symbolTable;

        return this;
    }

    public ISyntaxTreeBuilder DefineAliasType(string name, TypeNode aliasType)
    {
        var type = new TypeAliasDeclarationNode(AccessModifier.Public, name, aliasType);

        declaration.Add(type);

        if (!symbolTable.TryAddType(TypeSymbol.Alias(name, type)))
            throw new Exception();

        if (aliasType.IsArray && !symbolTable.TryAddType(TypeSymbol.Array(aliasType.Name)))
            throw new Exception();

        type.SymbolTable = symbolTable;

        return this;
    }

    public ISyntaxTreeBuilder DefineFunctionType(string name, Action<IFunctionTypeBuilder> action)
    {
        var builder = new FunctionTypeBuilder(symbolTable, name);
        action(builder);

        var functionType = builder.Build();
        declaration.Add(functionType);

        if (!symbolTable.TryAddType(TypeSymbol.Function(name, functionType)))
            throw new Exception();

        functionType.SymbolTable = symbolTable;

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
            returnType = new TypeNode("void");
        }

        public IFunctionBuilder DefineParameter(string name, string type)
            => DefineParameter(name, new TypeNode(type));

        public IFunctionBuilder DefineParameter(string name, TypeNode type)
        {
            var parameter = new ParameterNode(name, type) { SymbolTable = symbolTable };
            parameters.Add(parameter);

            if (!symbolTable.TryAddVariable(new VariableSymbol(parameter)))
                throw new Exception();

            if (type.IsArray && !symbolTable.TryAddType(TypeSymbol.Array(type.Name)))
                throw new Exception();

            parameter.SymbolTable = symbolTable;

            return this;
        }

        public IFunctionBuilder ReturnType(string type)
        {
            returnType = new TypeNode(type);

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
        private readonly List<ConstructorDeclarationNode> constructors;
        private readonly List<MethodDeclarationNode> methods;
        private AccessModifier accessModifier;

        public TypeBuilder(ISymbolTable symbolTable, string typeName)
        {
            this.symbolTable = symbolTable;
            this.typeName = typeName;
            fields = [];
            constructors = [];
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

        public ITypeBuilder DefineConstructor(Action<IConstructorBuilder> action)
        {
            var builder = new ConstructorBuilder(symbolTable.CreateChild());

            action(builder);

            var constructor = builder.Build();
            constructors.Add(constructor);

            constructor.SymbolTable = symbolTable;

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
            => new TypeDeclarationNode(accessModifier, typeName, fields, constructors, methods);
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
            => new FieldDeclarationNode(accessModifier, name, new TypeNode(type));
    }

    private sealed class ConstructorBuilder : IConstructorBuilder
    {
        private readonly ISymbolTable symbolTable;

        private readonly List<ParameterNode> parameters;
        private AccessModifier accessModifier;
        private BlockStatementNode? body;

        public ConstructorBuilder(ISymbolTable symbolTable)
        {
            this.symbolTable = symbolTable;
            accessModifier = Trilang.Parsing.Ast.AccessModifier.Public;
            parameters = [];
        }

        public IConstructorBuilder AccessModifier(AccessModifier modifier)
        {
            accessModifier = modifier;

            return this;
        }

        public IConstructorBuilder DefineParameter(string name, string type)
            => DefineParameter(name, new TypeNode(type));

        public IConstructorBuilder DefineParameter(string name, TypeNode type)
        {
            var parameter = new ParameterNode(name, type) { SymbolTable = symbolTable };
            parameters.Add(parameter);

            if (!symbolTable.TryAddVariable(new VariableSymbol(parameter)))
                throw new Exception();

            parameter.SymbolTable = symbolTable;

            return this;
        }

        public IConstructorBuilder Body(Action<IBlockBuilder>? action = null)
        {
            var builder = new BlockBuilder(symbolTable);

            action?.Invoke(builder);
            body = builder.Build();

            return this;
        }

        public ConstructorDeclarationNode Build()
            => new ConstructorDeclarationNode(
                accessModifier,
                parameters,
                body ?? throw new ArgumentNullException());
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
            returnType = new TypeNode("void");
        }

        public IMethodBuilder AccessModifier(AccessModifier modifier)
        {
            this.accessModifier = modifier;

            return this;
        }

        public IMethodBuilder DefineParameter(string name, string type)
            => DefineParameter(name, new TypeNode(type));

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
            returnType = new TypeNode(type);

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

            var variable = new VariableDeclarationStatementNode(name, new TypeNode(type), builder.Build())
            {
                SymbolTable = symbolTable
            };
            statements.Add(variable);

            if (!symbolTable.TryAddVariable(new VariableSymbol(variable)))
                throw new Exception();

            return this;
        }

        public IBlockBuilder Return(Action<IExpressionBuilder>? action = null)
        {
            var exp = default(IExpressionNode);
            if (action is not null)
            {
                var builder = new ExpressionBuilder(symbolTable);
                action(builder);
                exp = builder.Build();
            }

            var returnNode = new ReturnStatementNode(exp) { SymbolTable = symbolTable };
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

    private sealed class FunctionTypeBuilder : IFunctionTypeBuilder
    {
        private readonly ISymbolTable symbolTable;
        private readonly string name;
        private readonly List<TypeNode> parameterTypes;
        private AccessModifier accessModifier;
        private TypeNode returnType;

        public FunctionTypeBuilder(ISymbolTable symbolTable, string name)
        {
            this.symbolTable = symbolTable;
            this.name = name;
            parameterTypes = [];
            accessModifier = Trilang.Parsing.Ast.AccessModifier.Public;
            returnType = new TypeNode("void");
        }

        public IFunctionTypeBuilder AccessModifier(AccessModifier modifier)
        {
            accessModifier = modifier;

            return this;
        }

        public IFunctionTypeBuilder DefineParameter(string type)
        {
            parameterTypes.Add(new TypeNode(type));

            return this;
        }

        public IFunctionTypeBuilder ReturnType(string type)
        {
            returnType = new TypeNode(type);

            return this;
        }

        public FunctionTypeDeclarationNode Build()
            => new FunctionTypeDeclarationNode(accessModifier, name, parameterTypes, returnType);
    }
}
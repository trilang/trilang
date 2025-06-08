using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

internal sealed class TreeBuilder : ISyntaxTreeBuilder
{
    private readonly List<IDeclarationNode> declaration;

    public TreeBuilder()
        => declaration = [];

    public ISyntaxTreeBuilder DefineFunction(string name, Action<IFunctionBuilder> action)
    {
        var builder = new FunctionBuilder(name);

        action(builder);

        var function = builder.Build();
        declaration.Add(function);

        return this;
    }

    public ISyntaxTreeBuilder DefineType(string name, Action<ITypeBuilder>? action = null)
    {
        var builder = new TypeBuilder(name);
        action?.Invoke(builder);

        var type = builder.Build();
        declaration.Add(type);

        return this;
    }

    public ISyntaxTreeBuilder DefineAliasType(string name, Action<ITypeAliasBuilder> action)
    {
        var builder = new TypeAliasBuilder(name);
        action(builder);

        var aliasType = builder.Build();
        declaration.Add(aliasType);

        return this;
    }

    public SyntaxTree Build()
        => new SyntaxTree(declaration);

    private sealed class FunctionBuilder : IFunctionBuilder
    {
        private readonly string functionName;
        private readonly List<ParameterNode> parameters;
        private IInlineTypeNode returnType;
        private BlockStatementNode body;

        public FunctionBuilder(string functionName)
        {
            this.functionName = functionName;
            parameters = [];
            returnType = new TypeNode("void");
            body = new BlockStatementNode();
        }

        public IFunctionBuilder DefineParameter(string name, Func<IInlineTypeBuilder, IInlineTypeNode> action)
        {
            var builder = new InlineTypeBuilder();
            var type = action(builder);
            parameters.Add(new ParameterNode(name, type));

            return this;
        }

        public IFunctionBuilder ReturnType(string type)
            => ReturnType(x => x.Type(type));

        public IFunctionBuilder ReturnType(Func<IInlineTypeBuilder, IInlineTypeNode> action)
        {
            var builder = new InlineTypeBuilder();
            var type = action(builder);

            returnType = type;

            return this;
        }

        public IFunctionBuilder Body(Action<IBlockBuilder> action)
        {
            var builder = new BlockBuilder();

            action(builder);
            body = builder.Build();

            return this;
        }

        public FunctionDeclarationNode Build()
            => FunctionDeclarationNode.Create(functionName, parameters, returnType, body);
    }

    private sealed class TypeBuilder : ITypeBuilder
    {
        private readonly string typeName;
        private readonly List<TypeNode> genericArguments;
        private readonly List<TypeNode> interfaces;
        private readonly List<PropertyDeclarationNode> properties;
        private readonly List<ConstructorDeclarationNode> constructors;
        private readonly List<MethodDeclarationNode> methods;
        private AccessModifier accessModifier;

        public TypeBuilder(string typeName)
        {
            this.typeName = typeName;
            genericArguments = [];
            interfaces = [];
            properties = [];
            constructors = [];
            methods = [];
            accessModifier = Trilang.Parsing.Ast.AccessModifier.Public;
        }

        public ITypeBuilder AccessModifier(AccessModifier modifier)
        {
            this.accessModifier = modifier;

            return this;
        }

        public ITypeBuilder DefineProperty(string name, string type, Action<IPropertyBuilder>? action = null)
            => DefineProperty(name, t => t.Type(type), action);

        public ITypeBuilder DefineProperty(
            string name,
            Func<IInlineTypeBuilder, IInlineTypeNode> type,
            Action<IPropertyBuilder>? action = null)
        {
            var typeBuilder = new InlineTypeBuilder();
            var typeNode = type(typeBuilder);

            var builder = new PropertyBuilder(name, typeNode);
            action?.Invoke(builder);

            var property = builder.Build();
            properties.Add(property);

            return this;
        }

        public ITypeBuilder DefineConstructor(Action<IConstructorBuilder> action)
        {
            var builder = new ConstructorBuilder();

            action(builder);

            var constructor = builder.Build();
            constructors.Add(constructor);

            return this;
        }

        public ITypeBuilder DefineMethod(string name, Action<IMethodBuilder> action)
        {
            var builder = new MethodBuilder(name);

            action(builder);

            var method = builder.Build();
            methods.Add(method);

            return this;
        }

        public ITypeBuilder AddInterface(string name)
        {
            var type = new TypeNode(name);
            interfaces.Add(type);

            return this;
        }

        public ITypeBuilder DefineGenericArgument(string name)
        {
            var type = new TypeNode(name);
            genericArguments.Add(type);

            return this;
        }

        public TypeDeclarationNode Build()
            => new TypeDeclarationNode(
                accessModifier,
                typeName,
                genericArguments,
                interfaces,
                properties,
                constructors,
                methods);
    }

    private sealed class PropertyBuilder : IPropertyBuilder
    {
        private readonly string name;
        private readonly IInlineTypeNode type;
        private PropertyGetterNode? getter;
        private PropertySetterNode? setter;

        public PropertyBuilder(string name, IInlineTypeNode type)
        {
            this.name = name;
            this.type = type;
        }

        public IPropertyBuilder Getter(AccessModifier modifier, Action<IBlockBuilder>? action = null)
        {
            var builder = new BlockBuilder();
            action?.Invoke(builder);

            getter = new PropertyGetterNode(modifier, builder.Build());

            return this;
        }

        public IPropertyBuilder Setter(AccessModifier modifier, Action<IBlockBuilder>? action = null)
        {
            var builder = new BlockBuilder();
            action?.Invoke(builder);

            setter = new PropertySetterNode(modifier, builder.Build());

            return this;
        }

        public PropertyDeclarationNode Build()
            => new PropertyDeclarationNode(name, type, getter, setter);
    }

    private sealed class ConstructorBuilder : IConstructorBuilder
    {
        private readonly List<ParameterNode> parameters;
        private AccessModifier accessModifier;
        private BlockStatementNode body;

        public ConstructorBuilder()
        {
            accessModifier = Trilang.Parsing.Ast.AccessModifier.Public;
            parameters = [];
            body = new BlockStatementNode();
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
            var parameter = new ParameterNode(name, type);
            parameters.Add(parameter);

            return this;
        }

        public IConstructorBuilder Body(Action<IBlockBuilder>? action = null)
        {
            var builder = new BlockBuilder();

            action?.Invoke(builder);
            body = builder.Build();

            return this;
        }

        public ConstructorDeclarationNode Build()
            => new ConstructorDeclarationNode(accessModifier, parameters, body);
    }

    private sealed class MethodBuilder : IMethodBuilder
    {
        private readonly string functionName;
        private readonly List<ParameterNode> parameters;
        private AccessModifier accessModifier;
        private TypeNode returnType;
        private BlockStatementNode body;
        private bool isStatic;

        public MethodBuilder(string functionName)
        {
            this.functionName = functionName;
            accessModifier = Trilang.Parsing.Ast.AccessModifier.Public;
            parameters = [];
            returnType = new TypeNode("void");
            body = new BlockStatementNode();
            isStatic = false;
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
            var parameter = new ParameterNode(name, type);
            parameters.Add(parameter);

            return this;
        }

        public IMethodBuilder ReturnType(string type)
        {
            returnType = new TypeNode(type);

            return this;
        }

        public IMethodBuilder Body(Action<IBlockBuilder>? action = null)
        {
            var builder = new BlockBuilder();

            action?.Invoke(builder);
            body = builder.Build();

            return this;
        }

        public IMethodBuilder Static()
        {
            isStatic = true;

            return this;
        }

        public MethodDeclarationNode Build()
            => new MethodDeclarationNode(accessModifier, isStatic, functionName, parameters, returnType, body);
    }

    private sealed class BlockBuilder : IBlockBuilder
    {
        private readonly List<IStatementNode> statements;

        public BlockBuilder()
            => statements = [];

        public IBlockBuilder DefineVariable(string name, string type, Action<IExpressionBuilder> action)
            => DefineVariable(name, new TypeNode(type), action);

        public IBlockBuilder DefineVariable(string name, IInlineTypeNode type, Action<IExpressionBuilder> action)
        {
            var builder = new ExpressionBuilder();
            action(builder);

            var variable = new VariableDeclarationStatementNode(name, type, builder.Build());
            statements.Add(variable);

            return this;
        }

        public IBlockBuilder Return(Action<IExpressionBuilder>? action = null)
        {
            var exp = default(IExpressionNode);
            if (action is not null)
            {
                var builder = new ExpressionBuilder();
                action(builder);
                exp = builder.Build();
            }

            var returnNode = new ReturnStatementNode(exp);
            statements.Add(returnNode);

            return this;
        }

        public IBlockBuilder Statement(Action<IExpressionBuilder> action)
        {
            var builder = new ExpressionBuilder();
            action(builder);

            var statement = new ExpressionStatementNode(builder.Build());
            statements.Add(statement);

            return this;
        }

        public IBlockBuilder Block(Action<IBlockBuilder> action)
        {
            var builder = new BlockBuilder();
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
            var conditionBuilder = new ExpressionBuilder();
            condition(conditionBuilder);

            var thenBuilder = new BlockBuilder();
            then(thenBuilder);

            BlockBuilder? elseBuilder = null;
            if (@else is not null)
            {
                elseBuilder = new BlockBuilder();
                @else(elseBuilder);
            }

            var ifStatement = new IfStatementNode(
                conditionBuilder.Build(),
                thenBuilder.Build(),
                elseBuilder?.Build());
            statements.Add(ifStatement);

            return this;
        }

        public IBlockBuilder While(Action<IExpressionBuilder> condition, Action<IBlockBuilder> body)
        {
            var conditionBuilder = new ExpressionBuilder();
            condition(conditionBuilder);

            var bodyBuilder = new BlockBuilder();
            body(bodyBuilder);

            var whileStatement = new WhileNode(conditionBuilder.Build(), bodyBuilder.Build());
            statements.Add(whileStatement);

            return this;
        }

        public IBlockBuilder Break()
        {
            statements.Add(new BreakNode());

            return this;
        }

        public IBlockBuilder Continue()
        {
            statements.Add(new ContinueNode());

            return this;
        }

        public IBlockBuilder Expression(Action<IExpressionBuilder> action)
        {
            var builder = new ExpressionBuilder();
            action(builder);

            var expression = builder.Build();
            var statement = new ExpressionStatementNode(expression);
            statements.Add(statement);

            return this;
        }

        public BlockStatementNode Build()
            => new BlockStatementNode(statements);
    }

    private sealed class ExpressionBuilder : IExpressionBuilder
    {
        private readonly Stack<IExpressionNode> stack;

        public ExpressionBuilder()
            => stack = [];

        public IExpressionBuilder Number(int number)
        {
            var literal = LiteralExpressionNode.Number(number);

            stack.Push(literal);

            return this;
        }

        public IExpressionBuilder True()
        {
            var literal = LiteralExpressionNode.True();

            stack.Push(literal);

            return this;
        }

        public IExpressionBuilder False()
        {
            var literal = LiteralExpressionNode.False();

            stack.Push(literal);

            return this;
        }

        public IExpressionBuilder Char(char c)
        {
            var literal = LiteralExpressionNode.Char(c);

            stack.Push(literal);

            return this;
        }

        public IExpressionBuilder String(string str)
        {
            var literal = LiteralExpressionNode.String(str);

            stack.Push(literal);

            return this;
        }

        public IExpressionBuilder MemberAccess(string name, bool @new = false)
        {
            var parent = default(IExpressionNode);
            if (!@new)
                stack.TryPop(out parent);

            var memberAccess = new MemberAccessExpressionNode(parent, name);
            stack.Push(memberAccess);

            return this;
        }

        public IExpressionBuilder ArrayAccess()
        {
            var index = stack.Pop();
            var array = (MemberAccessExpressionNode)stack.Pop();
            var arrayAccess = new ArrayAccessExpressionNode(array, index);
            stack.Push(arrayAccess);

            return this;
        }

        public IExpressionBuilder Unary(UnaryExpressionKind kind)
        {
            var plus = new UnaryExpressionNode(kind, stack.Pop());
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
            var exp = new BinaryExpressionNode(kind, left, right);
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

        public IExpressionBuilder Call()
        {
            var memberAccess = stack.Pop();
            var parameters = new IExpressionNode[stack.Count];
            for (var i = stack.Count - 1; i >= 0; i--)
                parameters[i] = stack.Pop();

            var call = new CallExpressionNode(memberAccess, parameters);
            stack.Push(call);

            return this;
        }

        public IExpressionBuilder NewObject(string type)
        {
            var parameters = new IExpressionNode[stack.Count];
            for (var i = stack.Count - 1; i >= 0; i--)
                parameters[i] = stack.Pop();

            var newOp = new NewObjectExpressionNode(new TypeNode(type), parameters);
            stack.Push(newOp);

            return this;
        }

        public IExpressionBuilder NewObject(string type, params string[] args)
        {
            var parameters = new IExpressionNode[stack.Count];
            for (var i = stack.Count - 1; i >= 0; i--)
                parameters[i] = stack.Pop();

            var genericTypeNode = new GenericTypeNode(type, args.Select(x => new TypeNode(x)).ToList());
            var newOp = new NewObjectExpressionNode(genericTypeNode, parameters);
            stack.Push(newOp);

            return this;
        }

        public IExpressionBuilder NewArray(string type)
        {
            var newArray = new NewArrayExpressionNode(new ArrayTypeNode(new TypeNode(type)), stack.Pop());
            stack.Push(newArray);

            return this;
        }

        public IExpressionBuilder Tuple()
        {
            var parameters = new IExpressionNode[stack.Count];
            for (var i = stack.Count - 1; i >= 0; i--)
                parameters[i] = stack.Pop();

            var tuple = new TupleExpressionNode(parameters);
            stack.Push(tuple);

            return this;
        }

        public IExpressionBuilder As(string type)
        {
            var exp = stack.Pop();
            var asNode = new AsExpressionNode(exp, new TypeNode(type));
            stack.Push(asNode);

            return this;
        }

        public IExpressionNode Build()
        {
            if (stack.Count != 1)
                throw new Exception();

            return stack.Pop();
        }
    }

    private sealed class TypeAliasBuilder : ITypeAliasBuilder
    {
        private readonly string typeName;
        private readonly List<TypeNode> genericArguments;
        private AccessModifier accessModifier;
        private IInlineTypeNode? aliasedType;

        public TypeAliasBuilder(string typeName)
        {
            this.typeName = typeName;
            this.genericArguments = [];
            accessModifier = Trilang.Parsing.Ast.AccessModifier.Public;
        }

        public ITypeAliasBuilder AccessModifier(AccessModifier modifier)
        {
            accessModifier = modifier;

            return this;
        }

        public ITypeAliasBuilder DefineGenericArgument(string name)
        {
            genericArguments.Add(new TypeNode(name));

            return this;
        }

        public ITypeAliasBuilder Type(string name)
        {
            aliasedType = new TypeNode(name);

            return this;
        }

        public ITypeAliasBuilder Array(string name)
        {
            aliasedType = new ArrayTypeNode(new TypeNode(name));

            return this;
        }

        public ITypeAliasBuilder FunctionType(Action<IFunctionTypeBuilder> action)
        {
            var builder = new FunctionTypeBuilder();
            action(builder);

            aliasedType = builder.Build();

            return this;
        }

        public ITypeAliasBuilder Interface(Action<IInterfaceBuilder>? action = null)
        {
            var builder = new InterfaceBuilder();
            action?.Invoke(builder);

            aliasedType = builder.Build();

            return this;
        }

        public ITypeAliasBuilder DiscriminatedUnion(Action<IDiscriminatedUnionBuilder> action)
        {
            var builder = new DiscriminatedUnionBuilder();
            action(builder);

            aliasedType = builder.Build();

            return this;
        }

        public ITypeAliasBuilder Tuple(Action<ITupleBuilder> action)
        {
            var builder = new TupleBuilder();
            action(builder);

            aliasedType = builder.Build();

            return this;
        }

        public ITypeAliasBuilder Generic(string name, Action<IGenericTypeBuilder> action)
        {
            var builder = new GenericTypeBuilder(name);
            action(builder);

            aliasedType = builder.Build();

            return this;
        }

        public TypeAliasDeclarationNode Build()
            => new TypeAliasDeclarationNode(
                accessModifier,
                typeName,
                genericArguments,
                aliasedType ?? throw new Exception());
    }

    private sealed class FunctionTypeBuilder : IFunctionTypeBuilder
    {
        private readonly List<IInlineTypeNode> parameterTypes;
        private TypeNode returnType;

        public FunctionTypeBuilder()
        {
            parameterTypes = [];
            returnType = new TypeNode("void");
        }

        public IFunctionTypeBuilder DefineParameter(string type)
        {
            parameterTypes.Add(new TypeNode(type));

            return this;
        }

        public IFunctionTypeBuilder DefineParameter(Func<IInlineTypeBuilder, IInlineTypeNode> action)
        {
            var builder = new InlineTypeBuilder();
            var type = action(builder);
            parameterTypes.Add(type);

            return this;
        }

        public IFunctionTypeBuilder ReturnType(string type)
        {
            returnType = new TypeNode(type);

            return this;
        }

        public FunctionTypeNode Build()
            => new FunctionTypeNode(parameterTypes, returnType);
    }

    private sealed class InterfaceBuilder : IInterfaceBuilder
    {
        private readonly List<InterfacePropertyNode> properties;
        private readonly List<InterfaceMethodNode> methods;

        public InterfaceBuilder()
        {
            this.properties = [];
            this.methods = [];
        }

        public IInterfaceBuilder DefineProperty(
            string name,
            string type,
            AccessModifier getterModifier = AccessModifier.Public,
            AccessModifier setterModifier = AccessModifier.Private)
        {
            var property = new InterfacePropertyNode(
                name,
                new TypeNode(type),
                getterModifier,
                setterModifier);

            properties.Add(property);

            return this;
        }

        public IInterfaceBuilder DefineMethod(string name, Action<IInterfaceMethodBuilder>? action = null)
        {
            var builder = new InterfaceMethodBuilder(name);
            action?.Invoke(builder);

            var method = builder.Build();
            methods.Add(method);

            return this;
        }

        public InterfaceNode Build()
            => new InterfaceNode(properties, methods);
    }

    private sealed class InterfaceMethodBuilder : IInterfaceMethodBuilder
    {
        private readonly string methodName;
        private readonly List<IInlineTypeNode> parameters;
        private IInlineTypeNode returnType;

        public InterfaceMethodBuilder(string methodName)
        {
            this.methodName = methodName;
            parameters = [];
            returnType = new TypeNode("void");
        }

        public IInterfaceMethodBuilder DefineParameter(string type)
        {
            var parameter = new TypeNode(type);

            parameters.Add(parameter);

            return this;
        }

        public IInterfaceMethodBuilder ReturnType(string type)
        {
            returnType = new TypeNode(type);

            return this;
        }

        public InterfaceMethodNode Build()
            => new InterfaceMethodNode(methodName, parameters, returnType);
    }

    private sealed class DiscriminatedUnionBuilder : IDiscriminatedUnionBuilder
    {
        private readonly List<IInlineTypeNode> types;

        public DiscriminatedUnionBuilder()
            => types = [];

        public IDiscriminatedUnionBuilder AddCase(Func<IInlineTypeBuilder, IInlineTypeNode> action)
        {
            var builder = new InlineTypeBuilder();
            var type = action(builder);

            types.Add(type);

            return this;
        }

        public DiscriminatedUnionNode Build()
            => new DiscriminatedUnionNode(types);
    }

    private sealed class TupleBuilder : ITupleBuilder
    {
        private readonly List<IInlineTypeNode> types;

        public TupleBuilder()
            => types = [];

        public ITupleBuilder AddCase(Func<IInlineTypeBuilder, IInlineTypeNode> action)
        {
            var builder = new InlineTypeBuilder();
            var type = action(builder);

            types.Add(type);

            return this;
        }

        public TupleTypeNode Build()
            => new TupleTypeNode(types);
    }

    private sealed class InlineTypeBuilder : IInlineTypeBuilder
    {
        public IInlineTypeNode Type(string name)
            => new TypeNode(name);

        public IInlineTypeNode Array(string name)
            => new ArrayTypeNode(new TypeNode(name));

        public IInlineTypeNode FunctionType(Action<IFunctionTypeBuilder> action)
        {
            var builder = new FunctionTypeBuilder();
            action(builder);

            return builder.Build();
        }

        public IInlineTypeNode Interface(Action<IInterfaceBuilder>? action = null)
        {
            var builder = new InterfaceBuilder();
            action?.Invoke(builder);

            return builder.Build();
        }

        public IInlineTypeNode DiscriminatedUnion(Action<IDiscriminatedUnionBuilder> action)
        {
            var builder = new DiscriminatedUnionBuilder();
            action(builder);

            return builder.Build();
        }

        public IInlineTypeNode Tuple(Action<ITupleBuilder> action)
        {
            var builder = new TupleBuilder();
            action(builder);

            return builder.Build();
        }

        public IInlineTypeNode Generic(string name, Action<IGenericTypeBuilder> action)
        {
            var builder = new GenericTypeBuilder(name);
            action(builder);

            return builder.Build();
        }
    }

    private sealed class GenericTypeBuilder : IGenericTypeBuilder
    {
        private readonly string typeName;
        private readonly List<IInlineTypeNode> genericArguments;

        public GenericTypeBuilder(string typeName)
        {
            this.typeName = typeName;
            genericArguments = [];
        }

        public IGenericTypeBuilder DefineGenericArgument(string name)
        {
            var type = new TypeNode(name);
            genericArguments.Add(type);

            return this;
        }

        public IGenericTypeBuilder DefineGenericArgument(Func<IInlineTypeBuilder, IInlineTypeNode> action)
        {
            var builder = new InlineTypeBuilder();
            var type = action(builder);
            genericArguments.Add(type);

            return this;
        }

        public GenericTypeNode Build()
            => new GenericTypeNode(typeName, genericArguments);
    }
}
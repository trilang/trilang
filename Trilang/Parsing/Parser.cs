using Trilang.Lexing;
using Trilang.Parsing.Ast;

namespace Trilang.Parsing;

public class Parser
{
    public SyntaxTree Parse(string code)
    {
        var lexer = new Lexer();
        var tokens = lexer.Tokenize(code);
        var context = new ParserContext(tokens, this);
        var functions = new List<IDeclarationNode>();

        while (!context.Reader.HasEnded)
        {
            var declaration = TryParseFunction(context) ??
                              TryParseTypeAlias(context) ??
                              TryParseTypeDeclarationNode(context) as IDeclarationNode ??
                              throw new ParseException("Expected a type or a function.");

            functions.Add(declaration);
        }

        return new SyntaxTree(functions);
    }

    private FunctionDeclarationNode? TryParseFunction(ParserContext context)
    {
        if (!context.Reader.Check(TokenKind.Function))
            return null;

        var name = TryParseId(context) ??
                   throw new ParseException("Expected a function name.");

        var parameters = ParseFunctionParameters(context);

        if (!context.Reader.Check(TokenKind.Colon))
            throw new ParseException("Expected a colon.");

        var returnType = TryParseDiscriminatedUnion(context) ??
                         throw new ParseException("Expected a function return type.");

        var block = TryParseBlock(context) ??
                    throw new ParseException("Expected a function block.");

        return FunctionDeclarationNode.Create(name, parameters, returnType, block);
    }

    private List<ParameterNode> ParseFunctionParameters(ParserContext context)
    {
        if (!context.Reader.Check(TokenKind.OpenParenthesis))
            throw new ParseException("Expected an open parenthesis.");

        var parameters = new List<ParameterNode>();

        var parameter = TryParseFunctionParameter(context);
        if (parameter is not null)
        {
            parameters.Add(parameter);

            while (context.Reader.Check(TokenKind.Comma))
            {
                parameter = TryParseFunctionParameter(context) ??
                            throw new ParseException("Expected a parameter.");

                parameters.Add(parameter);
            }
        }

        if (!context.Reader.Check(TokenKind.CloseParenthesis))
            throw new ParseException("Expected an close parenthesis.");

        return parameters;
    }

    private ParameterNode? TryParseFunctionParameter(ParserContext context)
    {
        var name = TryParseId(context);
        if (name is null)
            return null;

        if (!context.Reader.Check(TokenKind.Colon))
            throw new ParseException("Expected a colon.");

        var type = TryParseDiscriminatedUnion(context) ??
                   throw new ParseException("Expected a type.");

        return new ParameterNode(name, type);
    }

    private BlockStatementNode? TryParseBlock(ParserContext context)
    {
        if (!context.Reader.Check(TokenKind.OpenBrace))
            return null;

        var statements = new List<IStatementNode>();
        while (!context.Reader.Check(TokenKind.CloseBrace))
        {
            var statement = TryParseStatement(context) ??
                            throw new ParseException("Expected a statement.");

            statements.Add(statement);
        }

        return new BlockStatementNode(statements);
    }

    private AccessModifier? TryParseAccessModifier(ParserContext context)
    {
        if (context.Reader.Check(TokenKind.Public))
            return AccessModifier.Public;

        if (context.Reader.Check(TokenKind.Private))
            return AccessModifier.Private;

        return null;
    }

    private TypeAliasDeclarationNode? TryParseTypeAlias(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var accessModifier = c.Parser.TryParseAccessModifier(c);
            if (accessModifier is null)
                return null;

            if (!c.Reader.Check(TokenKind.Type))
                return null;

            var name = c.Parser.TryParseId(c) ??
                       throw new ParseException("Expected a type alias name.");

            var genericArguments = c.Parser.TryParseGenericTypeArguments(c);

            if (!c.Reader.Check(TokenKind.Equal))
                return null;

            var type = c.Parser.TryParseDiscriminatedUnion(c) ??
                       throw new ParseException("Expected a type.");

            if (type is not InterfaceNode && !c.Reader.Check(TokenKind.SemiColon))
                throw new ParseException("Expected a semicolon.");

            return new TypeAliasDeclarationNode(accessModifier.Value, name, genericArguments, type);
        });

    private TypeDeclarationNode? TryParseTypeDeclarationNode(ParserContext context)
    {
        var accessModifier = TryParseAccessModifier(context);
        if (accessModifier is null)
            return null;

        if (!context.Reader.Check(TokenKind.Type))
            throw new ParseException("Expected a type declaration.");

        var name = TryParseId(context) ??
                   throw new ParseException("Expected a type name.");

        var genericArguments = TryParseGenericTypeArguments(context);

        var interfaces = new List<TypeNode>();
        if (context.Reader.Check(TokenKind.Colon))
        {
            var @interface = TryParseTypeNode(context) ??
                             throw new ParseException("Expected an interface.");

            interfaces.Add(@interface);

            while (context.Reader.Check(TokenKind.Comma))
            {
                @interface = TryParseTypeNode(context) ??
                             throw new ParseException("Expected an interface.");

                interfaces.Add(@interface);
            }
        }

        if (!context.Reader.Check(TokenKind.OpenBrace))
            throw new ParseException("Expected an open brace.");

        var properties = new List<PropertyDeclarationNode>();
        var constructors = new List<ConstructorDeclarationNode>();
        var methods = new List<MethodDeclarationNode>();

        while (!context.Reader.Check(TokenKind.CloseBrace))
        {
            var constructor = TryParseConstructor(context);
            if (constructor is not null)
            {
                constructors.Add(constructor);
                continue;
            }

            var property = TryParseProperty(context);
            if (property is not null)
            {
                properties.Add(property);
                continue;
            }

            var method = TryParseMethod(context);
            if (method is not null)
            {
                methods.Add(method);
                continue;
            }

            throw new ParseException("Expected a property or a method.");
        }

        return new TypeDeclarationNode(
            accessModifier.Value,
            name,
            genericArguments,
            interfaces,
            properties,
            constructors,
            methods);
    }

    private List<TypeNode> TryParseGenericTypeArguments(ParserContext context)
    {
        var arguments = new List<TypeNode>();

        if (!context.Reader.Check(TokenKind.Less))
            return arguments;

        var type = TryParseTypeNode(context) ??
                   throw new ParseException("Expected a type.");

        arguments.Add(type);

        while (context.Reader.Check(TokenKind.Comma))
        {
            type = TryParseTypeNode(context) ??
                   throw new ParseException("Expected a type.");

            arguments.Add(type);
        }

        if (!context.Reader.Check(TokenKind.Greater))
            throw new ParseException("Expected a greater than sign.");

        return arguments;
    }

    private ConstructorDeclarationNode? TryParseConstructor(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var accessModifier = c.Parser.TryParseAccessModifier(c);
            if (accessModifier is null)
                return null;

            if (!c.Reader.Check(TokenKind.Constructor))
                return null;

            var parameters = c.Parser.ParseFunctionParameters(c);

            var block = c.Parser.TryParseBlock(c) ??
                        throw new ParseException("Expected a constructor block.");

            return new ConstructorDeclarationNode(accessModifier.Value, parameters, block);
        });

    private PropertyDeclarationNode? TryParseProperty(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var name = c.Parser.TryParseId(c);
            if (name is null)
                return null;

            if (!c.Reader.Check(TokenKind.Colon))
                return null;

            var type = c.Parser.TryParseDiscriminatedUnion(c) ??
                       throw new ParseException("Expected a type.");

            if (c.Reader.Check(TokenKind.SemiColon))
                return new PropertyDeclarationNode(name, type, null, null);

            if (!c.Reader.Check(TokenKind.OpenBrace))
                throw new ParseException("Expected an open brace.");

            var getter = c.Parser.TryParsePropertyGetter(c);
            var setter = c.Parser.TryParsePropertySetter(c);

            if (!c.Reader.Check(TokenKind.CloseBrace))
                throw new ParseException("Expected a close brace.");

            return new PropertyDeclarationNode(name, type, getter, setter);
        });

    private PropertyGetterNode? TryParsePropertyGetter(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var accessModifier = c.Parser.TryParseAccessModifier(c);
            if (accessModifier is null)
                return null;

            if (!c.Reader.Check(TokenKind.Get))
                return null;

            if (c.Reader.Check(TokenKind.SemiColon))
                return new PropertyGetterNode(accessModifier.Value, null);

            var body = c.Parser.TryParseBlock(c);

            return new PropertyGetterNode(accessModifier.Value, body);
        });

    private PropertySetterNode? TryParsePropertySetter(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var accessModifier = c.Parser.TryParseAccessModifier(c);
            if (accessModifier is null)
                return null;

            if (!c.Reader.Check(TokenKind.Set))
                return null;

            if (c.Reader.Check(TokenKind.SemiColon))
                return new PropertySetterNode(accessModifier.Value, null);

            var body = c.Parser.TryParseBlock(c);

            return new PropertySetterNode(accessModifier.Value, body);
        });

    private MethodDeclarationNode? TryParseMethod(ParserContext context)
    {
        var accessModifier = TryParseAccessModifier(context);
        if (accessModifier is null)
            return null;

        var name = TryParseId(context) ??
                   throw new ParseException("Expected a method name.");

        var parameters = ParseFunctionParameters(context);

        if (!context.Reader.Check(TokenKind.Colon))
            throw new ParseException("Expected a colon.");

        var returnType = TryParseDiscriminatedUnion(context) ??
                         throw new ParseException("Expected a function return type.");

        var block = TryParseBlock(context) ??
                    throw new ParseException("Expected a function block.");

        return new MethodDeclarationNode(accessModifier.Value, name, parameters, returnType, block);
    }

    private IStatementNode? TryParseStatement(ParserContext context)
        => TryParseVariableStatement(context) ??
           TryParseIfStatement(context) ??
           TryParseReturnStatement(context) ??
           TryParseExpressionStatement(context) ??
           TryParseWhileStatement(context) ??
           TryParseBreakStatement(context) ??
           TryParseContinueStatement(context) as IStatementNode;

    private VariableDeclarationStatementNode? TryParseVariableStatement(ParserContext context)
    {
        if (!context.Reader.Check(TokenKind.Var))
            return null;

        var name = TryParseId(context) ??
                   throw new ParseException("Expected a variable name.");

        if (!context.Reader.Check(TokenKind.Colon))
            throw new ParseException("Expected a colon.");

        var type = TryParseDiscriminatedUnion(context) ??
                   throw new ParseException("Expected a type.");

        if (!context.Reader.Check(TokenKind.Equal))
            throw new ParseException("Expected an equal sign.");

        var expression = TryParseExpression(context) ??
                         throw new ParseException("Expected an expression.");

        if (!context.Reader.Check(TokenKind.SemiColon))
            throw new ParseException("Expected a semicolon.");

        return new VariableDeclarationStatementNode(name, type, expression);
    }

    private IfStatementNode? TryParseIfStatement(ParserContext context)
    {
        if (!context.Reader.Check(TokenKind.If))
            return null;

        if (!context.Reader.Check(TokenKind.OpenParenthesis))
            throw new ParseException("Expected an open parenthesis.");

        var condition = TryParseExpression(context) ??
                        throw new ParseException("Expected an expression.");

        if (!context.Reader.Check(TokenKind.CloseParenthesis))
            throw new ParseException("Expected a close parenthesis.");

        var then = TryParseBlock(context) ??
                   throw new ParseException("Expected a 'then' block.");

        if (!context.Reader.Check(TokenKind.Else))
            return new IfStatementNode(condition, then);

        var @else = TryParseBlock(context) ??
                    throw new ParseException("Expected a 'else' block.");

        return new IfStatementNode(condition, then, @else);
    }

    private ReturnStatementNode? TryParseReturnStatement(ParserContext context)
    {
        if (!context.Reader.Check(TokenKind.Return))
            return null;

        var expression = TryParseExpression(context);

        if (!context.Reader.Check(TokenKind.SemiColon))
            throw new ParseException("Expected a semicolon.");

        return new ReturnStatementNode(expression);
    }

    private WhileNode? TryParseWhileStatement(ParserContext context)
    {
        if (!context.Reader.Check(TokenKind.While))
            return null;

        if (!context.Reader.Check(TokenKind.OpenParenthesis))
            throw new ParseException("Expected an open parenthesis.");

        var condition = TryParseExpression(context) ??
                        throw new ParseException("Expected a condition.");

        if (!context.Reader.Check(TokenKind.CloseParenthesis))
            throw new ParseException("Expected a close parenthesis.");

        var block = TryParseBlock(context) ??
                    throw new ParseException("Expected a block.");

        return new WhileNode(condition, block);
    }

    private BreakNode? TryParseBreakStatement(ParserContext context)
    {
        if (!context.Reader.Check(TokenKind.Break))
            return null;

        if (!context.Reader.Check(TokenKind.SemiColon))
            throw new ParseException("Expected a semicolon.");

        return new BreakNode();
    }

    private ContinueNode? TryParseContinueStatement(ParserContext context)
    {
        if (!context.Reader.Check(TokenKind.Continue))
            return null;

        if (!context.Reader.Check(TokenKind.SemiColon))
            throw new ParseException("Expected a semicolon.");

        return new ContinueNode();
    }

    private ExpressionStatementNode? TryParseExpressionStatement(ParserContext context)
    {
        var expression = TryParseExpression(context);
        if (expression is null)
            return null;

        if (!context.Reader.Check(TokenKind.SemiColon))
            throw new ParseException("Expected a semicolon.");

        return new ExpressionStatementNode(expression);
    }

    private IExpressionNode? TryParseExpression(ParserContext context)
        => TryParseBinaryExpression(context);

    private IExpressionNode? TryParseBinaryExpression(ParserContext context, int parentPrecedence = 0)
    {
        var left = TryParseUnaryExpression(context);
        if (left is null)
            return null;

        while (true)
        {
            var kind = BinaryExpressionKind.Unknown;
            if (context.Reader.Current.Is(TokenKind.Plus))
                kind = BinaryExpressionKind.Addition;
            else if (context.Reader.Current.Is(TokenKind.Minus))
                kind = BinaryExpressionKind.Subtraction;
            else if (context.Reader.Current.Is(TokenKind.Asterisk))
                kind = BinaryExpressionKind.Multiplication;
            else if (context.Reader.Current.Is(TokenKind.Slash))
                kind = BinaryExpressionKind.Division;
            else if (context.Reader.Current.Is(TokenKind.Ampersand))
                kind = BinaryExpressionKind.BitwiseAnd;
            else if (context.Reader.Current.Is(TokenKind.Percent))
                kind = BinaryExpressionKind.Modulus;
            else if (context.Reader.Current.Is(TokenKind.Pipe))
                kind = BinaryExpressionKind.BitwiseOr;
            else if (context.Reader.Current.Is(TokenKind.Caret))
                kind = BinaryExpressionKind.BitwiseXor;
            else if (context.Reader.Current.Is(TokenKind.AmpersandAmpersand))
                kind = BinaryExpressionKind.ConditionalAnd;
            else if (context.Reader.Current.Is(TokenKind.PipePipe))
                kind = BinaryExpressionKind.ConditionalOr;
            else if (context.Reader.Current.Is(TokenKind.EqualEqual))
                kind = BinaryExpressionKind.Equality;
            else if (context.Reader.Current.Is(TokenKind.ExclamationEqual))
                kind = BinaryExpressionKind.Inequality;
            else if (context.Reader.Current.Is(TokenKind.Less))
                kind = BinaryExpressionKind.LessThan;
            else if (context.Reader.Current.Is(TokenKind.LessEqual))
                kind = BinaryExpressionKind.LessThanOrEqual;
            else if (context.Reader.Current.Is(TokenKind.Greater))
                kind = BinaryExpressionKind.GreaterThan;
            else if (context.Reader.Current.Is(TokenKind.GreaterEqual))
                kind = BinaryExpressionKind.GreaterThanOrEqual;
            else if (context.Reader.Current.Is(TokenKind.Equal))
                kind = BinaryExpressionKind.Assignment;
            else if (context.Reader.Current.Is(TokenKind.PlusEqual))
                kind = BinaryExpressionKind.AdditionAssignment;
            else if (context.Reader.Current.Is(TokenKind.MinusEqual))
                kind = BinaryExpressionKind.SubtractionAssignment;
            else if (context.Reader.Current.Is(TokenKind.AsteriskEqual))
                kind = BinaryExpressionKind.MultiplicationAssignment;
            else if (context.Reader.Current.Is(TokenKind.SlashEqual))
                kind = BinaryExpressionKind.DivisionAssignment;
            else if (context.Reader.Current.Is(TokenKind.PercentEqual))
                kind = BinaryExpressionKind.ModulusAssignment;
            else if (context.Reader.Current.Is(TokenKind.AmpersandEqual))
                kind = BinaryExpressionKind.BitwiseAndAssignment;
            else if (context.Reader.Current.Is(TokenKind.PipeEqual))
                kind = BinaryExpressionKind.BitwiseOrAssignment;
            else if (context.Reader.Current.Is(TokenKind.CaretEqual))
                kind = BinaryExpressionKind.BitwiseXorAssignment;
            else
                break;

            var precedence = kind.GetPrecedence();
            if (precedence <= parentPrecedence)
                break;

            context.Reader.Advance();
            var right = TryParseBinaryExpression(context, precedence) ??
                        throw new Exception("Expected right operand.");

            left = new BinaryExpressionNode(kind, left, right);
        }

        return left;
    }

    private IExpressionNode? TryParseUnaryExpression(ParserContext context)
    {
        var kind = UnaryExpressionKind.Unknown;
        if (context.Reader.Check(TokenKind.Minus))
            kind = UnaryExpressionKind.UnaryMinus;
        else if (context.Reader.Check(TokenKind.Plus))
            kind = UnaryExpressionKind.UnaryPlus;
        else if (context.Reader.Check(TokenKind.Exclamation))
            kind = UnaryExpressionKind.LogicalNot;
        else if (context.Reader.Check(TokenKind.Tilde))
            kind = UnaryExpressionKind.BitwiseNot;

        if (kind != UnaryExpressionKind.Unknown)
        {
            var operand = TryParseUnaryExpression(context) ??
                          throw new ParseException("Expected an operand.");

            return new UnaryExpressionNode(kind, operand);
        }

        return TryParseMemberExpression(context);
    }

    private IExpressionNode? TryParseMemberExpression(ParserContext context)
    {
        var member = TryParseOperand(context);
        if (member is null)
            return null;

        while (true)
        {
            if (context.Reader.Check(TokenKind.Dot))
            {
                if (!context.Reader.Check(TokenKind.Identifier, out var token))
                    throw new ParseException("Expected an identifier.");

                member = new MemberAccessExpressionNode(member, token.Identifier);
            }
            else if (context.Reader.Check(TokenKind.OpenBracket))
            {
                var index = TryParseExpression(context) ??
                            throw new ParseException("Expected an index.");

                if (!context.Reader.Check(TokenKind.CloseBracket))
                    throw new ParseException("Expected a close bracket.");

                member = new ArrayAccessExpressionNode(member, index);
            }
            else if (context.Reader.Check(TokenKind.OpenParenthesis))
            {
                var arguments = TryParseCallArguments(context);

                if (!context.Reader.Check(TokenKind.CloseParenthesis))
                    throw new ParseException("Expected a close parenthesis.");

                member = new CallExpressionNode(member, arguments);
            }
            else
            {
                break;
            }
        }

        return member;
    }

    private List<IExpressionNode> TryParseCallArguments(ParserContext context)
    {
        var arguments = new List<IExpressionNode>();
        var argument = TryParseExpression(context);
        if (argument is not null)
        {
            arguments.Add(argument);

            while (context.Reader.Check(TokenKind.Comma))
            {
                argument = TryParseExpression(context) ??
                           throw new ParseException("Expected an argument.");

                arguments.Add(argument);
            }
        }

        return arguments;
    }

    private IExpressionNode? TryParseOperand(ParserContext context)
        => TryParseTupleExpression(context) ??
           TryParseParenExpression(context) ??
           TryParseVariable(context) ??
           TryParseNewObjectExpression(context) ??
           TryParseNewArrayExpression(context) ??
           TryParseNullExpression(context) ??
           TryParseLiteral(context) as IExpressionNode;

    private IExpressionNode? TryParseTupleExpression(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            if (!c.Reader.Check(TokenKind.OpenParenthesis))
                return null;

            var expression = c.Parser.TryParseExpression(c);
            if (expression is null)
                return null;

            var expressions = new List<IExpressionNode> { expression };
            while (c.Reader.Check(TokenKind.Comma))
            {
                expression = c.Parser.TryParseExpression(c) ??
                             throw new ParseException("Expected an expression.");

                expressions.Add(expression);
            }

            if (expressions.Count <= 1)
                return null;

            if (!c.Reader.Check(TokenKind.CloseParenthesis))
                throw new ParseException("Expected a close parenthesis.");

            return new TupleExpressionNode(expressions);
        });

    private IExpressionNode? TryParseParenExpression(ParserContext context)
    {
        if (!context.Reader.Check(TokenKind.OpenParenthesis))
            return null;

        var expression = TryParseExpression(context) ??
                         throw new ParseException("Expected an expression.");

        if (!context.Reader.Check(TokenKind.CloseParenthesis))
            throw new ParseException("Expected a close parenthesis.");

        return expression;
    }

    private IExpressionNode? TryParseVariable(ParserContext context)
    {
        var name = TryParseId(context);
        if (name is null)
            return null;

        return new MemberAccessExpressionNode(name);
    }

    private NullExpressionNode? TryParseNullExpression(ParserContext context)
    {
        if (!context.Reader.Check(TokenKind.Null))
            return null;

        return new NullExpressionNode();
    }

    private NewObjectExpressionNode? TryParseNewObjectExpression(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            if (!c.Reader.Check(TokenKind.New))
                return null;

            var type = c.Parser.TryParseGenericTypeNode(c) ??
                       c.Parser.TryParseTypeNode(c) as IInlineTypeNode ??
                       throw new ParseException("Expected a type.");

            if (!c.Reader.Check(TokenKind.OpenParenthesis))
                return null;

            var arguments = c.Parser.TryParseCallArguments(c);

            if (!c.Reader.Check(TokenKind.CloseParenthesis))
                throw new ParseException("Expected a close parenthesis.");

            return new NewObjectExpressionNode(type, arguments);
        });

    private NewArrayExpressionNode? TryParseNewArrayExpression(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            if (!c.Reader.Check(TokenKind.New))
                return null;

            var type = c.Parser.TryParseTypeNode(c) ??
                       throw new ParseException("Expected a type.");

            if (!c.Reader.Check(TokenKind.OpenBracket))
                return null;

            var size = c.Parser.TryParseExpression(c) ??
                       throw new ParseException("Expected a size.");

            if (!c.Reader.Check(TokenKind.CloseBracket))
                throw new ParseException("Expected a close bracket.");

            return new NewArrayExpressionNode(new ArrayTypeNode(type), size);
        });

    private LiteralExpressionNode? TryParseLiteral(ParserContext context)
    {
        if (context.Reader.Check(TokenKind.Number, out var token))
            return new LiteralExpressionNode(LiteralExpressionKind.Number, token.Number);

        if (context.Reader.Check(TokenKind.True, out token))
            return new LiteralExpressionNode(LiteralExpressionKind.Boolean, true);

        if (context.Reader.Check(TokenKind.False, out token))
            return new LiteralExpressionNode(LiteralExpressionKind.Boolean, false);

        if (context.Reader.Check(TokenKind.String, out token))
            return new LiteralExpressionNode(LiteralExpressionKind.String, token.String);

        if (context.Reader.Check(TokenKind.Char, out token))
            return new LiteralExpressionNode(LiteralExpressionKind.Char, token.String);

        return null;
    }

    private IInlineTypeNode? TryParseDiscriminatedUnion(ParserContext context)
    {
        var type = TryParseInlineTypeNode(context);
        if (type is null)
            return null;

        if (!context.Reader.Current.Is(TokenKind.Pipe))
            return type;

        var types = new List<IInlineTypeNode> { type };
        while (context.Reader.Check(TokenKind.Pipe))
        {
            type = TryParseInlineTypeNode(context) ??
                   throw new ParseException("Expected a type.");

            types.Add(type);
        }

        return new DiscriminatedUnionNode(types);
    }

    private IInlineTypeNode? TryParseInlineTypeNode(ParserContext context)
        => TryParseNull(context) ??
           TryParseArrayType(context) ??
           TryParseGenericTypeNode(context) ??
           TryParseTypeNode(context) ??
           TryParseFunctionType(context) ??
           TryParseTupleType(context) ??
           TryParseInterface(context) as IInlineTypeNode;

    private IInlineTypeNode? TryParseNull(ParserContext context)
    {
        if (!context.Reader.Check(TokenKind.Null))
            return null;

        return new TypeNode("null");
    }

    private IInlineTypeNode? TryParseArrayType(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            if (!c.Reader.Check(TokenKind.Identifier, out var token))
                return null;

            if (c.Reader.Current.Is(TokenKind.OpenBracket))
            {
                var next = c.Reader.Peek();
                if (next.Is(TokenKind.CloseBracket))
                {
                    c.Reader.Advance();
                    c.Reader.Advance();

                    var typeNode = new TypeNode(token.Identifier);
                    return new ArrayTypeNode(typeNode);
                }
            }

            return null;
        });

    private GenericTypeNode? TryParseGenericTypeNode(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            if (!c.Reader.Check(TokenKind.Identifier, out var token))
                return null;

            if (!c.Reader.Check(TokenKind.Less))
                return null;

            var typeArguments = new List<IInlineTypeNode>();
            var typeArgument = c.Parser.TryParseInlineTypeNode(c) ??
                               throw new ParseException("Expected a type argument.");

            typeArguments.Add(typeArgument);

            while (c.Reader.Check(TokenKind.Comma))
            {
                typeArgument = c.Parser.TryParseInlineTypeNode(c) ??
                               throw new ParseException("Expected a type argument.");

                typeArguments.Add(typeArgument);
            }

            if (!c.Reader.Check(TokenKind.Greater))
                throw new ParseException("Expected a close angle bracket.");

            return new GenericTypeNode(token.Identifier, typeArguments);
        });

    private TypeNode? TryParseTypeNode(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            if (!c.Reader.Check(TokenKind.Identifier, out var token))
                return null;

            return new TypeNode(token.Identifier);
        });

    private FunctionTypeNode? TryParseFunctionType(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var parameters = c.Parser.TryParseFunctionTypeParameters(c);
            if (parameters is null)
                return null;

            if (!c.Reader.Check(TokenKind.EqualGreater))
                return null;

            var returnType = c.Parser.TryParseDiscriminatedUnion(c) ??
                             throw new ParseException("Expected a function return type.");

            return new FunctionTypeNode(parameters, returnType);
        });

    private IReadOnlyList<IInlineTypeNode>? TryParseFunctionTypeParameters(ParserContext context)
    {
        if (!context.Reader.Check(TokenKind.OpenParenthesis))
            return null;

        var parameters = new List<IInlineTypeNode>();
        var parameter = TryParseDiscriminatedUnion(context);
        if (parameter is not null)
        {
            parameters.Add(parameter);

            while (context.Reader.Check(TokenKind.Comma))
            {
                parameter = TryParseDiscriminatedUnion(context) ??
                            throw new ParseException("Expected a parameter.");

                parameters.Add(parameter);
            }
        }

        if (!context.Reader.Check(TokenKind.CloseParenthesis))
            throw new ParseException("Expected a close parenthesis.");

        return parameters;
    }

    private TupleTypeNode? TryParseTupleType(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            if (!c.Reader.Check(TokenKind.OpenParenthesis))
                return null;

            var type = c.Parser.TryParseDiscriminatedUnion(c);
            if (type is null)
                return null;

            var types = new List<IInlineTypeNode> { type };
            while (c.Reader.Check(TokenKind.Comma))
            {
                type = c.Parser.TryParseDiscriminatedUnion(c) ??
                       throw new ParseException("Expected a type.");

                types.Add(type);
            }

            if (types.Count <= 1)
                return null;

            if (!c.Reader.Check(TokenKind.CloseParenthesis))
                throw new ParseException("Expected a close parenthesis.");

            return new TupleTypeNode(types);
        });

    private InterfaceNode? TryParseInterface(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            if (!c.Reader.Check(TokenKind.OpenBrace))
                return null;

            var properties = c.Parser.TryParseInterfaceProperties(c);
            var methods = c.Parser.TryParseInterfaceMethods(c);

            if (!c.Reader.Check(TokenKind.CloseBrace))
                throw new ParseException("Expected a close brace.");

            return new InterfaceNode(properties, methods);
        });

    private List<InterfacePropertyNode> TryParseInterfaceProperties(ParserContext context)
    {
        var properties = new List<InterfacePropertyNode>();

        while (true)
        {
            var property = TryParseInterfaceProperty(context);
            if (property is null)
                break;

            properties.Add(property);
        }

        return properties;
    }

    private InterfacePropertyNode? TryParseInterfaceProperty(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var name = c.Parser.TryParseId(c);
            if (name is null)
                return null;

            if (!c.Reader.Check(TokenKind.Colon))
                return null;

            var type = c.Parser.TryParseDiscriminatedUnion(c) ??
                       throw new ParseException("Expected a type.");

            if (c.Reader.Check(TokenKind.SemiColon))
                return new InterfacePropertyNode(name, type, null, null);

            if (!c.Reader.Check(TokenKind.OpenBrace))
                throw new ParseException("Expected an open brace.");

            var getter = c.Parser.TryParseInterfacePropertyGetter(c);
            var setter = c.Parser.TryParseInterfacePropertySetter(c);

            if (!c.Reader.Check(TokenKind.CloseBrace))
                throw new ParseException("Expected a close brace.");

            return new InterfacePropertyNode(name, type, getter, setter);
        });

    private AccessModifier? TryParseInterfacePropertyGetter(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var accessModifier = c.Parser.TryParseAccessModifier(c);
            if (accessModifier is null)
                return null;

            if (!c.Reader.Check(TokenKind.Get))
                throw new ParseException("Expected a get keyword.");

            if (!c.Reader.Check(TokenKind.SemiColon))
                throw new ParseException("Expected a semi colon.");

            return accessModifier;
        });

    private AccessModifier? TryParseInterfacePropertySetter(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var accessModifier = c.Parser.TryParseAccessModifier(c);
            if (accessModifier is null)
                return null;

            if (!c.Reader.Check(TokenKind.Set))
                throw new ParseException("Expected a get keyword.");

            if (!c.Reader.Check(TokenKind.SemiColon))
                throw new ParseException("Expected a semi colon.");

            return accessModifier;
        });

    private List<InterfaceMethodNode> TryParseInterfaceMethods(ParserContext context)
    {
        var methods = new List<InterfaceMethodNode>();

        while (true)
        {
            var method = TryParseInterfaceMethod(context);
            if (method is null)
                break;

            methods.Add(method);
        }

        return methods;
    }

    private InterfaceMethodNode? TryParseInterfaceMethod(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var name = c.Parser.TryParseId(c);
            if (name is null)
                return null;

            var parameters = c.Parser.TryParseFunctionTypeParameters(c) ??
                             throw new ParseException("Expected a function parameter list.");

            if (!c.Reader.Check(TokenKind.Colon))
                throw new ParseException("Expected a colon.");

            var returnType = c.Parser.TryParseDiscriminatedUnion(c) ??
                             throw new ParseException("Expected a type.");

            if (!c.Reader.Check(TokenKind.SemiColon))
                throw new ParseException("Expected a semi-colon.");

            return new InterfaceMethodNode(name, parameters, returnType);
        });

    private string? TryParseId(ParserContext context)
    {
        if (!context.Reader.Check(TokenKind.Identifier, out var token))
            return null;

        return token.Identifier;
    }
}
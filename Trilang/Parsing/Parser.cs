using Trilang.Lexing;
using Trilang.Parsing.Ast;
using static Trilang.Lexing.TokenKind;

namespace Trilang.Parsing;

public class Parser
{
    public SyntaxTree Parse(IReadOnlyList<Token> tokens)
    {
        var context = new ParserContext(tokens, this);
        var functions = new List<IDeclarationNode>();

        while (!context.Reader.HasEnded)
        {
            var declaration = TryParseDeclaration(context) ??
                              throw new ParseException("Expected a type or a function.");

            functions.Add(declaration);
        }

        return new SyntaxTree(functions);
    }

    private IDeclarationNode? TryParseDeclaration(ParserContext context)
        => TryParseFunction(context) ??
           TryParseTypeAlias(context) ??
           TryParseTypeDeclarationNode(context) ??
           TryParseTopLevelIfDirective(context) as IDeclarationNode;

    private IfDirectiveNode? TryParseTopLevelIfDirective(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            if (!c.Reader.Check(Hash, out var hash))
                return null;

            if (!c.Reader.Check(If))
                return null;

            var (_, name) = c.Parser.TryParseId(c);
            if (name is null)
                throw new ParseException("Expected a directive name.");

            var then = new List<IDeclarationNode>();
            var @else = new List<IDeclarationNode>();

            while (true)
            {
                var declaration = c.Parser.TryParseDeclaration(c);
                if (declaration is null)
                    break;

                then.Add(declaration);
            }

            if (!c.Reader.Check(Hash))
                throw new ParseException("Expected a hash symbol.");

            if (c.Reader.Check(Else))
            {
                while (true)
                {
                    var declaration = c.Parser.TryParseDeclaration(c);
                    if (declaration is null)
                        break;

                    @else.Add(declaration);
                }

                if (!c.Reader.Check(Hash))
                    throw new ParseException("Expected a hash symbol.");
            }

            if (!c.Reader.Check(EndIf, out var endif))
                throw new ParseException("Expected an 'endif' directive.");

            return new IfDirectiveNode(hash.SourceSpan.Combine(endif.SourceSpan), name, then, @else);
        });

    private FunctionDeclarationNode? TryParseFunction(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var (accessModifierSpan, accessModifier) = c.Parser.TryParseAccessModifier(c);
            if (accessModifier is null)
                return null;

            var (_, name) = c.Parser.TryParseId(c);
            if (name is null)
                return null;

            var parameters = c.Parser.ParseFunctionParameters(c);

            if (!c.Reader.Check(Colon))
                throw new ParseException("Expected a colon.");

            var returnType = c.Parser.TryParseDiscriminatedUnion(c) ??
                             throw new ParseException("Expected a function return type.");

            var block = c.Parser.TryParseBlock(c) ??
                        throw new ParseException("Expected a function block.");

            return FunctionDeclarationNode.Create(
                accessModifierSpan.Combine(block.SourceSpan),
                accessModifier.Value,
                name,
                parameters,
                returnType,
                block);
        });

    private List<ParameterNode> ParseFunctionParameters(ParserContext context)
    {
        if (!context.Reader.Check(OpenParenthesis))
            throw new ParseException("Expected an open parenthesis.");

        var parameters = new List<ParameterNode>();

        var parameter = TryParseFunctionParameter(context);
        if (parameter is not null)
        {
            parameters.Add(parameter);

            while (context.Reader.Check(Comma))
            {
                parameter = TryParseFunctionParameter(context) ??
                            throw new ParseException("Expected a parameter.");

                parameters.Add(parameter);
            }
        }

        if (!context.Reader.Check(CloseParenthesis))
            throw new ParseException("Expected an close parenthesis.");

        return parameters;
    }

    private ParameterNode? TryParseFunctionParameter(ParserContext context)
    {
        var (span, name) = TryParseId(context);
        if (name is null)
            return null;

        if (!context.Reader.Check(Colon))
            throw new ParseException("Expected a colon.");

        var type = TryParseDiscriminatedUnion(context) ??
                   throw new ParseException("Expected a type.");

        return new ParameterNode(span.Combine(type.SourceSpan), name, type);
    }

    private BlockStatementNode? TryParseBlock(ParserContext context)
    {
        if (!context.Reader.Check(OpenBrace, out var openBrace))
            return null;

        var statements = new List<IStatementNode>();
        var closeBrace = default(Token);
        while (!context.Reader.Check(CloseBrace, out closeBrace))
        {
            var statement = TryParseStatement(context) ??
                            throw new ParseException("Expected a statement.");

            statements.Add(statement);
        }

        return new BlockStatementNode(openBrace.SourceSpan.Combine(closeBrace.SourceSpan), statements);
    }

    private (SourceSpan, AccessModifier?) TryParseAccessModifier(ParserContext context)
    {
        if (context.Reader.Check(Public, out var publicKeyword))
            return (publicKeyword.SourceSpan, AccessModifier.Public);

        if (context.Reader.Check(Internal, out var internalKeyword))
            return (internalKeyword.SourceSpan, AccessModifier.Internal);

        if (context.Reader.Check(Private, out var privateKeyword))
            return (privateKeyword.SourceSpan, AccessModifier.Private);

        return (default, null);
    }

    private TypeAliasDeclarationNode? TryParseTypeAlias(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var (span, accessModifier) = c.Parser.TryParseAccessModifier(c);
            if (accessModifier is null)
                return null;

            if (!c.Reader.Check(TokenKind.Type))
                return null;

            var (_, name) = c.Parser.TryParseId(c);
            if (name is null)
                throw new ParseException("Expected a type name.");

            var genericArguments = c.Parser.TryParseGenericTypeArguments(c);

            if (!c.Reader.Check(Equal))
                return null;

            var type = c.Parser.TryParseDiscriminatedUnion(c) ??
                       throw new ParseException("Expected a type.");

            if (type is InterfaceNode)
                return new TypeAliasDeclarationNode(
                    span.Combine(type.SourceSpan),
                    accessModifier.Value,
                    name,
                    genericArguments,
                    type);

            if (!c.Reader.Check(SemiColon, out var semiColon))
                throw new ParseException("Expected a semicolon.");

            return new TypeAliasDeclarationNode(
                span.Combine(semiColon.SourceSpan),
                accessModifier.Value,
                name,
                genericArguments,
                type);
        });

    private TypeDeclarationNode? TryParseTypeDeclarationNode(ParserContext context)
    {
        var (accessModifierSpan, accessModifier) = TryParseAccessModifier(context);
        if (accessModifier is null)
            return null;

        if (!context.Reader.Check(TokenKind.Type))
            throw new ParseException("Expected a type declaration.");

        var (_, name) = TryParseId(context);
        if (name is null)
            throw new ParseException("Expected a type name.");

        var genericArguments = TryParseGenericTypeArguments(context);

        var interfaces = new List<TypeNode>();
        if (context.Reader.Check(Colon))
        {
            var @interface = TryParseTypeNode(context) ??
                             throw new ParseException("Expected an interface.");

            interfaces.Add(@interface);

            while (context.Reader.Check(Comma))
            {
                @interface = TryParseTypeNode(context) ??
                             throw new ParseException("Expected an interface.");

                interfaces.Add(@interface);
            }
        }

        if (!context.Reader.Check(OpenBrace))
            throw new ParseException("Expected an open brace.");

        var properties = new List<PropertyDeclarationNode>();
        var constructors = new List<ConstructorDeclarationNode>();
        var methods = new List<MethodDeclarationNode>();

        var closeBrace = default(Token);
        while (true)
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

            if (context.Reader.Check(CloseBrace, out closeBrace))
                break;

            throw new ParseException("Expected a close brace.");
        }

        return new TypeDeclarationNode(
            accessModifierSpan.Combine(closeBrace.SourceSpan),
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

        if (!context.Reader.Check(Less))
            return arguments;

        var type = TryParseTypeNode(context) ??
                   throw new ParseException("Expected a type.");

        arguments.Add(type);

        while (context.Reader.Check(Comma))
        {
            type = TryParseTypeNode(context) ??
                   throw new ParseException("Expected a type.");

            arguments.Add(type);
        }

        if (!context.Reader.Check(Greater))
            throw new ParseException("Expected a greater than sign.");

        return arguments;
    }

    private ConstructorDeclarationNode? TryParseConstructor(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var (span, accessModifier) = c.Parser.TryParseAccessModifier(c);
            if (accessModifier is null)
                return null;

            if (!c.Reader.Check(Constructor))
                return null;

            var parameters = c.Parser.ParseFunctionParameters(c);

            var block = c.Parser.TryParseBlock(c) ??
                        throw new ParseException("Expected a constructor block.");

            return new ConstructorDeclarationNode(
                span.Combine(block.SourceSpan),
                accessModifier.Value,
                parameters,
                block);
        });

    private PropertyDeclarationNode? TryParseProperty(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var (span, name) = c.Parser.TryParseId(c);
            if (name is null)
                return null;

            if (!c.Reader.Check(Colon))
                return null;

            var type = c.Parser.TryParseDiscriminatedUnion(c) ??
                       throw new ParseException("Expected a type.");

            if (c.Reader.Check(SemiColon, out var semiColon))
                return new PropertyDeclarationNode(span.Combine(semiColon.SourceSpan), name, type);

            if (!c.Reader.Check(OpenBrace))
                throw new ParseException("Expected an open brace.");

            var getter = c.Parser.TryParsePropertyGetter(c);
            var setter = c.Parser.TryParsePropertySetter(c);

            if (!c.Reader.Check(CloseBrace, out var closeBrace))
                throw new ParseException("Expected a close brace.");

            return new PropertyDeclarationNode(span.Combine(closeBrace.SourceSpan), name, type, getter, setter);
        });

    private PropertyGetterNode? TryParsePropertyGetter(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var (span, accessModifier) = c.Parser.TryParseAccessModifier(c);
            if (accessModifier is null)
                return null;

            if (!c.Reader.Check(Get))
                return null;

            if (c.Reader.Check(SemiColon, out var semiColon))
                return new PropertyGetterNode(
                    span.Combine(semiColon.SourceSpan),
                    accessModifier.Value,
                    null);

            var body = c.Parser.TryParseBlock(c) ??
                       throw new ParseException("Expected a getter block.");

            return new PropertyGetterNode(
                span.Combine(body.SourceSpan),
                accessModifier.Value,
                body);
        });

    private PropertySetterNode? TryParsePropertySetter(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var (span, accessModifier) = c.Parser.TryParseAccessModifier(c);
            if (accessModifier is null)
                return null;

            if (!c.Reader.Check(Set))
                return null;

            if (c.Reader.Check(SemiColon, out var semiColon))
                return new PropertySetterNode(
                    span.Combine(semiColon.SourceSpan),
                    accessModifier.Value,
                    null);

            var body = c.Parser.TryParseBlock(c) ??
                       throw new ParseException("Expected a setter block.");

            return new PropertySetterNode(
                span.Combine(body.SourceSpan),
                accessModifier.Value,
                body);
        });

    private MethodDeclarationNode? TryParseMethod(ParserContext context)
    {
        var (span, accessModifier) = TryParseAccessModifier(context);
        if (accessModifier is null)
            return null;

        var isStatic = context.Reader.Check(Static);

        var (_, name) = TryParseId(context);
        if (name is null)
            throw new ParseException("Expected a method name.");

        var parameters = ParseFunctionParameters(context);

        if (!context.Reader.Check(Colon))
            throw new ParseException("Expected a colon.");

        var returnType = TryParseDiscriminatedUnion(context) ??
                         throw new ParseException("Expected a function return type.");

        var block = TryParseBlock(context) ??
                    throw new ParseException("Expected a function block.");

        return new MethodDeclarationNode(
            span.Combine(block.SourceSpan),
            accessModifier.Value,
            isStatic,
            name,
            parameters,
            returnType,
            block);
    }

    private IStatementNode? TryParseStatement(ParserContext context)
        => TryParseBlock(context) ??
           TryParseVariableStatement(context) ??
           TryParseIfStatement(context) ??
           TryParseReturnStatement(context) ??
           TryParseExpressionStatement(context) ??
           TryParseWhileStatement(context) ??
           TryParseBreakStatement(context) ??
           TryParseContinueStatement(context) ??
           TryParseStatementLevelIfDirective(context) as IStatementNode;

    private IfDirectiveNode? TryParseStatementLevelIfDirective(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            if (!c.Reader.Check(Hash, out var hash))
                return null;

            if (!c.Reader.Check(If))
                return null;

            var (_, name) = c.Parser.TryParseId(c);
            if (name is null)
                throw new ParseException("Expected a directive name.");

            var then = new List<IStatementNode>();
            var @else = new List<IStatementNode>();

            while (true)
            {
                var declaration = c.Parser.TryParseStatement(c);
                if (declaration is null)
                    break;

                then.Add(declaration);
            }

            if (!c.Reader.Check(Hash))
                throw new ParseException("Expected a hash symbol.");

            if (c.Reader.Check(Else))
            {
                while (true)
                {
                    var declaration = c.Parser.TryParseStatement(c);
                    if (declaration is null)
                        break;

                    @else.Add(declaration);
                }

                if (!c.Reader.Check(Hash))
                    throw new ParseException("Expected a hash symbol.");
            }

            if (!c.Reader.Check(EndIf, out var endif))
                throw new ParseException("Expected an 'endif' directive.");

            return new IfDirectiveNode(hash.SourceSpan.Combine(endif.SourceSpan), name, then, @else);
        });

    private VariableDeclarationNode? TryParseVariableStatement(ParserContext context)
    {
        if (!context.Reader.Check(Var, out var varKeyword))
            return null;

        var (_, name) = TryParseId(context);
        if (name is null)
            throw new ParseException("Expected a variable name.");

        if (!context.Reader.Check(Colon))
            throw new ParseException("Expected a colon.");

        var type = TryParseDiscriminatedUnion(context) ??
                   throw new ParseException("Expected a type.");

        if (!context.Reader.Check(Equal))
            throw new ParseException("Expected an equal sign.");

        var expression = TryParseExpression(context) ??
                         throw new ParseException("Expected an expression.");

        if (!context.Reader.Check(SemiColon, out var semiColon))
            throw new ParseException("Expected a semicolon.");

        return new VariableDeclarationNode(
            varKeyword.SourceSpan.Combine(semiColon.SourceSpan),
            name,
            type,
            expression);
    }

    private IfStatementNode? TryParseIfStatement(ParserContext context)
    {
        if (!context.Reader.Check(If, out var ifKeyword))
            return null;

        if (!context.Reader.Check(OpenParenthesis))
            throw new ParseException("Expected an open parenthesis.");

        var condition = TryParseExpression(context) ??
                        throw new ParseException("Expected an expression.");

        if (!context.Reader.Check(CloseParenthesis))
            throw new ParseException("Expected a close parenthesis.");

        var then = TryParseBlock(context) ??
                   throw new ParseException("Expected a 'then' block.");

        if (!context.Reader.Check(Else))
            return new IfStatementNode(ifKeyword.SourceSpan.Combine(then.SourceSpan), condition, then);

        var @else = TryParseBlock(context) ??
                    throw new ParseException("Expected a 'else' block.");

        return new IfStatementNode(ifKeyword.SourceSpan.Combine(@else.SourceSpan), condition, then, @else);
    }

    private ReturnStatementNode? TryParseReturnStatement(ParserContext context)
    {
        if (!context.Reader.Check(Return, out var returnToken))
            return null;

        var expression = TryParseExpression(context);

        if (!context.Reader.Check(SemiColon, out var semiColon))
            throw new ParseException("Expected a semicolon.");

        return new ReturnStatementNode(returnToken.SourceSpan.Combine(semiColon.SourceSpan), expression);
    }

    private WhileNode? TryParseWhileStatement(ParserContext context)
    {
        if (!context.Reader.Check(While, out var whileToken))
            return null;

        if (!context.Reader.Check(OpenParenthesis))
            throw new ParseException("Expected an open parenthesis.");

        var condition = TryParseExpression(context) ??
                        throw new ParseException("Expected a condition.");

        if (!context.Reader.Check(CloseParenthesis))
            throw new ParseException("Expected a close parenthesis.");

        var block = TryParseBlock(context) ??
                    throw new ParseException("Expected a block.");

        return new WhileNode(whileToken.SourceSpan.Combine(block.SourceSpan), condition, block);
    }

    private BreakNode? TryParseBreakStatement(ParserContext context)
    {
        if (!context.Reader.Check(Break, out var breakToken))
            return null;

        if (!context.Reader.Check(SemiColon, out var semiColon))
            throw new ParseException("Expected a semicolon.");

        return new BreakNode(breakToken.SourceSpan.Combine(semiColon.SourceSpan));
    }

    private ContinueNode? TryParseContinueStatement(ParserContext context)
    {
        if (!context.Reader.Check(Continue, out var continueToken))
            return null;

        if (!context.Reader.Check(SemiColon, out var semiColon))
            throw new ParseException("Expected a semicolon.");

        return new ContinueNode(continueToken.SourceSpan.Combine(semiColon.SourceSpan));
    }

    private ExpressionStatementNode? TryParseExpressionStatement(ParserContext context)
    {
        var expression = TryParseExpression(context);
        if (expression is null)
            return null;

        if (!context.Reader.Check(SemiColon, out var semiColon))
            throw new ParseException("Expected a semicolon.");

        return new ExpressionStatementNode(expression.SourceSpan.Combine(semiColon.SourceSpan), expression);
    }

    private IExpressionNode? TryParseExpression(ParserContext context)
        => TryParseBinaryExpression(context);

    private IExpressionNode? TryParseBinaryExpression(ParserContext context, int parentPrecedence = 0)
    {
        var left = TryParseIsExpression(context);
        if (left is null)
            return null;

        while (true)
        {
            var kind = BinaryExpressionKind.Unknown;
            if (context.Reader.Current.Is(Plus))
                kind = BinaryExpressionKind.Addition;
            else if (context.Reader.Current.Is(Minus))
                kind = BinaryExpressionKind.Subtraction;
            else if (context.Reader.Current.Is(Asterisk))
                kind = BinaryExpressionKind.Multiplication;
            else if (context.Reader.Current.Is(Slash))
                kind = BinaryExpressionKind.Division;
            else if (context.Reader.Current.Is(Ampersand))
                kind = BinaryExpressionKind.BitwiseAnd;
            else if (context.Reader.Current.Is(Percent))
                kind = BinaryExpressionKind.Modulus;
            else if (context.Reader.Current.Is(Pipe))
                kind = BinaryExpressionKind.BitwiseOr;
            else if (context.Reader.Current.Is(Caret))
                kind = BinaryExpressionKind.BitwiseXor;
            else if (context.Reader.Current.Is(AmpersandAmpersand))
                kind = BinaryExpressionKind.ConditionalAnd;
            else if (context.Reader.Current.Is(PipePipe))
                kind = BinaryExpressionKind.ConditionalOr;
            else if (context.Reader.Current.Is(EqualEqual))
                kind = BinaryExpressionKind.Equality;
            else if (context.Reader.Current.Is(ExclamationEqual))
                kind = BinaryExpressionKind.Inequality;
            else if (context.Reader.Current.Is(Less))
                kind = BinaryExpressionKind.LessThan;
            else if (context.Reader.Current.Is(LessEqual))
                kind = BinaryExpressionKind.LessThanOrEqual;
            else if (context.Reader.Current.Is(Greater))
                kind = BinaryExpressionKind.GreaterThan;
            else if (context.Reader.Current.Is(GreaterEqual))
                kind = BinaryExpressionKind.GreaterThanOrEqual;
            else if (context.Reader.Current.Is(Equal))
                kind = BinaryExpressionKind.Assignment;
            else if (context.Reader.Current.Is(PlusEqual))
                kind = BinaryExpressionKind.AdditionAssignment;
            else if (context.Reader.Current.Is(MinusEqual))
                kind = BinaryExpressionKind.SubtractionAssignment;
            else if (context.Reader.Current.Is(AsteriskEqual))
                kind = BinaryExpressionKind.MultiplicationAssignment;
            else if (context.Reader.Current.Is(SlashEqual))
                kind = BinaryExpressionKind.DivisionAssignment;
            else if (context.Reader.Current.Is(PercentEqual))
                kind = BinaryExpressionKind.ModulusAssignment;
            else if (context.Reader.Current.Is(AmpersandEqual))
                kind = BinaryExpressionKind.BitwiseAndAssignment;
            else if (context.Reader.Current.Is(PipeEqual))
                kind = BinaryExpressionKind.BitwiseOrAssignment;
            else if (context.Reader.Current.Is(CaretEqual))
                kind = BinaryExpressionKind.BitwiseXorAssignment;
            else
                break;

            var precedence = kind.GetPrecedence();
            if (precedence <= parentPrecedence)
                break;

            context.Reader.Advance();
            var right = TryParseBinaryExpression(context, precedence) ??
                        throw new Exception("Expected right operand.");

            left = new BinaryExpressionNode(
                left.SourceSpan.Combine(right.SourceSpan),
                kind,
                left,
                right);
        }

        return left;
    }

    private IExpressionNode? TryParseIsExpression(ParserContext context)
    {
        var expression = TryParseCastExpression(context) ??
                         TryParseUnaryExpression(context);
        if (expression is null)
            return null;

        if (!context.Reader.Check(Is))
            return expression;

        var type = TryParseDiscriminatedUnion(context) ??
                   throw new ParseException("Expected a type.");

        return new IsExpressionNode(expression, type);
    }

    private IExpressionNode? TryParseCastExpression(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            if (!c.Reader.Check(OpenParenthesis, out var openParen))
                return null;

            var type = c.Parser.TryParseDiscriminatedUnion(c);
            if (type is null)
                return null;

            if (!c.Reader.Check(CloseParenthesis))
                throw new ParseException("Expected a close parenthesis.");

            var expression = c.Parser.TryParseUnaryExpression(c) ??
                             throw new ParseException("Expected an expression.");

            return new CastExpressionNode(
                openParen.SourceSpan.Combine(expression.SourceSpan),
                type,
                expression);
        });

    private IExpressionNode? TryParseUnaryExpression(ParserContext context)
    {
        var token = context.Reader.Current;
        var kind = UnaryExpressionKind.Unknown;
        if (token.Is(Minus))
            kind = UnaryExpressionKind.UnaryMinus;
        else if (token.Is(Plus))
            kind = UnaryExpressionKind.UnaryPlus;
        else if (token.Is(Exclamation))
            kind = UnaryExpressionKind.LogicalNot;
        else if (token.Is(Tilde))
            kind = UnaryExpressionKind.BitwiseNot;

        if (kind != UnaryExpressionKind.Unknown)
        {
            context.Reader.Advance();

            var operand = TryParseUnaryExpression(context) ??
                          throw new ParseException("Expected an operand.");

            return new UnaryExpressionNode(token.SourceSpan.Combine(operand.SourceSpan), kind, operand);
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
            if (context.Reader.Check(Dot))
            {
                if (context.Reader.Check(Identifier, out var id))
                {
                    member = new MemberAccessExpressionNode(
                        member.SourceSpan.Combine(id.SourceSpan),
                        member,
                        id.Identifier);
                }
                else if (context.Reader.Check(Integer, out var number))
                {
                    member = new MemberAccessExpressionNode(
                        member.SourceSpan.Combine(number.SourceSpan),
                        member,
                        number.Integer.ToString());
                }
                else
                {
                    throw new ParseException("Expected an identifier.");
                }
            }
            else if (context.Reader.Check(OpenBracket))
            {
                var index = TryParseExpression(context) ??
                            throw new ParseException("Expected an index.");

                if (!context.Reader.Check(CloseBracket, out var closeBracket))
                    throw new ParseException("Expected a close bracket.");

                member = new ArrayAccessExpressionNode(
                    member.SourceSpan.Combine(closeBracket.SourceSpan),
                    member,
                    index);
            }
            else if (context.Reader.Check(OpenParenthesis))
            {
                var arguments = TryParseCallArguments(context);

                if (!context.Reader.Check(CloseParenthesis, out var closeParen))
                    throw new ParseException("Expected a close parenthesis.");

                member = new CallExpressionNode(
                    member.SourceSpan.Combine(closeParen.SourceSpan),
                    member,
                    arguments);
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

            while (context.Reader.Check(Comma))
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
            if (!c.Reader.Check(OpenParenthesis, out var openParen))
                return null;

            var expression = c.Parser.TryParseExpression(c);
            if (expression is null)
                return null;

            var expressions = new List<IExpressionNode> { expression };
            while (c.Reader.Check(Comma))
            {
                expression = c.Parser.TryParseExpression(c) ??
                             throw new ParseException("Expected an expression.");

                expressions.Add(expression);
            }

            if (expressions.Count <= 1)
                return null;

            if (!c.Reader.Check(CloseParenthesis, out var closeParen))
                throw new ParseException("Expected a close parenthesis.");

            return new TupleExpressionNode(
                openParen.SourceSpan.Combine(closeParen.SourceSpan),
                expressions);
        });

    private IExpressionNode? TryParseParenExpression(ParserContext context)
    {
        if (!context.Reader.Check(OpenParenthesis))
            return null;

        var expression = TryParseExpression(context) ??
                         throw new ParseException("Expected an expression.");

        if (!context.Reader.Check(CloseParenthesis))
            throw new ParseException("Expected a close parenthesis.");

        return expression;
    }

    private IExpressionNode? TryParseVariable(ParserContext context)
    {
        var (span, name) = TryParseId(context);
        if (name is null)
            return null;

        return new MemberAccessExpressionNode(span, name);
    }

    private NullExpressionNode? TryParseNullExpression(ParserContext context)
    {
        if (!context.Reader.Check(Null, out var nullKeyword))
            return null;

        return new NullExpressionNode(nullKeyword.SourceSpan);
    }

    private NewObjectExpressionNode? TryParseNewObjectExpression(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            if (!c.Reader.Check(New, out var newKeyword))
                return null;

            var type = c.Parser.TryParseGenericTypeNode(c) ??
                       c.Parser.TryParseTypeNode(c) as IInlineTypeNode ??
                       throw new ParseException("Expected a type.");

            if (!c.Reader.Check(OpenParenthesis))
                return null;

            var arguments = c.Parser.TryParseCallArguments(c);

            if (!c.Reader.Check(CloseParenthesis, out var closeParen))
                throw new ParseException("Expected a close parenthesis.");

            return new NewObjectExpressionNode(
                newKeyword.SourceSpan.Combine(closeParen.SourceSpan),
                type,
                arguments);
        });

    private NewArrayExpressionNode? TryParseNewArrayExpression(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            if (!c.Reader.Check(New, out var newKeyword))
                return null;

            var type = c.Parser.TryParseTypeNode(c) ??
                       throw new ParseException("Expected a type.");

            if (!c.Reader.Check(OpenBracket))
                return null;

            var size = c.Parser.TryParseExpression(c) ??
                       throw new ParseException("Expected a size.");

            if (!c.Reader.Check(CloseBracket, out var closeBracket))
                throw new ParseException("Expected a close bracket.");

            return new NewArrayExpressionNode(
                newKeyword.SourceSpan.Combine(closeBracket.SourceSpan),
                new ArrayTypeNode(type.SourceSpan, type),
                size);
        });

    private LiteralExpressionNode? TryParseLiteral(ParserContext context)
    {
        if (context.Reader.Check(Integer, out var token))
            return LiteralExpressionNode.Integer(token.SourceSpan, token.Integer);

        if (context.Reader.Check(Float, out token))
            return LiteralExpressionNode.Float(token.SourceSpan, token.Float);

        if (context.Reader.Check(True, out token))
            return LiteralExpressionNode.True(token.SourceSpan);

        if (context.Reader.Check(False, out token))
            return LiteralExpressionNode.False(token.SourceSpan);

        if (context.Reader.Check(TokenKind.String, out token))
            return LiteralExpressionNode.String(token.SourceSpan, token.String);

        if (context.Reader.Check(TokenKind.Char, out token))
            return LiteralExpressionNode.Char(token.SourceSpan, token.Char);

        return null;
    }

    private IInlineTypeNode? TryParseDiscriminatedUnion(ParserContext context)
    {
        var type = TryParseInlineTypeNode(context);
        if (type is null)
            return null;

        if (!context.Reader.Current.Is(Pipe))
            return type;

        var types = new List<IInlineTypeNode> { type };
        while (context.Reader.Check(Pipe))
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
           TryParseTupleOrParenthesizedType(context) ??
           TryParseInterface(context);

    private IInlineTypeNode? TryParseNull(ParserContext context)
    {
        if (!context.Reader.Check(Null, out var token))
            return null;

        return new TypeNode(token.SourceSpan, "null");
    }

    private IInlineTypeNode? TryParseArrayType(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            if (!c.Reader.Check(Identifier, out var id))
                return null;

            if (!c.Reader.Check(OpenBracket) ||
                !c.Reader.Check(CloseBracket, out var closeBracket))
                return null;

            var typeNode = new TypeNode(id.SourceSpan, id.Identifier);
            return new ArrayTypeNode(id.SourceSpan.Combine(closeBracket.SourceSpan), typeNode);
        });

    private GenericTypeNode? TryParseGenericTypeNode(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            if (!c.Reader.Check(Identifier, out var token))
                return null;

            if (!c.Reader.Check(Less))
                return null;

            var typeArguments = new List<IInlineTypeNode>();
            var typeArgument = c.Parser.TryParseDiscriminatedUnion(c) ??
                               throw new ParseException("Expected a type argument.");

            typeArguments.Add(typeArgument);

            while (c.Reader.Check(Comma))
            {
                typeArgument = c.Parser.TryParseDiscriminatedUnion(c) ??
                               throw new ParseException("Expected a type argument.");

                typeArguments.Add(typeArgument);
            }

            if (!c.Reader.Check(Greater, out var greaterSign))
                throw new ParseException("Expected a close angle bracket.");

            return new GenericTypeNode(
                token.SourceSpan.Combine(greaterSign.SourceSpan),
                token.Identifier,
                typeArguments);
        });

    private TypeNode? TryParseTypeNode(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            if (!c.Reader.Check(Identifier, out var token))
                return null;

            return new TypeNode(token.SourceSpan, token.Identifier);
        });

    private FunctionTypeNode? TryParseFunctionType(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var (span, parameters) = c.Parser.TryParseFunctionTypeParameters(c);
            if (parameters is null)
                return null;

            if (!c.Reader.Check(EqualGreater))
                return null;

            var returnType = c.Parser.TryParseDiscriminatedUnion(c) ??
                             throw new ParseException("Expected a function return type.");

            return new FunctionTypeNode(span, parameters, returnType);
        });

    private (SourceSpan, IReadOnlyList<IInlineTypeNode>?) TryParseFunctionTypeParameters(ParserContext context)
    {
        if (!context.Reader.Check(OpenParenthesis, out var openParen))
            return (default, null);

        var parameters = new List<IInlineTypeNode>();
        var parameter = TryParseDiscriminatedUnion(context);
        if (parameter is not null)
        {
            parameters.Add(parameter);

            while (context.Reader.Check(Comma))
            {
                parameter = TryParseDiscriminatedUnion(context) ??
                            throw new ParseException("Expected a parameter.");

                parameters.Add(parameter);
            }
        }

        if (!context.Reader.Check(CloseParenthesis, out var closeParen))
            throw new ParseException("Expected a close parenthesis.");

        return (openParen.SourceSpan.Combine(closeParen.SourceSpan), parameters);
    }

    private IInlineTypeNode? TryParseTupleOrParenthesizedType(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            if (!c.Reader.Check(OpenParenthesis, out var openParen))
                return null;

            var type = c.Parser.TryParseDiscriminatedUnion(c);
            if (type is null)
                return null;

            var types = new List<IInlineTypeNode> { type };
            while (c.Reader.Check(Comma))
            {
                type = c.Parser.TryParseDiscriminatedUnion(c) ??
                       throw new ParseException("Expected a type.");

                types.Add(type);
            }

            if (!c.Reader.Check(CloseParenthesis, out var closeParen))
                throw new ParseException("Expected a close parenthesis.");

            if (types.Count == 1)
                return types[0];

            return new TupleTypeNode(openParen.SourceSpan.Combine(closeParen.SourceSpan), types);
        });

    private InterfaceNode? TryParseInterface(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            if (!c.Reader.Check(OpenBrace, out var openBrace))
                return null;

            var properties = c.Parser.TryParseInterfaceProperties(c);
            var methods = c.Parser.TryParseInterfaceMethods(c);

            if (!c.Reader.Check(CloseBrace, out var closeBrace))
            {
                if (properties.Count == 0 && methods.Count == 0)
                    return null;

                throw new ParseException("Expected a close brace.");
            }

            return new InterfaceNode(
                openBrace.SourceSpan.Combine(closeBrace.SourceSpan),
                properties,
                methods);
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
            var (span, name) = c.Parser.TryParseId(c);
            if (name is null)
                return null;

            if (!c.Reader.Check(Colon))
                return null;

            var type = c.Parser.TryParseDiscriminatedUnion(c) ??
                       throw new ParseException("Expected a type.");

            if (c.Reader.Check(SemiColon, out var semiColon))
                return new InterfacePropertyNode(span.Combine(semiColon.SourceSpan), name, type, null, null);

            if (!c.Reader.Check(OpenBrace))
                throw new ParseException("Expected an open brace.");

            var getter = c.Parser.TryParseInterfacePropertyGetter(c);
            var setter = c.Parser.TryParseInterfacePropertySetter(c);

            if (!c.Reader.Check(CloseBrace, out var closeBrace))
                throw new ParseException("Expected a close brace.");

            return new InterfacePropertyNode(
                span.Combine(closeBrace.SourceSpan),
                name,
                type,
                getter,
                setter);
        });

    private AccessModifier? TryParseInterfacePropertyGetter(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var (_, accessModifier) = c.Parser.TryParseAccessModifier(c);
            if (accessModifier is null)
                return null;

            if (!c.Reader.Check(Get))
                return null;

            if (!c.Reader.Check(SemiColon))
                throw new ParseException("Expected a semi colon.");

            return accessModifier;
        });

    private AccessModifier? TryParseInterfacePropertySetter(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var (_, accessModifier) = c.Parser.TryParseAccessModifier(c);
            if (accessModifier is null)
                return null;

            if (!c.Reader.Check(Set))
                return null;

            if (!c.Reader.Check(SemiColon))
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
            var (span, name) = c.Parser.TryParseId(c);
            if (name is null)
                return null;

            var (_, parameters) = c.Parser.TryParseFunctionTypeParameters(c);
            if (parameters is null)
                throw new ParseException("Expected a function parameter list.");

            if (!c.Reader.Check(Colon))
                throw new ParseException("Expected a colon.");

            var returnType = c.Parser.TryParseDiscriminatedUnion(c) ??
                             throw new ParseException("Expected a type.");

            if (!c.Reader.Check(SemiColon, out var semiColon))
                throw new ParseException("Expected a semi-colon.");

            return new InterfaceMethodNode(span.Combine(semiColon.SourceSpan), name, parameters, returnType);
        });

    private (SourceSpan, string?) TryParseId(ParserContext context)
    {
        if (!context.Reader.Check(Identifier, out var token))
            return default;

        return (token.SourceSpan, token.Identifier);
    }
}
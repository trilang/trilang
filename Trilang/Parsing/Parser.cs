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

        var name = TryParseId(context);
        if (name is null)
            throw new ParseException("Expected a function name.");

        var parameters = ParseFunctionParameters(context);

        if (!context.Reader.Check(TokenKind.Colon))
            throw new ParseException("Expected a colon.");

        var returnType = TryParseInlineTypeNode(context);
        if (returnType is null)
            throw new ParseException("Expected a function return type.");

        var block = TryParseBlock(context);
        if (block is null)
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
                parameter = TryParseFunctionParameter(context);
                if (parameter is null)
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

        var type = TryParseInlineTypeNode(context);
        if (type is null)
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
            var statement = TryParseStatement(context);
            if (statement is null)
                throw new ParseException();

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

            var name = c.Parser.TryParseId(c);
            if (name is null)
                throw new ParseException("Expected a type alias name.");

            if (!c.Reader.Check(TokenKind.Equal))
                return null;

            var type = c.Parser.TryParseInlineTypeNode(c);
            if (type is null)
                throw new ParseException("Expected a type.");

            if (type is not InterfaceNode && !c.Reader.Check(TokenKind.SemiColon))
                throw new ParseException("Expected a semicolon.");

            return new TypeAliasDeclarationNode(accessModifier.Value, name, type);
        });

    private TypeDeclarationNode? TryParseTypeDeclarationNode(ParserContext context)
    {
        var accessModifier = TryParseAccessModifier(context);
        if (accessModifier is null)
            return null;

        if (!context.Reader.Check(TokenKind.Type))
            throw new ParseException("Expected a type declaration.");

        var name = TryParseId(context);
        if (name is null)
            throw new ParseException("Expected a type name.");

        if (!context.Reader.Check(TokenKind.OpenBrace))
            throw new ParseException("Expected an open brace.");

        var fields = new List<FieldDeclarationNode>();
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

            var field = TryParseField(context);
            if (field is not null)
            {
                fields.Add(field);
                continue;
            }

            var method = TryParseMethod(context);
            if (method is not null)
            {
                methods.Add(method);
                continue;
            }

            throw new ParseException("Expected a field or a method.");
        }

        return new TypeDeclarationNode(
            accessModifier.Value,
            name,
            fields,
            constructors,
            methods);
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

            var block = c.Parser.TryParseBlock(c);
            if (block is null)
                throw new ParseException("Expected a constructor block.");

            return new ConstructorDeclarationNode(accessModifier.Value, parameters, block);
        });

    private FieldDeclarationNode? TryParseField(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var accessModifier = c.Parser.TryParseAccessModifier(c);
            if (accessModifier is null)
                return null;

            var name = c.Parser.TryParseId(c);
            if (name is null)
                throw new ParseException("Expected a field name.");

            if (!c.Reader.Check(TokenKind.Colon))
                return null;

            var type = c.Parser.TryParseInlineTypeNode(c);
            if (type is null)
                throw new ParseException("Expected a type.");

            if (!c.Reader.Check(TokenKind.SemiColon))
                throw new ParseException("Expected a semi-colon.");

            return new FieldDeclarationNode(accessModifier.Value, name, type);
        });

    private MethodDeclarationNode? TryParseMethod(ParserContext context)
    {
        var accessModifier = TryParseAccessModifier(context);
        if (accessModifier is null)
            return null;

        var name = TryParseId(context);
        if (name is null)
            throw new ParseException("Expected a method name.");

        var parameters = ParseFunctionParameters(context);

        if (!context.Reader.Check(TokenKind.Colon))
            throw new ParseException("Expected a colon.");

        var returnType = TryParseInlineTypeNode(context);
        if (returnType is null)
            throw new ParseException("Expected a function return type.");

        var block = TryParseBlock(context);
        if (block is null)
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

        var name = TryParseId(context);
        if (name is null)
            throw new ParseException("Expected a variable name.");

        if (!context.Reader.Check(TokenKind.Colon))
            throw new ParseException("Expected a colon.");

        var type = TryParseInlineTypeNode(context);
        if (type is null)
            throw new ParseException("Expected a type.");

        if (!context.Reader.Check(TokenKind.Equal))
            throw new ParseException("Expected an equal sign.");

        var expression = TryParseExpression(context);
        if (expression is null)
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

        var condition = TryParseExpression(context);
        if (condition is null)
            throw new ParseException("Expected an expression.");

        if (!context.Reader.Check(TokenKind.CloseParenthesis))
            throw new ParseException("Expected a close parenthesis.");

        var then = TryParseBlock(context);
        if (then is null)
            throw new ParseException("Expected a 'then' block.");

        if (!context.Reader.Check(TokenKind.Else))
            return new IfStatementNode(condition, then);

        var @else = TryParseBlock(context);
        if (@else is null)
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

        var condition = TryParseExpression(context);
        if (condition is null)
            throw new ParseException("Expected a condition.");

        if (!context.Reader.Check(TokenKind.CloseParenthesis))
            throw new ParseException("Expected a close parenthesis.");

        var block = TryParseBlock(context);
        if (block is null)
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
            var right = TryParseBinaryExpression(context, precedence);
            if (right is null)
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

        return TryParseOperand(context);
    }

    private IExpressionNode? TryParseOperand(ParserContext context)
        => TryParseParenExpression(context) ??
           TryParseCallExpression(context) ??
           TryParseArrayAccessExpression(context) ??
           TryParseMemberExpression(context) ??
           TryParseNewExpression(context) ??
           TryParseLiteral(context) as IExpressionNode;

    private IExpressionNode? TryParseParenExpression(ParserContext context)
    {
        if (!context.Reader.Check(TokenKind.OpenParenthesis))
            return null;

        var expression = TryParseExpression(context);
        if (expression is null)
            throw new ParseException("Expected an expression.");

        if (!context.Reader.Check(TokenKind.CloseParenthesis))
            throw new ParseException("Expected a close parenthesis.");

        return expression;
    }

    private IExpressionNode? TryParseCallExpression(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var memberExpression = c.Parser.TryParseMemberExpression(c);
            if (memberExpression is null)
                return null;

            if (!c.Reader.Check(TokenKind.OpenParenthesis))
                return null;

            var arguments = new List<IExpressionNode>();
            var argument = c.Parser.TryParseExpression(c);
            if (argument is not null)
            {
                arguments.Add(argument);

                while (c.Reader.Check(TokenKind.Comma))
                {
                    argument = c.Parser.TryParseExpression(c) ??
                               throw new ParseException("Expected an argument.");

                    arguments.Add(argument);
                }
            }

            if (!c.Reader.Check(TokenKind.CloseParenthesis))
                throw new ParseException("Expected a close parenthesis.");

            return new CallExpressionNode(memberExpression, arguments);
        });

    private IExpressionNode? TryParseArrayAccessExpression(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var memberExpression = c.Parser.TryParseMemberExpression(c);
            if (memberExpression is null)
                return null;

            if (!c.Reader.Check(TokenKind.OpenBracket))
                return null;

            var index = c.Parser.TryParseExpression(c) ??
                        throw new ParseException("Expected an index.");

            if (!c.Reader.Check(TokenKind.CloseBracket))
                throw new ParseException("Expected a close bracket.");

            return new ArrayAccessExpressionNode(memberExpression, index);
        });

    private MemberAccessExpressionNode? TryParseMemberExpression(ParserContext context)
    {
        var name = TryParseId(context);
        if (name is null)
        {
            if (!context.Reader.Check(TokenKind.This))
                return null;

            name = MemberAccessExpressionNode.This;
        }

        var member = new MemberAccessExpressionNode(name);

        while (context.Reader.Check(TokenKind.Dot))
        {
            name = TryParseId(context) ??
                   throw new ParseException("Expected an identifier.");

            member = new MemberAccessExpressionNode(member, name);
        }

        return member;
    }

    private NewExpressionNode? TryParseNewExpression(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            if (!c.Reader.Check(TokenKind.New))
                return null;

            var type = c.Parser.TryParseTypeNode(c) ??
                       throw new ParseException("Expected a type.");

            if (!c.Reader.Check(TokenKind.OpenParenthesis))
                return null;

            var arguments = new List<IExpressionNode>();
            var argument = c.Parser.TryParseExpression(c);
            if (argument is not null)
            {
                arguments.Add(argument);

                while (c.Reader.Check(TokenKind.Comma))
                {
                    argument = c.Parser.TryParseExpression(c) ??
                               throw new ParseException("Expected an argument.");

                    arguments.Add(argument);
                }
            }

            if (!c.Reader.Check(TokenKind.CloseParenthesis))
                throw new ParseException("Expected a close parenthesis.");

            return new NewExpressionNode(type, arguments);
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

    private IInlineTypeNode? TryParseInlineTypeNode(ParserContext context)
        => TryParseTypeNode(context) ??
           TryParseFunctionType(context) ??
           TryParseInterface(context) as IInlineTypeNode;

    private TypeNode? TryParseTypeNode(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            if (!c.Reader.Check(TokenKind.Identifier, out var token))
                return null;

            if (!c.Reader.Check(TokenKind.OpenBracket))
                return new TypeNode(token.Identifier);

            if (!c.Reader.Check(TokenKind.CloseBracket))
                throw new ParseException("Expected a close bracket.");

            return new TypeNode($"{token.Identifier}[]");
        });

    private FunctionTypeNode? TryParseFunctionType(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var parameters = c.Parser.TryParseFunctionTypeParameters(c);
            if (parameters is null)
                return null;

            if (!c.Reader.Check(TokenKind.EqualGreater))
                throw new ParseException("Expected an arrow function.");

            var returnType = c.Parser.TryParseInlineTypeNode(c);
            if (returnType is null)
                throw new ParseException("Expected a function return type.");

            return new FunctionTypeNode(parameters, returnType);
        });

    private IReadOnlyList<IInlineTypeNode>? TryParseFunctionTypeParameters(ParserContext context)
    {
        if (!context.Reader.Check(TokenKind.OpenParenthesis))
            return null;

        var parameters = new List<IInlineTypeNode>();
        var parameter = TryParseInlineTypeNode(context);
        if (parameter is not null)
        {
            parameters.Add(parameter);

            while (context.Reader.Check(TokenKind.Comma))
            {
                parameter = TryParseInlineTypeNode(context);
                if (parameter is null)
                    throw new ParseException("Expected a parameter.");

                parameters.Add(parameter);
            }
        }

        if (!context.Reader.Check(TokenKind.CloseParenthesis))
            throw new ParseException("Expected a close parenthesis.");

        return parameters;
    }

    private InterfaceNode? TryParseInterface(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            if (!c.Reader.Check(TokenKind.OpenBrace))
                return null;

            var fields = c.Parser.TryParseInterfaceFields(c);
            var methods = c.Parser.TryParseInterfaceMethods(c);

            if (!c.Reader.Check(TokenKind.CloseBrace))
                throw new ParseException("Expected a close brace.");

            return new InterfaceNode(fields, methods);
        });

    private List<InterfaceFieldNode> TryParseInterfaceFields(ParserContext context)
    {
        var fields = new List<InterfaceFieldNode>();

        while (true)
        {
            var field = TryParseInterfaceField(context);

            if (field is null)
                break;

            fields.Add(field);
        }

        return fields;
    }

    private InterfaceFieldNode? TryParseInterfaceField(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var name = c.Parser.TryParseId(c);
            if (name is null)
                return null;

            if (!c.Reader.Check(TokenKind.Colon))
                return null;

            var type = c.Parser.TryParseInlineTypeNode(c);
            if (type is null)
                throw new ParseException("Expected a type.");

            if (!c.Reader.Check(TokenKind.SemiColon))
                throw new ParseException("Expected a semi-colon.");

            return new InterfaceFieldNode(name, type);
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

            var parameters = c.Parser.ParseFunctionParameters(c);

            if (!c.Reader.Check(TokenKind.Colon))
                throw new ParseException("Expected a colon.");

            var returnType = c.Parser.TryParseInlineTypeNode(c);
            if (returnType is null)
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
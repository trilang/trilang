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
        var functions = new List<FunctionDeclarationNode>();

        while (!context.Reader.HasEnded)
        {
            var function = TryParseFunction(context);
            if (function is null)
                throw new ParseException("Expected a function.");

            functions.Add(function);
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

        var returnType = TryParseTypeNode(context);
        if (returnType is null)
            throw new ParseException("Expected a function return type.");

        var block = TryParseBlock(context);
        if (block is null)
            throw new ParseException("Expected a function block.");

        return FunctionDeclarationNode.Create(name, parameters, returnType, block);
    }

    private List<FunctionParameterNode> ParseFunctionParameters(ParserContext context)
    {
        if (!context.Reader.Check(TokenKind.OpenParenthesis))
            throw new ParseException("Expected an open parenthesis.");

        var parameters = new List<FunctionParameterNode>();

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

    private FunctionParameterNode? TryParseFunctionParameter(ParserContext context)
    {
        var name = TryParseId(context);
        if (name is null)
            return null;

        if (!context.Reader.Check(TokenKind.Colon))
            throw new ParseException("Expected a colon.");

        var type = TryParseTypeNode(context);
        if (type is null)
            throw new ParseException("Expected a type.");

        return new FunctionParameterNode(name, type);
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

        var type = TryParseTypeNode(context);
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
        if (expression is null)
            throw new ParseException("Expected an expression.");

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
           TryParseLiteral(context);

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
            var name = c.Parser.TryParseId(c);
            if (name is null)
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

            return new CallExpressionNode(new MemberAccessExpressionNode(name), arguments);
        });

    private IExpressionNode? TryParseArrayAccessExpression(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var name = c.Parser.TryParseId(c);
            if (name is null)
                return null;

            if (!c.Reader.Check(TokenKind.OpenBracket))
                return null;

            var index = c.Parser.TryParseExpression(c) ??
                        throw new ParseException("Expected an index.");

            if (!c.Reader.Check(TokenKind.CloseBracket))
                throw new ParseException("Expected a close bracket.");

            return new ArrayAccessExpressionNode(new MemberAccessExpressionNode(name), index);
        });

    private IExpressionNode? TryParseMemberExpression(ParserContext context)
    {
        var name = TryParseId(context);
        if (name is null)
            return null;

        return new MemberAccessExpressionNode(name);
    }

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

    private TypeNode? TryParseTypeNode(ParserContext context)
    {
        if (!context.Reader.Check(TokenKind.Identifier, out var token))
            return null;

        if (!context.Reader.Check(TokenKind.OpenBracket))
            return TypeNode.Create(token.Identifier);

        if (!context.Reader.Check(TokenKind.CloseBracket))
            throw new ParseException("Expected a close bracket.");

        return TypeNode.Array(token.Identifier);
    }

    private string? TryParseId(ParserContext context)
    {
        if (!context.Reader.Check(TokenKind.Identifier, out var token))
            return null;

        return token.Identifier;
    }
}
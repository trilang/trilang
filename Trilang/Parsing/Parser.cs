using System.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing.Ast;
using static Trilang.Lexing.TokenKind;

namespace Trilang.Parsing;

public class Parser
{
    private int nameCounter;

    public Parser()
        => nameCounter = 0;

    public SyntaxTree Parse(IReadOnlyList<Token> tokens, ParserOptions options)
    {
        var context = new ParserContext(tokens, options.Diagnostics, this);
        var namespaceNode = TryParseNamespaceNode(context);
        var useNodes = ParseUseNodes(context);
        var declarations = ParseDeclarations(context);

        return new SyntaxTree(
            options.SourceFile,
            namespaceNode,
            useNodes,
            declarations);
    }

    private string GetGeneratedName()
        => $"<>_{nameCounter++}";

    private IReadOnlyList<UseNode> ParseUseNodes(ParserContext context)
    {
        var useNodes = new List<UseNode>();

        while (!context.Reader.HasEnded)
        {
            var useNode = ParseUseNode(context);
            if (useNode is null)
                break;

            useNodes.Add(useNode);
        }

        return useNodes;
    }

    private UseNode? ParseUseNode(ParserContext context)
    {
        var (hasUse, useKeyword) = context.Reader.Check(Use);
        if (!hasUse)
            return null;

        var parts = new List<string>
        {
            ParseNamespacePart(context)
        };

        while (context.Reader.Check(Dot))
            parts.Add(ParseNamespacePart(context));

        var semiColonSpan = context.Reader.Expect(SemiColon);

        return new UseNode(useKeyword.SourceSpan.Combine(semiColonSpan), parts);
    }

    private NamespaceNode? TryParseNamespaceNode(ParserContext context)
    {
        var (hasNamespace, namespaceToken) = context.Reader.Check(Namespace);
        if (!hasNamespace)
            return null;

        var parts = new List<string>
        {
            ParseNamespacePart(context)
        };

        while (context.Reader.Check(Dot))
            parts.Add(ParseNamespacePart(context));

        var semiColonSpan = context.Reader.Expect(SemiColon);

        return new NamespaceNode(namespaceToken.SourceSpan.Combine(semiColonSpan), parts);
    }

    private string ParseNamespacePart(ParserContext context)
    {
        var (_, part) = TryParseId(context);
        if (part is null)
            context.Diagnostics.ExpectedNamespacePart(
                context.Reader.SkipTo(Dot, SemiColon));

        return part ?? "<namespace>";
    }

    private IReadOnlyList<IDeclarationNode> ParseDeclarations(ParserContext context)
    {
        var declarations = new List<IDeclarationNode>();

        while (!context.Reader.HasEnded)
        {
            var declaration = TryParseDeclaration(context) ??
                              ParseFakeDeclaration(context, Public, Internal, Private);

            declarations.Add(declaration);
        }

        return declarations;
    }

    private IDeclarationNode? TryParseDeclaration(ParserContext context)
        => TryParseFunction(context) ??
           TryParseTypeDeclarationOrTypeAliasNode(context) ??
           TryParseTopLevelIfDirective(context);

    private IfDirectiveNode? TryParseTopLevelIfDirective(ParserContext context)
    {
        var (hasHash, hash) = context.Reader.Check(Hash);
        if (!hasHash)
            return null;

        context.Reader.Expect(If);

        var (_, name) = TryParseId(context);
        if (name is null)
        {
            name = GetGeneratedName();
            context.Diagnostics.ExpectedDirectiveName(context.Reader.Span.Start);
        }

        var then = new List<IDeclarationNode>();
        var @else = new List<IDeclarationNode>();

        while (!context.Reader.HasEnded && !IsEndDirective(context, Else, EndIf))
        {
            var declaration = TryParseDeclaration(context) ??
                              ParseFakeDeclaration(context, Public, Internal, Private, Hash);

            then.Add(declaration);
        }

        context.Reader.Expect(Hash);

        if (context.Reader.Check(Else))
        {
            while (!context.Reader.HasEnded && !IsEndDirective(context, EndIf))
            {
                var declaration = TryParseDeclaration(context) ??
                                  ParseFakeDeclaration(context, Public, Internal, Private, Hash);

                @else.Add(declaration);
            }

            context.Reader.Expect(Hash);
        }

        var endIfSpan = context.Reader.Expect(EndIf);

        return new IfDirectiveNode(hash.SourceSpan.Combine(endIfSpan), name, then, @else);
    }

    private bool IsEndDirective(ParserContext context, params Span<TokenKind> keywords)
    {
        Debug.Assert(keywords.Length > 0, "At least one keyword must be provided");

        if (!context.Reader.Token.Is(Hash))
            return false;

        foreach (var keyword in keywords)
            if (context.Reader.Peek().Is(keyword))
                return true;

        return false;
    }

    private FakeDeclarationNode ParseFakeDeclaration(
        ParserContext context,
        params Span<TokenKind> tokenKinds)
    {
        var span = context.Reader.SkipTo(tokenKinds);
        context.Diagnostics.ExpectedDeclaration(span);

        return new FakeDeclarationNode(span);
    }

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

            c.Reader.Expect(Colon);

            var returnType = c.Parser.TryParseDiscriminatedUnion(c) ??
                             c.Parser.ParseFakeType(c, OpenBrace, CloseBrace);

            var block = c.Parser.ParseBlock(c);

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
        context.Reader.Expect(OpenParen);

        var parameters = new List<ParameterNode>();
        var parameter = TryParseFunctionParameter(context);
        if (parameter is not null)
        {
            parameters.Add(parameter);

            while (true)
            {
                var hasComma = context.Reader.Check(Comma);
                var comma = context.Reader.Token;

                parameter = TryParseFunctionParameter(context);
                if (parameter is null)
                {
                    if (hasComma)
                    {
                        var span = context.Reader.SkipTo(CloseParen, OpenBrace);
                        context.Diagnostics.ExpectedParameter(span);
                    }

                    break;
                }

                if (!hasComma)
                    context.Diagnostics.MissingToken(comma.SourceSpan.Start, Comma);

                parameters.Add(parameter);
            }
        }

        context.Reader.Expect(CloseParen);

        return parameters;
    }

    private ParameterNode? TryParseFunctionParameter(ParserContext context)
    {
        var (span, name) = TryParseId(context);
        if (name is null)
            return null;

        context.Reader.Expect(Colon);

        var type = TryParseDiscriminatedUnion(context) ??
                   ParseFakeType(context, Comma, CloseParen);

        return new ParameterNode(span.Combine(type.SourceSpan), name, type);
    }

    private BlockStatementNode ParseBlock(ParserContext context)
    {
        var openBraceSpan = context.Reader.Expect(OpenBrace);

        return ParseBlockInternal(context, openBraceSpan);
    }

    private BlockStatementNode? TryParseBlock(ParserContext context)
    {
        var (hasOpenBrace, openBrace) = context.Reader.Check(OpenBrace);
        if (!hasOpenBrace)
            return null;

        return ParseBlockInternal(context, openBrace.SourceSpan);
    }

    private BlockStatementNode ParseBlockInternal(ParserContext context, SourceSpan openBraceSpan)
    {
        var closeBraceSpan = default(SourcePosition);

        var statements = new List<IStatementNode>();
        while (true)
        {
            if (context.Reader.HasEnded)
            {
                closeBraceSpan = context.Reader.Span.Start;
                context.Diagnostics.MissingToken(closeBraceSpan, CloseBrace);
                break;
            }

            var (hasCloseBrace, closeBrace) = context.Reader.Check(CloseBrace);
            if (hasCloseBrace)
            {
                closeBraceSpan = closeBrace.SourceSpan.End;
                break;
            }

            var statement = TryParseStatement(context) ??
                            ParseFakeStatement(context, CloseBrace);

            statements.Add(statement);
        }

        return new BlockStatementNode(openBraceSpan.Combine(closeBraceSpan), statements);
    }

    private (SourceSpan, AccessModifier?) TryParseAccessModifier(ParserContext context)
    {
        var (hasPublic, publicKeyword) = context.Reader.Check(Public);
        if (hasPublic)
            return (publicKeyword.SourceSpan, AccessModifier.Public);

        var (hasInternal, internalKeyword) = context.Reader.Check(Internal);
        if (hasInternal)
            return (internalKeyword.SourceSpan, AccessModifier.Internal);

        var (hasPrivate, privateKeyword) = context.Reader.Check(Private);
        if (hasPrivate)
            return (privateKeyword.SourceSpan, AccessModifier.Private);

        return (default, null);
    }

    private IDeclarationNode? TryParseTypeDeclarationOrTypeAliasNode(ParserContext context)
    {
        var (accessModifierSpan, accessModifier) = TryParseAccessModifier(context);
        if (accessModifier is null)
            return null;

        context.Reader.Expect(TokenKind.Type);

        var (_, name) = TryParseId(context);
        if (name is null)
        {
            name = GetGeneratedName();
            context.Diagnostics.ExpectedTypeName(context.Reader.Span.Start);
        }

        var genericArguments = TryParseGenericTypeArguments(context);
        if (context.Reader.Check(Equal))
        {
            var type = TryParseDiscriminatedUnion(context) ??
                       ParseFakeType(context, SemiColon);

            if (type is InterfaceNode)
                return new AliasDeclarationNode(
                    accessModifierSpan.Combine(type.SourceSpan),
                    accessModifier.Value,
                    name,
                    genericArguments,
                    type);

            var endSpan = context.Reader.Expect(SemiColon);

            return new AliasDeclarationNode(
                accessModifierSpan.Combine(endSpan),
                accessModifier.Value,
                name,
                genericArguments,
                type);
        }

        var interfaces = new List<TypeRefNode>();
        if (context.Reader.Check(Colon))
        {
            var @interface = TryParseTypeNode(context);
            if (@interface is null)
            {
                context.Diagnostics.ExpectedInterface(context.Reader.Span.Start);
            }
            else
            {
                interfaces.Add(@interface);
            }

            while (true)
            {
                var hasComma = context.Reader.Check(Comma);
                var comma = context.Reader.Token;

                @interface = TryParseTypeNode(context);
                if (@interface is null)
                {
                    if (hasComma)
                        context.Diagnostics.ExpectedInterface(context.Reader.Span.Start);

                    break;
                }

                if (!hasComma)
                    context.Diagnostics.MissingToken(comma.SourceSpan.Start, Comma);

                interfaces.Add(@interface);
            }
        }

        context.Reader.Expect(OpenBrace);

        var properties = new List<PropertyDeclarationNode>();
        var constructors = new List<ConstructorDeclarationNode>();
        var methods = new List<MethodDeclarationNode>();

        var closeBraceSpan = default(SourceSpan);
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

            var (hasCloseBrace, closeBrace) = context.Reader.Check(CloseBrace);
            if (!hasCloseBrace)
            {
                var span = context.Reader.SkipTo(CloseBrace);
                context.Diagnostics.ExpectedTypeMember(span);

                context.Reader.Advance();
                closeBraceSpan = context.Reader.Token.SourceSpan;
            }
            else
            {
                closeBraceSpan = closeBrace.SourceSpan;
            }

            break;
        }

        return new TypeDeclarationNode(
            accessModifierSpan.Combine(closeBraceSpan),
            accessModifier.Value,
            name,
            genericArguments,
            interfaces,
            properties,
            constructors,
            methods);
    }

    private List<IInlineTypeNode> TryParseGenericTypeArguments(ParserContext context)
    {
        var arguments = new List<IInlineTypeNode>();

        if (!context.Reader.Check(Less))
            return arguments;

        var type = TryParseTypeNode(context) as IInlineTypeNode ??
                   ParseFakeType(context, Comma, Greater);

        arguments.Add(type);

        while (true)
        {
            var hasComma = context.Reader.Check(Comma);
            var comma = context.Reader.Token;

            type = TryParseTypeNode(context);
            if (type is null)
            {
                if (!hasComma)
                    break;

                type = ParseFakeType(context, Comma, Greater);
            }

            if (!hasComma)
                context.Diagnostics.MissingToken(comma.SourceSpan.Start, Comma);

            arguments.Add(type);
        }

        context.Reader.Expect(Greater);

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

            var block = c.Parser.ParseBlock(c);

            return new ConstructorDeclarationNode(
                span.Combine(block.SourceSpan),
                accessModifier.Value,
                parameters,
                block);
        });

    private PropertyDeclarationNode? TryParseProperty(ParserContext context)
    {
        var (span, name) = TryParseId(context);
        if (name is null)
            return null;

        context.Reader.Expect(Colon);

        var type = TryParseDiscriminatedUnion(context) ??
                   ParseFakeType(context, SemiColon, OpenBrace);

        var (hasSemicolon, semiColon) = context.Reader.Check(SemiColon);
        if (hasSemicolon)
            return new PropertyDeclarationNode(span.Combine(semiColon.SourceSpan), name, type);

        if (!context.Reader.Check(OpenBrace))
        {
            var semiColonSpan = context.Reader.Span.Start;
            context.Diagnostics.MissingToken(semiColonSpan, SemiColon);

            return new PropertyDeclarationNode(span.Combine(semiColonSpan), name, type);
        }

        var getter = TryParsePropertyGetter(context);
        var setter = TryParsePropertySetter(context);

        var closeBraceSpan = context.Reader.Expect(CloseBrace);

        return new PropertyDeclarationNode(
            span.Combine(closeBraceSpan),
            name,
            type,
            getter,
            setter);
    }

    private PropertyGetterNode? TryParsePropertyGetter(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var (span, accessModifier) = c.Parser.TryParseAccessModifier(c);
            if (accessModifier is null)
                return null;

            if (!c.Reader.Check(Get))
                return null;

            var (hasSemicolon, semiColon) = c.Reader.Check(SemiColon);
            if (hasSemicolon)
                return new PropertyGetterNode(
                    span.Combine(semiColon.SourceSpan),
                    accessModifier.Value,
                    null);

            var body = c.Parser.ParseBlock(c);

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

            var (hasSemicolon, semiColon) = c.Reader.Check(SemiColon);
            if (hasSemicolon)
                return new PropertySetterNode(
                    span.Combine(semiColon.SourceSpan),
                    accessModifier.Value,
                    null);

            var body = c.Parser.ParseBlock(c);

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
        {
            name = GetGeneratedName();
            context.Diagnostics.ExpectedMethodName(context.Reader.Span.Start);
        }

        var parameters = ParseFunctionParameters(context);

        context.Reader.Expect(Colon);

        var returnType = TryParseDiscriminatedUnion(context) ??
                         ParseFakeType(context, OpenBrace);

        var block = ParseBlock(context);

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
           TryParseWhileStatement(context) ??
           TryParseBreakStatement(context) ??
           TryParseContinueStatement(context) ??
           TryParseStatementLevelIfDirective(context) ??
           TryParseExpressionStatement(context) as IStatementNode;

    private IfDirectiveNode? TryParseStatementLevelIfDirective(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var (hasHash, hash) = c.Reader.Check(Hash);
            if (!hasHash)
                return null;

            if (!c.Reader.Check(If))
                return null;

            var (_, name) = c.Parser.TryParseId(c);
            if (name is null)
            {
                name = c.Parser.GetGeneratedName();
                c.Diagnostics.ExpectedDirectiveName(c.Reader.Span.Start);
            }

            var then = new List<IStatementNode>();
            var @else = new List<IStatementNode>();

            while (!c.Reader.HasEnded && !c.Reader.Check(Hash))
            {
                var declaration = c.Parser.TryParseStatement(c) ??
                                  c.Parser.ParseFakeStatement(c, Hash);

                then.Add(declaration);
            }

            if (c.Reader.Check(Else))
            {
                while (!c.Reader.HasEnded && !c.Reader.Check(Hash))
                {
                    var declaration = c.Parser.TryParseStatement(c) ??
                                      c.Parser.ParseFakeStatement(c, Hash);

                    @else.Add(declaration);
                }
            }

            var endIfSpan = c.Reader.Expect(EndIf);

            return new IfDirectiveNode(hash.SourceSpan.Combine(endIfSpan), name, then, @else);
        });

    private VariableDeclarationNode? TryParseVariableStatement(ParserContext context)
    {
        var (hasVar, varKeyword) = context.Reader.Check(Var);
        if (!hasVar)
            return null;

        var (_, name) = TryParseId(context);
        if (name is null)
        {
            name = GetGeneratedName();
            context.Diagnostics.ExpectedVariableName(context.Reader.Span.Start);
        }

        context.Reader.Expect(Colon);

        var type = TryParseDiscriminatedUnion(context) ??
                   ParseFakeType(context, Equal, SemiColon);

        context.Reader.Expect(Equal);

        var expression = TryParseExpression(context) ??
                         ParseFakeExpression(context, SemiColon);

        var semiColonSpan = context.Reader.Expect(SemiColon);

        return new VariableDeclarationNode(
            varKeyword.SourceSpan.Combine(semiColonSpan),
            name,
            type,
            expression);
    }

    private IfStatementNode? TryParseIfStatement(ParserContext context)
    {
        var (hasIf, ifKeyword) = context.Reader.Check(If);
        if (!hasIf)
            return null;

        context.Reader.Expect(OpenParen);

        var condition = TryParseExpression(context) ??
                        ParseFakeExpression(context, CloseParen, OpenBrace);

        context.Reader.Expect(CloseParen);

        var then = ParseBlock(context);

        if (!context.Reader.Check(Else))
            return new IfStatementNode(ifKeyword.SourceSpan.Combine(then.SourceSpan), condition, then);

        var @else = ParseBlock(context);

        return new IfStatementNode(
            ifKeyword.SourceSpan.Combine(@else.SourceSpan),
            condition,
            then,
            @else);
    }

    private ReturnStatementNode? TryParseReturnStatement(ParserContext context)
    {
        var (hasReturn, returnToken) = context.Reader.Check(Return);
        if (!hasReturn)
            return null;

        var expression = TryParseExpression(context);
        var semiColonSpan = context.Reader.Expect(SemiColon);

        return new ReturnStatementNode(
            returnToken.SourceSpan.Combine(semiColonSpan),
            expression);
    }

    private WhileNode? TryParseWhileStatement(ParserContext context)
    {
        var (hasWhile, whileToken) = context.Reader.Check(While);
        if (!hasWhile)
            return null;

        context.Reader.Expect(OpenParen);

        var condition = TryParseExpression(context) ??
                        ParseFakeExpression(context, CloseParen, OpenBrace);

        context.Reader.Expect(CloseParen);

        var block = ParseBlock(context);

        return new WhileNode(whileToken.SourceSpan.Combine(block.SourceSpan), condition, block);
    }

    private BreakNode? TryParseBreakStatement(ParserContext context)
    {
        var (hasBreak, breakToken) = context.Reader.Check(Break);
        if (!hasBreak)
            return null;

        var semiColonSpan = context.Reader.Expect(SemiColon);

        return new BreakNode(breakToken.SourceSpan.Combine(semiColonSpan));
    }

    private ContinueNode? TryParseContinueStatement(ParserContext context)
    {
        var (hasContinue, continueToken) = context.Reader.Check(Continue);
        if (!hasContinue)
            return null;

        var semiColonSpan = context.Reader.Expect(SemiColon);

        return new ContinueNode(continueToken.SourceSpan.Combine(semiColonSpan));
    }

    private ExpressionStatementNode? TryParseExpressionStatement(ParserContext context)
    {
        var expression = TryParseExpression(context);
        if (expression is null)
            return null;

        var semiColonSpan = context.Reader.Expect(SemiColon);

        return new ExpressionStatementNode(
            expression.SourceSpan.Combine(semiColonSpan),
            expression);
    }

    private FakeStatementNode ParseFakeStatement(
        ParserContext context,
        params Span<TokenKind> tokenKinds)
    {
        var span = context.Reader.SkipTo([SemiColon, .. tokenKinds]);
        context.Reader.Check(SemiColon);
        context.Diagnostics.ExpectedStatement(span);

        return new FakeStatementNode(span);
    }

    private IExpressionNode? TryParseExpression(ParserContext context)
        => TryParseBinaryExpression(context);

    private FakeExpressionNode ParseFakeExpression(
        ParserContext context,
        params Span<TokenKind> tokenKinds)
    {
        var span = context.Reader.SkipTo(tokenKinds);
        context.Diagnostics.ExpectedExpression(span);

        return new FakeExpressionNode(span);
    }

    private IExpressionNode? TryParseBinaryExpression(ParserContext context, int parentPrecedence = 0)
    {
        var left = TryParseIsExpression(context);
        if (left is null)
            return null;

        while (true)
        {
            var kind = BinaryExpressionKind.Unknown;
            if (context.Reader.Token.Is(Plus))
                kind = BinaryExpressionKind.Addition;
            else if (context.Reader.Token.Is(Minus))
                kind = BinaryExpressionKind.Subtraction;
            else if (context.Reader.Token.Is(Asterisk))
                kind = BinaryExpressionKind.Multiplication;
            else if (context.Reader.Token.Is(Slash))
                kind = BinaryExpressionKind.Division;
            else if (context.Reader.Token.Is(Ampersand))
                kind = BinaryExpressionKind.BitwiseAnd;
            else if (context.Reader.Token.Is(Percent))
                kind = BinaryExpressionKind.Modulus;
            else if (context.Reader.Token.Is(Pipe))
                kind = BinaryExpressionKind.BitwiseOr;
            else if (context.Reader.Token.Is(Caret))
                kind = BinaryExpressionKind.BitwiseXor;
            else if (context.Reader.Token.Is(AmpersandAmpersand))
                kind = BinaryExpressionKind.ConditionalAnd;
            else if (context.Reader.Token.Is(PipePipe))
                kind = BinaryExpressionKind.ConditionalOr;
            else if (context.Reader.Token.Is(EqualEqual))
                kind = BinaryExpressionKind.Equality;
            else if (context.Reader.Token.Is(ExclamationEqual))
                kind = BinaryExpressionKind.Inequality;
            else if (context.Reader.Token.Is(Less))
                kind = BinaryExpressionKind.LessThan;
            else if (context.Reader.Token.Is(LessEqual))
                kind = BinaryExpressionKind.LessThanOrEqual;
            else if (context.Reader.Token.Is(Greater))
                kind = BinaryExpressionKind.GreaterThan;
            else if (context.Reader.Token.Is(GreaterEqual))
                kind = BinaryExpressionKind.GreaterThanOrEqual;
            else if (context.Reader.Token.Is(Equal))
                kind = BinaryExpressionKind.Assignment;
            else if (context.Reader.Token.Is(PlusEqual))
                kind = BinaryExpressionKind.AdditionAssignment;
            else if (context.Reader.Token.Is(MinusEqual))
                kind = BinaryExpressionKind.SubtractionAssignment;
            else if (context.Reader.Token.Is(AsteriskEqual))
                kind = BinaryExpressionKind.MultiplicationAssignment;
            else if (context.Reader.Token.Is(SlashEqual))
                kind = BinaryExpressionKind.DivisionAssignment;
            else if (context.Reader.Token.Is(PercentEqual))
                kind = BinaryExpressionKind.ModulusAssignment;
            else if (context.Reader.Token.Is(AmpersandEqual))
                kind = BinaryExpressionKind.BitwiseAndAssignment;
            else if (context.Reader.Token.Is(PipeEqual))
                kind = BinaryExpressionKind.BitwiseOrAssignment;
            else if (context.Reader.Token.Is(CaretEqual))
                kind = BinaryExpressionKind.BitwiseXorAssignment;
            else
                break;

            var precedence = kind.GetPrecedence();
            if (precedence <= parentPrecedence)
                break;

            context.Reader.Advance();
            var right = TryParseBinaryExpression(context, precedence) ??
                        ParseFakeExpression(context, Comma, CloseParen, CloseBracket, SemiColon);

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
                   ParseFakeType(context, Comma, CloseParen, SemiColon);

        return new IsExpressionNode(expression, type);
    }

    private IExpressionNode? TryParseCastExpression(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var (hasOpenParen, openParen) = c.Reader.Check(OpenParen);
            if (!hasOpenParen)
                return null;

            var type = c.Parser.TryParseDiscriminatedUnion(c);
            if (type is null)
                return null;

            c.Reader.Expect(CloseParen);

            var expression = c.Parser.TryParseUnaryExpression(c) ??
                             c.Parser.ParseFakeExpression(c, Comma, CloseParen, CloseBracket, SemiColon);

            return new CastExpressionNode(
                openParen.SourceSpan.Combine(expression.SourceSpan),
                type,
                expression);
        });

    private IExpressionNode? TryParseUnaryExpression(ParserContext context)
    {
        var token = context.Reader.Token;
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
                          ParseFakeExpression(context, Comma, CloseParen, CloseBracket, SemiColon);

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
                var (hasId, id) = context.Reader.Check(Identifier);
                if (hasId)
                {
                    member = new MemberAccessExpressionNode(
                        member.SourceSpan.Combine(id.SourceSpan),
                        member,
                        id.Identifier);
                }
                else
                {
                    var (hasInteger, number) = context.Reader.Check(Integer);
                    if (hasInteger)
                    {
                        member = new MemberAccessExpressionNode(
                            member.SourceSpan.Combine(number.SourceSpan),
                            member,
                            number.Integer.ToString());
                    }
                    else
                    {
                        member = new FakeExpressionNode(context.Reader.Token.SourceSpan);
                        context.Diagnostics.ExpectedIdentifier(member.SourceSpan);
                    }
                }
            }
            else if (context.Reader.Check(OpenBracket))
            {
                var index = TryParseExpression(context) ??
                            ParseFakeExpression(context, CloseBracket);

                var closeBracketSpan = context.Reader.Expect(CloseBracket);

                member = new ArrayAccessExpressionNode(
                    member.SourceSpan.Combine(closeBracketSpan),
                    member,
                    index);
            }
            else if (context.Reader.Check(OpenParen))
            {
                var arguments = TryParseCallArguments(context);
                var closeParenSpan = context.Reader.Expect(CloseParen);

                member = new CallExpressionNode(
                    member.SourceSpan.Combine(closeParenSpan),
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

            while (true)
            {
                var hasComma = context.Reader.Check(Comma);
                var comma = context.Reader.Token;

                argument = TryParseExpression(context);
                if (argument is null)
                {
                    if (!hasComma)
                        break;

                    argument = ParseFakeExpression(context, Comma, CloseParen);
                }

                if (!hasComma)
                    context.Diagnostics.MissingToken(comma.SourceSpan.Start, Comma);

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
            var (hasOpenParen, openParen) = c.Reader.Check(OpenParen);
            if (!hasOpenParen)
                return null;

            var expression = c.Parser.TryParseExpression(c);
            if (expression is null)
                return null;

            var expressions = new List<IExpressionNode> { expression };
            while (true)
            {
                var hasComma = c.Reader.Check(Comma);
                var comma = c.Reader.Token;

                expression = c.Parser.TryParseExpression(c);
                if (expression is null)
                {
                    if (!hasComma)
                        break;

                    expression = c.Parser.ParseFakeExpression(c, Comma, CloseParen);
                }

                if (!hasComma)
                    c.Diagnostics.MissingToken(comma.SourceSpan.Start, Comma);

                expressions.Add(expression);
            }

            if (expressions.Count <= 1)
                return null;

            var closeParenSpan = c.Reader.Expect(CloseParen);

            return new TupleExpressionNode(
                openParen.SourceSpan.Combine(closeParenSpan),
                expressions);
        });

    private IExpressionNode? TryParseParenExpression(ParserContext context)
    {
        if (!context.Reader.Check(OpenParen))
            return null;

        var expression = TryParseExpression(context) ??
                         ParseFakeExpression(context, CloseParen);

        context.Reader.Expect(CloseParen);

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
        var (hasNull, nullKeyword) = context.Reader.Check(Null);
        if (!hasNull)
            return null;

        return new NullExpressionNode(nullKeyword.SourceSpan);
    }

    private NewObjectExpressionNode? TryParseNewObjectExpression(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var (hasNew, newKeyword) = c.Reader.Check(New);
            if (!hasNew)
                return null;

            var type = c.Parser.TryParseGenericTypeNode(c) ??
                       c.Parser.TryParseTypeNode(c) ??
                       c.Parser.ParseFakeType(c, OpenParen) as IInlineTypeNode;

            if (!c.Reader.Check(OpenParen))
                return null;

            var arguments = c.Parser.TryParseCallArguments(c);
            var closeParenSpan = c.Reader.Expect(CloseParen);

            return new NewObjectExpressionNode(
                newKeyword.SourceSpan.Combine(closeParenSpan),
                type,
                arguments);
        });

    private NewArrayExpressionNode? TryParseNewArrayExpression(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var (hasNew, newKeyword) = c.Reader.Check(New);
            if (!hasNew)
                return null;

            var type = c.Parser.TryParseTypeNode(c) as IInlineTypeNode ??
                       c.Parser.ParseFakeType(c, OpenBracket, CloseBracket);

            if (!c.Reader.Check(OpenBracket))
                return null;

            var size = c.Parser.TryParseExpression(c) ??
                       c.Parser.ParseFakeExpression(c, CloseBracket);

            var closeBraceSpan = c.Reader.Expect(CloseBracket);

            return new NewArrayExpressionNode(
                newKeyword.SourceSpan.Combine(closeBraceSpan),
                new ArrayTypeNode(type.SourceSpan, type),
                size);
        });

    private LiteralExpressionNode? TryParseLiteral(ParserContext context)
    {
        var (hasInteger, token) = context.Reader.Check(Integer);
        if (hasInteger)
            return LiteralExpressionNode.Integer(token.SourceSpan, token.Integer);

        var (hasFloat, floatToken) = context.Reader.Check(Float);
        if (hasFloat)
            return LiteralExpressionNode.Float(floatToken.SourceSpan, floatToken.Float);

        var (hasTrue, trueToken) = context.Reader.Check(True);
        if (hasTrue)
            return LiteralExpressionNode.True(trueToken.SourceSpan);

        var (hasFalse, falseToken) = context.Reader.Check(False);
        if (hasFalse)
            return LiteralExpressionNode.False(falseToken.SourceSpan);

        var (hasString, stringToken) = context.Reader.Check(TokenKind.String);
        if (hasString)
            return LiteralExpressionNode.String(stringToken.SourceSpan, stringToken.String);

        var (hasChar, charToken) = context.Reader.Check(TokenKind.Char);
        if (hasChar)
            return LiteralExpressionNode.Char(charToken.SourceSpan, charToken.Char);

        return null;
    }

    private FakeTypeNode ParseFakeType(ParserContext context, params Span<TokenKind> tokenKinds)
    {
        var span = context.Reader.SkipTo(tokenKinds);
        context.Diagnostics.ExpectedType(span);

        var name = GetGeneratedName();

        return new FakeTypeNode(span, name);
    }

    private IInlineTypeNode? TryParseDiscriminatedUnion(ParserContext context)
    {
        var hasFirstPipe = context.Reader.Check(Pipe);
        var type = TryParseInlineTypeNode(context);
        if (type is null)
        {
            if (!hasFirstPipe)
                return null;

            type = ParseFakeType(context, Pipe, SemiColon);
        }

        if (!context.Reader.Token.Is(Pipe))
            return type;

        var types = new List<IInlineTypeNode> { type };
        while (context.Reader.Check(Pipe))
        {
            type = TryParseInlineTypeNode(context) ??
                   ParseFakeType(context, Pipe, SemiColon);

            types.Add(type);
        }

        return new DiscriminatedUnionNode(types);
    }

    private IInlineTypeNode? TryParseInlineTypeNode(ParserContext context)
        => TryParseNull(context) ??
           TryParseArrayType(context) ??
           TryParseGenericTypeNode(context) ??
           TryParseTypeNode(context) ??
           TryParseTupleOrFunctionType(context) ??
           TryParseInterface(context);

    private IInlineTypeNode? TryParseNull(ParserContext context)
    {
        var (hasNull, token) = context.Reader.Check(Null);
        if (!hasNull)
            return null;

        return new TypeRefNode(token.SourceSpan, "null");
    }

    private IInlineTypeNode? TryParseArrayType(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var (hasId, id) = c.Reader.Check(Identifier);
            if (!hasId)
                return null;

            if (!c.Reader.Check(OpenBracket))
                return null;

            var closeBracketSpan = c.Reader.Expect(CloseBracket);

            return new ArrayTypeNode(
                id.SourceSpan.Combine(closeBracketSpan),
                new TypeRefNode(id.SourceSpan, id.Identifier));
        });

    private GenericApplicationNode? TryParseGenericTypeNode(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var (hasToken, token) = c.Reader.Check(Identifier);
            if (!hasToken)
                return null;

            if (!c.Reader.Check(Less))
                return null;

            var typeArguments = new List<IInlineTypeNode>();
            var typeArgument = c.Parser.TryParseDiscriminatedUnion(c) ??
                               c.Parser.ParseFakeType(c, Comma, Greater);

            typeArguments.Add(typeArgument);

            while (true)
            {
                var hasComma = c.Reader.Check(Comma);
                var comma = c.Reader.Token;

                typeArgument = c.Parser.TryParseDiscriminatedUnion(c);
                if (typeArgument is null)
                {
                    if (!hasComma)
                        break;

                    typeArgument = c.Parser.ParseFakeType(c, Comma, Greater);
                }

                if (!hasComma)
                    c.Diagnostics.MissingToken(comma.SourceSpan.Start, Comma);

                typeArguments.Add(typeArgument);
            }

            var greaterSignSpan = c.Reader.Expect(Greater);

            return new GenericApplicationNode(
                token.SourceSpan.Combine(greaterSignSpan),
                token.Identifier,
                typeArguments);
        });

    private TypeRefNode? TryParseTypeNode(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var (hasToken, token) = c.Reader.Check(Identifier);
            if (!hasToken)
                return null;

            return new TypeRefNode(token.SourceSpan, token.Identifier);
        });

    private (SourceSpan, IReadOnlyList<IInlineTypeNode>?) TryParseParenTypes(
        ParserContext context)
    {
        var (hasOpenParen, openParen) = context.Reader.Check(OpenParen);
        if (!hasOpenParen)
            return (default, null);

        var types = new List<IInlineTypeNode>();
        var type = TryParseDiscriminatedUnion(context);
        if (type is not null)
        {
            types.Add(type);

            while (true)
            {
                var hasComma = context.Reader.Check(Comma);
                var comma = context.Reader.Token;

                type = TryParseDiscriminatedUnion(context);
                if (type is null)
                {
                    if (!hasComma)
                        break;

                    type = ParseFakeType(context, Comma, CloseParen);
                }

                if (!hasComma)
                    context.Diagnostics.MissingToken(comma.SourceSpan.Start, Comma);

                types.Add(type);
            }
        }

        var closeParenSpan = context.Reader.Expect(CloseParen);

        return (openParen.SourceSpan.Combine(closeParenSpan), types);
    }

    private IInlineTypeNode? TryParseTupleOrFunctionType(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var (typesSpan, types) = c.Parser.TryParseParenTypes(c);
            if (types is null)
                return null;

            var isFunctionType = c.Reader.Check(EqualGreater).Result;
            if (!isFunctionType && types.Count == 0)
            {
                c.Diagnostics.MissingToken(c.Reader.Span.Start, EqualGreater);
                isFunctionType = true;
            }

            if (isFunctionType)
            {
                var returnType = c.Parser.TryParseDiscriminatedUnion(c) ??
                                 c.Parser.ParseFakeType(c, Comma, CloseParen, SemiColon, CloseBrace);

                return new FunctionTypeNode(
                    typesSpan.Combine(returnType.SourceSpan),
                    types,
                    returnType);
            }

            if (types.Count == 1)
                return types[0];

            return new TupleTypeNode(typesSpan, types);
        });

    private InterfaceNode? TryParseInterface(ParserContext context)
        => context.Reader.Scoped(context, static c =>
        {
            var (hasOpenBrace, openBrace) = c.Reader.Check(OpenBrace);
            if (!hasOpenBrace)
                return null;

            // TODO: relax ordering
            var properties = c.Parser.TryParseInterfaceProperties(c);
            var methods = c.Parser.TryParseInterfaceMethods(c);
            var closeBraceSpan = c.Reader.Expect(CloseBrace);

            return new InterfaceNode(
                openBrace.SourceSpan.Combine(closeBraceSpan),
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
                       c.Parser.ParseFakeType(c, SemiColon, OpenBrace, CloseBrace);

            var (hasSemiColon, semiColon) = c.Reader.Check(SemiColon);
            if (hasSemiColon)
                return new InterfacePropertyNode(
                    span.Combine(semiColon.SourceSpan),
                    name,
                    type,
                    null,
                    null);

            c.Reader.Expect(OpenBrace);

            var getter = c.Parser.TryParseInterfacePropertyGetter(c);
            var setter = c.Parser.TryParseInterfacePropertySetter(c);
            var closeBraceSpan = c.Reader.Expect(CloseBrace);

            return new InterfacePropertyNode(
                span.Combine(closeBraceSpan),
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

            c.Reader.Expect(SemiColon);

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

            c.Reader.Expect(SemiColon);

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
            var (nameSpan, name) = c.Parser.TryParseId(c);
            if (name is null)
                return null;

            var (_, parameters) = c.Parser.TryParseParenTypes(c);
            if (parameters is null)
            {
                parameters = [];
                var span = c.Reader.SkipTo(Colon, SemiColon);
                c.Diagnostics.ExpectedInterfaceParameters(span);
            }

            c.Reader.Expect(Colon);

            var returnType = c.Parser.TryParseDiscriminatedUnion(c) ??
                             c.Parser.ParseFakeType(c, SemiColon, CloseBrace);

            var semiColonSpan = c.Reader.Expect(SemiColon);

            return new InterfaceMethodNode(
                nameSpan.Combine(semiColonSpan),
                name,
                parameters,
                returnType);
        });

    private (SourceSpan, string?) TryParseId(ParserContext context)
    {
        var (hasToken, token) = context.Reader.Check(Identifier);
        if (!hasToken)
            return default;

        return (token.SourceSpan, token.Identifier);
    }
}
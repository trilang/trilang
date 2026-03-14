using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Semantics;
using Trilang.Semantics.Model;

namespace Tri.Tests.Lower;

public class AddImplicitReturnStatementsTests
{
    private static readonly SourceFile file = new SourceFile("test.tri");

    private static SemanticTree Parse(string code)
    {
        var diagnostics = new DiagnosticCollection();

        var lexer = new Lexer();
        var lexerOptions = new LexerOptions(new LexerDiagnosticReporter(diagnostics, file));
        var tokens = lexer.Tokenize(code, lexerOptions);

        var parser = new Parser();
        var parserOptions = new ParserOptions(file, new ParserDiagnosticReporter(diagnostics, file));
        var tree = parser.Parse(tokens, parserOptions);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, cfgs) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var semanticTree = semanticTrees.Single();
        var lowering = new Lowering(builtInTypes);
        lowering.Lower(semanticTree, new LoweringOptions(new HashSet<string>(), cfgs));

        return semanticTree;
    }

    [Test]
    public void AddReturnToEmptyVoidMethodTest()
    {
        var tree = Parse(
            """
            namespace Test1;

            public test(): void { }
            """);

        var function = tree.Find<FunctionDeclaration>()!;
        var expected = new BlockStatement(null, [new ReturnStatement(null)]);

        Assert.That(function.Body, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void AddReturnToEndAfterWhileTest()
    {
        var tree = Parse(
            """
            namespace Test1;

            public test(): void {
                while (true) { }
            }
            """);

        var builtInTypes = new BuiltInTypes();
        NamespaceMetadata.CreateRoot(builtInTypes);

        var function = tree.Find<FunctionDeclaration>()!;
        var expected = new BlockStatement(null, [
            new BlockStatement(null, [
                new GoTo("loop_0_start"),
                new Label("loop_0_start"),
                new IfStatement(
                    null,
                    new LiteralExpression(null, LiteralExpressionKind.Boolean, true)
                    {
                        ReturnTypeMetadata = builtInTypes.Bool
                    },
                    new BlockStatement(null, [
                        new GoTo("loop_0_start"),
                    ]),
                    new BlockStatement(null, [
                        new GoTo("loop_0_end"),
                    ])
                ),
                new Label("loop_0_end"),
            ]),
            new ReturnStatement(null),
        ]);

        Assert.That(function.Body, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void AddReturnToEndAfterIfTest()
    {
        var tree = Parse(
            """
            namespace Test1;

            public test(): void {
                if (true) { }

                return;
            }
            """);

        var builtInTypes = new BuiltInTypes();
        NamespaceMetadata.CreateRoot(builtInTypes);

        var function = tree.Find<FunctionDeclaration>()!;
        var expected = new BlockStatement(null, [
            new IfStatement(
                null,
                new LiteralExpression(null, LiteralExpressionKind.Boolean, true)
                {
                    ReturnTypeMetadata = builtInTypes.Bool,
                },
                new BlockStatement(null, [
                    new GoTo("if_0_then"),
                ]),
                new BlockStatement(null, [
                    new GoTo("if_0_end"),
                ])
            ),
            new BlockStatement(null, [
                new Label("if_0_then"),
                new GoTo("if_0_end"),
            ]),
            new Label("if_0_end"),
            new ReturnStatement(null),
        ]);

        Assert.That(function.Body, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void KeepIfElseAsIsTest()
    {
        var tree = Parse(
            """
            namespace Test1;

            public test(): void {
                if (true) {
                    return;
                } else {
                    return;
                }
            }
            """);

        var builtInTypes = new BuiltInTypes();
        NamespaceMetadata.CreateRoot(builtInTypes);

        var function = tree.Find<FunctionDeclaration>()!;
        var expected = new BlockStatement(null, [
            new IfStatement(
                null,
                new LiteralExpression(null, LiteralExpressionKind.Boolean, true)
                {
                    ReturnTypeMetadata = builtInTypes.Bool
                },
                new BlockStatement(null, [
                    new GoTo("if_0_then"),
                ]),
                new BlockStatement(null, [
                    new GoTo("if_0_else"),
                ])
            ),
            new BlockStatement(null, [
                new Label("if_0_then"),
                new ReturnStatement(null),
                new GoTo("if_0_end"),
            ]),
            new BlockStatement(null, [
                new Label("if_0_else"),
                new ReturnStatement(null),
                new GoTo("if_0_end"),
            ]),
            new Label("if_0_end"),
        ]);

        Assert.That(function.Body, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void KeepInnerBlockAsIsTest()
    {
        var tree = Parse(
            """
            namespace Test1;

            public test(): void {
                {
                    return;
                }
            }
            """);

        var function = tree.Find<FunctionDeclaration>()!;
        var expected = new BlockStatement(null, [
            new BlockStatement(null, [
                new ReturnStatement(null),
            ]),
        ]);

        Assert.That(function.Body, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }
}
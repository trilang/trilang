using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using Trilang.Semantics.Passes.ControlFlow;

namespace Tri.Tests.Lower;

public class AddImplicitReturnStatementsTests
{
    private static (SemanticTree, ControlFlowGraphMap) Parse(string code)
    {
        var diagnostics = new DiagnosticCollection();

        var lexer = new Lexer();
        var tokens = lexer.Tokenize(code, new LexerOptions(diagnostics.Lexer));

        var parser = new Parser();
        var tree = parser.Parse(tokens);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, cfgs) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        return (semanticTree, cfgs);
    }

    [Test]
    public void AddReturnToEmptyVoidMethodTest()
    {
        var (tree, cfgs) = Parse("public test(): void { }");

        var lowering = new Lowering();
        lowering.Lower(tree, new LoweringOptions([], cfgs));

        var function = tree.Find<FunctionDeclaration>()!;
        var expected = new BlockStatement(null, [new ReturnStatement(null)]);

        Assert.That(function.Body, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void AddReturnToEndAfterWhileTest()
    {
        var (tree, cfgs) = Parse(
            """
            public test(): void {
                while (true) { }
            }
            """);

        var lowering = new Lowering();
        lowering.Lower(tree, new LoweringOptions([], cfgs));

        var function = tree.Find<FunctionDeclaration>()!;
        var expected = new BlockStatement(null, [
            new BlockStatement(null, [
                new GoTo("loop_0_start"),
                new Label("loop_0_start"),
                new IfStatement(
                    null,
                    new LiteralExpression(null, LiteralExpressionKind.Boolean, true)
                    {
                        ReturnTypeMetadata = TypeMetadata.Bool
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
        var (tree, cfgs) = Parse(
            """
            public test(): void {
                if (true) { }

                return;
            }
            """);

        var lowering = new Lowering();
        lowering.Lower(tree, new LoweringOptions([], cfgs));

        var function = tree.Find<FunctionDeclaration>()!;
        var expected = new BlockStatement(null, [
            new IfStatement(
                null,
                new LiteralExpression(null, LiteralExpressionKind.Boolean, true)
                {
                    ReturnTypeMetadata = TypeMetadata.Bool,
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
        var (tree, cfgs) = Parse(
            """
            public test(): void {
                if (true) {
                    return;
                } else {
                    return;
                }
            }
            """);

        var lowering = new Lowering();
        lowering.Lower(tree, new LoweringOptions([], cfgs));

        var function = tree.Find<FunctionDeclaration>()!;
        var expected = new BlockStatement(null, [
            new IfStatement(
                null,
                new LiteralExpression(null, LiteralExpressionKind.Boolean, true)
                {
                    ReturnTypeMetadata = TypeMetadata.Bool
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
        var (tree, cfgs) = Parse(
            """
            public test(): void {
                {
                    return;
                }
            }
            """);

        var lowering = new Lowering();
        lowering.Lower(tree, new LoweringOptions([], cfgs));

        var function = tree.Find<FunctionDeclaration>()!;
        var expected = new BlockStatement(null, [
            new BlockStatement(null, [
                new ReturnStatement(null),
            ]),
        ]);

        Assert.That(function.Body, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }
}
using Trilang;
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
        var parser = new Parser();
        var tree = parser.Parse(code);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, cfgs) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        return (semanticTree, cfgs);
    }

    [Test]
    public void AddReturnToEmptyVoidMethodTest()
    {
        var (tree, cfgs) = Parse("function test(): void { }");

        var lowering = new Lowering();
        lowering.Lower(tree, new LoweringOptions([], cfgs));

        var function = tree.Find<FunctionDeclaration>()!;
        var expected = new BlockStatement([new ReturnStatement()]);

        Assert.That(function.Body, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void AddReturnToEndAfterWhileTest()
    {
        var (tree, cfgs) = Parse(
            """
            function test(): void {
                while (true) { }
            }
            """);

        var lowering = new Lowering();
        lowering.Lower(tree, new LoweringOptions([], cfgs));

        var function = tree.Find<FunctionDeclaration>()!;
        var expected = new BlockStatement([
            new BlockStatement([
                new GoTo("loop_0_start"),
                new Label("loop_0_start"),
                new IfStatement(
                    new LiteralExpression(LiteralExpressionKind.Boolean, true)
                    {
                        ReturnTypeMetadata = TypeMetadata.Bool
                    },
                    new BlockStatement([
                        new GoTo("loop_0_start"),
                    ]),
                    new BlockStatement([
                        new GoTo("loop_0_end"),
                    ])
                ),
                new Label("loop_0_end"),
            ]),
            new ReturnStatement(),
        ]);

        Assert.That(function.Body, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void AddReturnToEndAfterIfTest()
    {
        var (tree, cfgs) = Parse(
            """
            function test(): void {
                if (true) { }

                return;
            }
            """);

        var lowering = new Lowering();
        lowering.Lower(tree, new LoweringOptions([], cfgs));

        var function = tree.Find<FunctionDeclaration>()!;
        var expected = new BlockStatement([
            new IfStatement(
                new LiteralExpression(LiteralExpressionKind.Boolean, true)
                {
                    ReturnTypeMetadata = TypeMetadata.Bool,
                },
                new BlockStatement([
                    new GoTo("if_0_then"),
                ]),
                new BlockStatement([
                    new GoTo("if_0_end"),
                ])
            ),
            new BlockStatement([
                new Label("if_0_then"),
                new GoTo("if_0_end"),
            ]),
            new Label("if_0_end"),
            new ReturnStatement(),
        ]);

        Assert.That(function.Body, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void KeepIfElseAsIsTest()
    {
        var (tree, cfgs) = Parse(
            """
            function test(): void {
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
        var expected = new BlockStatement([
            new IfStatement(
                new LiteralExpression(LiteralExpressionKind.Boolean, true)
                {
                    ReturnTypeMetadata = TypeMetadata.Bool
                },
                new BlockStatement([
                    new GoTo("if_0_then"),
                ]),
                new BlockStatement([
                    new GoTo("if_0_else"),
                ])
            ),
            new BlockStatement([
                new Label("if_0_then"),
                new ReturnStatement(),
                new GoTo("if_0_end"),
            ]),
            new BlockStatement([
                new Label("if_0_else"),
                new ReturnStatement(),
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
            function test(): void {
                {
                    return;
                }
            }
            """);

        var lowering = new Lowering();
        lowering.Lower(tree, new LoweringOptions([], cfgs));

        var function = tree.Find<FunctionDeclaration>()!;
        var expected = new BlockStatement([
            new BlockStatement([
                new ReturnStatement(),
            ]),
        ]);

        Assert.That(function.Body, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }
}
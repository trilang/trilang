using Trilang;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Lowering;

public class AddImplicitReturnStatementsTests
{
    [Test]
    public void AddReturnToEmptyVoidMethodTest()
    {
        var (tree, diagnostics, _) = Lower(
            CreateFile(
                """
                namespace Test1;

                public test(): void { }
                """));

        var function = tree.Find<FunctionDeclaration>()!;
        var expected = new BlockStatement(null, [new ReturnStatement(null)]);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(function.Body, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void AddReturnToEndAfterWhileTest()
    {
        var (tree, diagnostics, _) = Lower(
            CreateFile(
                """
                namespace Test1;

                public test(): void {
                    while (true) { }
                }
                """));

        var builtInTypes = new BuiltInTypes();
        RootNamespaceMetadata.Create(builtInTypes);

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

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(function.Body, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void AddReturnToEndAfterIfTest()
    {
        var (tree, diagnostics, _) = Lower(
            CreateFile(
                """
                namespace Test1;

                public test(): void {
                    if (true) { }

                    return;
                }
                """));

        var builtInTypes = new BuiltInTypes();
        RootNamespaceMetadata.Create(builtInTypes);

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

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(function.Body, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void KeepIfElseAsIsTest()
    {
        var (tree, diagnostics, _) = Lower(
            CreateFile(
                """
                namespace Test1;

                public test(): void {
                    if (true) {
                        return;
                    } else {
                        return;
                    }
                }
                """));

        var builtInTypes = new BuiltInTypes();
        RootNamespaceMetadata.Create(builtInTypes);

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

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(function.Body, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void KeepInnerBlockAsIsTest()
    {
        var (tree, diagnostics, _) = Lower(
            CreateFile(
                """
                namespace Test1;

                public test(): void {
                    {
                        return;
                    }
                }
                """));

        var function = tree.Find<FunctionDeclaration>()!;
        var expected = new BlockStatement(null, [
            new BlockStatement(null, [
                new ReturnStatement(null),
            ]),
        ]);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(function.Body, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }
}
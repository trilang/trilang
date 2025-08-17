using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class CheckAccessModifiersTests
{
    private static SyntaxTree Parse(string code)
    {
        var parser = new Parser();
        var tree = parser.Parse(code);

        return tree;
    }

    [Test]
    public void PrivateCtorTest()
    {
        var tree = Parse(
            """
            public type Test {
                private constructor() { }
            }

            function main(): void {
                var x: Test = new Test();
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The constructor of 'Test' is not accessible."));
    }

    [Test]
    public void IgnorePrivateCtorInTheSameTypeTest()
    {
        var tree = Parse(
            """
            public type Test {
                private constructor() { }

                public create(): Test {
                    return new Test();
                }
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.Nothing);
    }

    [Test]
    public void PrivateGetterTest()
    {
        var tree = Parse(
            """
            public type Point {
                x: i32 { private get; private set; }
            }

            function test(): i32 {
                var p: Point = new Point();

                return p.x;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo($"The getter of 'x' is private."));
    }

    [Test]
    public void PrivateSetterTest()
    {
        var tree = Parse(
            """
            public type Point {
                x: i32 { private get; private set; }
            }

            function test(): void {
                var p: Point = new Point();

                p.x = 1;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo($"The setter of 'x' is private."));
    }

    [Test]
    public void PrivateGetterInTheSameTypeTest()
    {
        var tree = Parse(
            """
            public type Point {
                x: i32 { private get; private set; }

                public getX(): i32 {
                    return x;
                }
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.Nothing);
    }

    [Test]
    public void PrivateSetterInTheSameTypeTest()
    {
        var tree = Parse(
            """
            public type Point {
                x: i32 { private get; private set; }

                public constructor(x: i32) {
                    this.x = x;
                }
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.Nothing);
    }

    [Test]
    public void MissingGetterTest()
    {
        var tree = Parse(
            """
            public type Point {
                x: i32 { public set; }
            }

            function test(p: Point): i32 {
                return p.x;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'x' property does not have a getter."));
    }

    [Test]
    public void MissingSetterTest()
    {
        var tree = Parse(
            """
            public type Point {
                x: i32 { public get; }
            }

            function test(p: Point): void {
                p.x = 1;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'x' property does not have a setter."));
    }
}
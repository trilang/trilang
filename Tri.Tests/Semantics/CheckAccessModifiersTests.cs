using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class CheckAccessModifiersTests
{
    private static readonly SourceFile file = new SourceFile("test.tri");

    private static (SyntaxTree, DiagnosticCollection) Parse(string code)
    {
        var diagnostics = new DiagnosticCollection();

        var lexer = new Lexer();
        var lexerOptions = new LexerOptions(new LexerDiagnosticReporter(diagnostics, file));
        var tokens = lexer.Tokenize(code, lexerOptions);

        var parser = new Parser();
        var parserOptions = new ParserOptions(file, new ParserDiagnosticReporter(diagnostics, file));
        var tree = parser.Parse(tokens, parserOptions);

        return (tree, diagnostics);
    }

    [Test]
    public void PrivateCtorTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test {
                private constructor() { }
            }

            public main(): void {
                var x: Test = new Test();
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The constructor of 'Test' is not accessible."));
    }

    [Test]
    public void IgnorePrivateCtorInTheSameTypeTest()
    {
        var (tree, diagnostics) = Parse(
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
            () => semantic.Analyze(tree, new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.Nothing);
    }

    [Test]
    public void PrivateGetterTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                x: i32 { private get; private set; }
            }

            public test(): i32 {
                var p: Point = new Point();

                return p.x;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo($"The getter of 'x' is private."));
    }

    [Test]
    public void PrivateSetterTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                x: i32 { private get; private set; }
            }

            public test(): void {
                var p: Point = new Point();

                p.x = 1;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo($"The setter of 'x' is private."));
    }

    [Test]
    public void PrivateGetterInTheSameTypeTest()
    {
        var (tree, diagnostics) = Parse(
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
            () => semantic.Analyze(tree, new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.Nothing);
    }

    [Test]
    public void PrivateSetterInTheSameTypeTest()
    {
        var (tree, diagnostics) = Parse(
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
            () => semantic.Analyze(tree, new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.Nothing);
    }

    [Test]
    public void MissingGetterTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                x: i32 { public set; }
            }

            public test(p: Point): i32 {
                return p.x;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'x' property does not have a getter."));
    }

    [Test]
    public void MissingSetterTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                x: i32 { public get; }
            }

            public test(p: Point): void {
                p.x = 1;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'x' property does not have a setter."));
    }

    [Test]
    public void InternalFunctionTest()
    {
        var (tree, diagnostics) = Parse("internal test(): void {}");

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'test' function can't be internal."));
    }
}
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
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0014MemberNotAccessible,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(92, 6, 19), new SourcePosition(102, 6, 29))),
            "The constructor of 'Test' is not accessible.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
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
            () => semantic.Analyze(
                [tree],
                new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
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
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0014MemberNotAccessible,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(129, 8, 12), new SourcePosition(132, 8, 15))),
            "The getter of 'x' is not accessible.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
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
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0014MemberNotAccessible,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(123, 8, 5), new SourcePosition(126, 8, 8))),
            "The setter of 'x' is not accessible.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
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
            () => semantic.Analyze(
                [tree],
                new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
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
            () => semantic.Analyze(
                [tree],
                new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
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
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0008UnknownMember,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(90, 6, 12), new SourcePosition(93, 6, 15))),
            "The 'x' property doesn't have a getter.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
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
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0008UnknownMember,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(84, 6, 5), new SourcePosition(87, 6, 8))),
            "The 'x' property doesn't have a setter.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void PublicMethodTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public toString(): string {
                    return "xxx";
                }
            }

            public test(p: Point): void {
                var s: string = p.toString();
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void PrivateMethodTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                private toString(): string {
                    return "xxx";
                }
            }

            public test(p: Point): void {
                var s: string = p.toString();
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0014MemberNotAccessible,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(134, 8, 21), new SourcePosition(144, 8, 31))),
            "The 'toString' method is not accessible.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void PrivateMethodInTheSameTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                private toString(): string {
                    return "xxx";
                }

                public test(): void {
                    var s: string = toString();
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void PrivateFunctionInTheSameFileTest()
    {
        var (tree, diagnostics) = Parse(
            """
            private test(): void { }

            public main(): void {
                test();
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }
}
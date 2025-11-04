using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class CheckStaticAndInstanceMembersAccessTests
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
    public void AccessNotStaticMethodOnTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test {
                public s(): void { }
            }

            public func(): void {
                Test.s();
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0019InstanceMethodAsStatic,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(73, 6, 5), new SourcePosition(79, 6, 11))),
            "The instance method 's' cannot be called as a static one.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void AccessStaticMethodOnInstanceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test {
                public static s(): void { }
            }

            public func(a: Test): void {
                a.s();
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0018StaticMethodAsInstance,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(87, 6, 5), new SourcePosition(90, 6, 8))),
            "The static method 's' cannot be called as an instance one.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void AccessInstanceMethodWithThisTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test {
                public method1(): void { }

                public method2(): void {
                    this.method1();
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
    public void AccessStaticOnInvalidTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test = Test;

            public func(): void {
                Test.s();
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0001CyclicTypeAlias,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(24, 1, 25))),
            "The cyclic type alias detected: 'Test'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}
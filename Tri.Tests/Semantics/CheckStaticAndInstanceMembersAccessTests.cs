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

        Assert.That(
            () => semantic.Analyze(tree, new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The instance method 's' cannot be called on a static one."));
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

        Assert.That(
            () => semantic.Analyze(tree, new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The static method 's' cannot be called on an instance one."));
    }

    [Test]
    public void AccessMemberOnInterfaceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test = {
                s(): void;
            }

            public func(): void {
                Test.s();
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("'Test' can't be used to call static members."));
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
                tree,
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

        // TODO: Throws.Nothing
        Assert.That(
            () => semantic.Analyze(
                tree,
                new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Cannot determine return type for member 's'"));
    }
}
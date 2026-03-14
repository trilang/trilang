using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class VariableUsedBeforeDeclaredTests
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
    public void VariableUsedAfterDeclarationTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                var a: i32 = 1;
                a;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(
                [tree],
                new SemanticAnalysisOptions(
                    new HashSet<string>(),
                    new SemanticDiagnosticReporter(diagnostics),
                    new BuiltInTypes())),
            Throws.Nothing);
    }

    [Test]
    public void ParameterUsedAfterDeclarationTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(a: i32): void {
                a;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(
                [tree],
                new SemanticAnalysisOptions(
                    new HashSet<string>(),
                    new SemanticDiagnosticReporter(diagnostics),
                    new BuiltInTypes())),
            Throws.Nothing);
    }

    [Test]
    public void VariableUsedBeforeDeclarationTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                a;
                var a: i32 = 1;
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                new SemanticDiagnosticReporter(diagnostics),
                new BuiltInTypes()));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0020VariableUsedBeforeDeclaration,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(44, 4, 5), new SourcePosition(45, 4, 6))),
            "The 'a' variable is used before its declaration.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void VariableInBlockUsedBeforeDeclarationTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                {
                    a;
                }
                var a: i32 = 1;
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                new SemanticDiagnosticReporter(diagnostics),
                new BuiltInTypes()));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0020VariableUsedBeforeDeclaration,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(54, 5, 9), new SourcePosition(55, 5, 10))),
            "The 'a' variable is used before its declaration.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void VariableInDifferentBlocksTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public main(): void {
                {
                    var a: i32 = 1;
                }
                {
                    a;
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                new SemanticDiagnosticReporter(diagnostics),
                new BuiltInTypes()));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0008UnknownMember,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(90, 8, 9), new SourcePosition(91, 8, 10))),
            "Unknown symbol: 'a'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void VariableInDeclaredInDifferentFunctionTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(): void {
                var a: i32 = 1;
            }

            public main(): void {
                a;
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                new SemanticDiagnosticReporter(diagnostics),
                new BuiltInTypes()));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0008UnknownMember,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(89, 8, 5), new SourcePosition(90, 8, 6))),
            "Unknown symbol: 'a'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void VariableInParentScopeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            namespace Test1;

            public test(): i32 {
                var a: i32 = 1;
                {
                    return a;
                }
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(
                [tree],
                new SemanticAnalysisOptions(
                    new HashSet<string>(),
                    new SemanticDiagnosticReporter(diagnostics),
                    new BuiltInTypes())),
            Throws.Nothing);
    }
}
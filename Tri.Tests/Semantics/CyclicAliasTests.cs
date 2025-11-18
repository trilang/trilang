using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class CyclicAliasTests
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
    public void RecursiveTypeAliasTest()
    {
        var (tree, diagnostics) = Parse("public type Test = Test;");

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var testAlias = typeProvider.GetType("Test");

        var diagnostic = new Diagnostic(
            DiagnosticId.S0001CyclicTypeAlias,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(24, 1, 25))),
            "The cyclic type alias detected: 'Test'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
        Assert.That(testAlias, Is.Not.Null);
        Assert.That(testAlias.IsInvalid, Is.True);
    }

    [Test]
    public void RecursiveTypeAliasTest2()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test1 = Test2;
            public type Test2 = Test1;
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var test1Alias = typeProvider.GetType("Test1");
        var test2Alias = typeProvider.GetType("Test2");

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.S0001CyclicTypeAlias,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    file,
                    new SourceSpan(new SourcePosition(0, 1, 1), new SourcePosition(26, 1, 27))),
                "The cyclic type alias detected: 'Test1'."),
            new Diagnostic(
                DiagnosticId.S0001CyclicTypeAlias,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    file,
                    new SourceSpan(new SourcePosition(27, 2, 1), new SourcePosition(53, 2, 27))),
                "The cyclic type alias detected: 'Test2'.")
        };

        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
        Assert.That(test1Alias, Is.Not.Null);
        Assert.That(test1Alias.IsInvalid, Is.True);
        Assert.That(test2Alias, Is.Not.Null);
        Assert.That(test2Alias.IsInvalid, Is.True);
    }
}
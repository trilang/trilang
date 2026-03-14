using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class CyclicAliasTests
{
    private static SyntaxTree Parse(DiagnosticCollection diagnostics, string filePath, string code)
    {
        var file = new SourceFile(filePath);
        var lexer = new Lexer();
        var lexerOptions = new LexerOptions(new LexerDiagnosticReporter(diagnostics, file));
        var tokens = lexer.Tokenize(code, lexerOptions);

        var parser = new Parser();
        var parserOptions = new ParserOptions(file, new ParserDiagnosticReporter(diagnostics, file));
        var tree = parser.Parse(tokens, parserOptions);

        return tree;
    }

    [Test]
    public void RecursiveTypeAliasTest()
    {
        var diagnostics = new DiagnosticCollection();
        var tree = Parse(
            diagnostics,
            "test.tri",
            """
            namespace Test1;

            public type Test = Test;
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), new BuiltInTypes()));

        var test1Ns = rootNamespace.FindNamespace(["Test1"])!;
        var testAlias = test1Ns.FindType("Test");

        var diagnostic = new Diagnostic(
            DiagnosticId.S0001CyclicTypeAlias,
            DiagnosticSeverity.Error,
            new SourceLocation(
                new SourceFile("test.tri"),
                new SourceSpan(new SourcePosition(18, 3, 1), new SourcePosition(42, 3, 25))),
            "The cyclic type alias detected: 'Test'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
        Assert.That(testAlias, Is.Not.Null);
        Assert.That(testAlias.IsInvalid, Is.True);
    }

    [Test]
    public void RecursiveTypeAliasTest2()
    {
        var diagnostics = new DiagnosticCollection();
        var tree = Parse(
            diagnostics,
            "test.tri",
            """
            namespace Test1;

            public type Test1 = Test2;
            public type Test2 = Test1;
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), new BuiltInTypes()));

        var test1Ns = rootNamespace.FindNamespace(["Test1"])!;
        var test1Alias = test1Ns.FindType("Test1");
        var test2Alias = test1Ns.FindType("Test2");

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.S0001CyclicTypeAlias,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    new SourceFile("test.tri"),
                    new SourceSpan(new SourcePosition(18, 3, 1), new SourcePosition(44, 3, 27))),
                "The cyclic type alias detected: 'Test1'."),
            new Diagnostic(
                DiagnosticId.S0001CyclicTypeAlias,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    new SourceFile("test.tri"),
                    new SourceSpan(new SourcePosition(45, 4, 1), new SourcePosition(71, 4, 27))),
                "The cyclic type alias detected: 'Test2'.")
        };

        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
        Assert.That(test1Alias, Is.Not.Null);
        Assert.That(test1Alias.IsInvalid, Is.True);
        Assert.That(test2Alias, Is.Not.Null);
        Assert.That(test2Alias.IsInvalid, Is.True);
    }

    [Test]
    public void RecursiveTypesInDifferentNamespacesTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;

            use NS2;

            public type Test1 = Test2;
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace NS2;

            use NS1;

            public type Test2 = Test1;
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [file1, file2],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), new BuiltInTypes()));

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.S0001CyclicTypeAlias,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    new SourceFile("file1.tri"),
                    new SourceSpan(new SourcePosition(26, 5, 1), new SourcePosition(52, 5, 27))),
                "The cyclic type alias detected: 'Test1'."),
            new Diagnostic(
                DiagnosticId.S0001CyclicTypeAlias,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    new SourceFile("file2.tri"),
                    new SourceSpan(new SourcePosition(26, 5, 1), new SourcePosition(52, 5, 27))),
                "The cyclic type alias detected: 'Test2'.")
        };

        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }
}
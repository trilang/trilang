using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class NotImplementedInterfaceTests
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
    public void EverythingIsImplementedInTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Interface1 = {
                x: i32;
                toString(): string;
            }

            public type Test : Interface1 {
                x: i32;
                public toString(): string {
                    return "Hello, World!";
                }
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.Nothing);
    }

    [Test]
    public void NotImplementedPropertyTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Interface1 = {
                x: i32;
                toString(): string;
            }

            public type Test : Interface1 {
                public toString(): string {
                    return "Hello, World!";
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            tree,
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0026MemberIsNotImplemented,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(66, 6, 1), new SourcePosition(169, 10, 2))),
            "The 'x' property is not implemented.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ImplementPropertyWithIncorrectTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Interface1 = {
                x: i32;
                toString(): string;
            }

            public type Test : Interface1 {
                x: i8;
                public toString(): string {
                    return "Hello, World!";
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            tree,
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0026MemberIsNotImplemented,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(102, 7, 5), new SourcePosition(108, 7, 11))),
            "The 'x' property is not of the correct type. Expected 'i8', got 'i32'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void NotImplementedMethodTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Interface1 = {
                x: i32;
                toString(): string;
            }

            public type Test : Interface1 {
                x: i32;
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            tree,
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0026MemberIsNotImplemented,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(66, 6, 1), new SourcePosition(111, 8, 2))),
            "The 'toString' method is not implemented.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ImplementMethodWithIncorrectReturnTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Interface1 = {
                x: i32;
                toString(): string;
            }

            public type Test : Interface1 {
                x: i32;
                public toString(): i32 {
                    return 1;
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            tree,
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0026MemberIsNotImplemented,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(114, 8, 5), new SourcePosition(162, 10, 6))),
            "The 'toString' method is not of the correct type. Expected '() => string', got '() => i32'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ImplementMethodWithIncorrectParametersTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Interface1 = {
                x: i32;
                toString(): string;
            }

            public type Test : Interface1 {
                x: i32;
                public toString(a: i32): string {
                    return "Hello, World!";
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            tree,
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0026MemberIsNotImplemented,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(114, 8, 5), new SourcePosition(185, 10, 6))),
            "The 'toString' method is not of the correct type. Expected '() => string', got '(i32) => string'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void TypeImplementsMethodAsPrivateTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Interface1 = {
                method(): void;
            }

            public type Test : Interface1 {
                private method(): void {
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            tree,
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0026MemberIsNotImplemented,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(86, 6, 5), new SourcePosition(116, 7, 6))),
            "The implementation of the interface method 'method' is not public.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void TypeImplementsGetterAsPrivateTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Interface1 = {
                x: i32 { public get; public set; }
            }

            public type Test : Interface1 {
                x: i32 { private get; public set; }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            tree,
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0026MemberIsNotImplemented,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(105, 6, 5), new SourcePosition(140, 6, 40))),
            "The implementation of an interface property getter 'x' cannot be private.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void TypeImplementsSetterAsPrivateTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Interface1 = {
                x: i32 { public get; public set; }
            }

            public type Test : Interface1 {
                x: i32 { public get; private set; }
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            tree,
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0026MemberIsNotImplemented,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(105, 6, 5), new SourcePosition(140, 6, 40))),
            "The implementation of an interface property setter 'x' cannot be private.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}
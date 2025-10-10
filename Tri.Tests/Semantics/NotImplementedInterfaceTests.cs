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

        Assert.That(
            () => semantic.Analyze(tree, new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'x' property is not implemented."));
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

        Assert.That(
            () => semantic.Analyze(tree, new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'x' property is not of the correct type."));
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

        Assert.That(
            () => semantic.Analyze(tree, new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'toString' method is not implemented."));
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

        Assert.That(
            () => semantic.Analyze(tree, new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'toString' method is not of the correct type."));
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

        Assert.That(
            () => semantic.Analyze(tree, new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'toString' method is not of the correct type."));
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

        Assert.That(
            () => semantic.Analyze(tree, new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The implementation of an interface method 'method' cannot be private."));
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

        Assert.That(
            () => semantic.Analyze(tree, new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The implementation of an interface property getter 'x' cannot be private."));
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

        Assert.That(
            () => semantic.Analyze(tree, new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics))),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The implementation of an interface property setter 'x' cannot be private."));
    }
}
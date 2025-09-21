using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class NotImplementedInterfaceTests
{
    private static SyntaxTree Parse(string code)
    {
        var diagnostics = new DiagnosticCollection();

        var lexer = new Lexer();
        var tokens = lexer.Tokenize(code, new LexerOptions(diagnostics.Lexer));

        var parser = new Parser();
        var tree = parser.Parse(tokens);

        return tree;
    }

    [Test]
    public void EverythingIsImplementedInTypeTest()
    {
        var tree = Parse(
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
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.Nothing);
    }

    [Test]
    public void NotImplementedPropertyTest()
    {
        var tree = Parse(
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
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'x' property is not implemented."));
    }

    [Test]
    public void ImplementPropertyWithIncorrectTypeTest()
    {
        var tree = Parse(
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
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'x' property is not of the correct type."));
    }

    [Test]
    public void NotImplementedMethodTest()
    {
        var tree = Parse(
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
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'toString' method is not implemented."));
    }

    [Test]
    public void ImplementMethodWithIncorrectReturnTypeTest()
    {
        var tree = Parse(
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
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'toString' method is not of the correct type."));
    }

    [Test]
    public void ImplementMethodWithIncorrectParametersTest()
    {
        var tree = Parse(
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
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'toString' method is not of the correct type."));
    }

    [Test]
    public void TypeImplementsMethodAsPrivateTest()
    {
        var tree = Parse(
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
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The implementation of an interface method 'method' cannot be private."));
    }

    [Test]
    public void TypeImplementsGetterAsPrivateTest()
    {
        var tree = Parse(
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
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The implementation of an interface property getter 'x' cannot be private."));
    }

    [Test]
    public void TypeImplementsSetterAsPrivateTest()
    {
        var tree = Parse(
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
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The implementation of an interface property setter 'x' cannot be private."));
    }
}
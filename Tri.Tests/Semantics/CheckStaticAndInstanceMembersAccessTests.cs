using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class CheckStaticAndInstanceMembersAccessTests
{
    private static SyntaxTree Parse(string code)
    {
        var parser = new Parser();
        var tree = parser.Parse(code);

        return tree;
    }

    [Test]
    public void AccessNotStaticMethodOnTypeTest()
    {
        var tree = Parse(
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
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The instance method 's' cannot be called on a static one."));
    }

    [Test]
    public void AccessStaticMethodOnInstanceTest()
    {
        var tree = Parse(
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
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The static method 's' cannot be called on an instance one."));
    }

    [Test]
    public void AccessMemberOnInterfaceTest()
    {
        var tree = Parse(
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
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("'Test' can't be used to call static members."));
    }

    [Test]
    public void AccessInstanceMethodWithThisTest()
    {
        var tree = Parse(
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
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.Nothing);
    }
}
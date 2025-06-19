using Trilang.Lower;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Lower;

public class ReplaceIfDirectivesTests
{
    private static SyntaxTree Parse(string code)
    {
        var parser = new Parser();
        var tree = parser.Parse(code);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        return tree;
    }

    [Test]
    public void ReplaceIfDirectiveWithThenDeclarationsTest()
    {
        const string code =
            """
            #if D1

            public type Type1 { }

            #else

            public type Type2 { }

            #endif

            public type Type3 { }
            """;
        var tree = Parse(code);

        var lowering = new Lowering();
        lowering.Lower(tree, new LoweringOptions(["D1"]));

        var expected = new SyntaxTree([
            new TypeDeclarationNode(AccessModifier.Public, "Type1", [], [], [], [], []),
            new TypeDeclarationNode(AccessModifier.Public, "Type3", [], [], [], [], []),
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ReplaceIfDirectiveWithElseDeclarationsTest()
    {
        const string code =
            """
            #if D1

            public type Type1 { }

            #else

            public type Type2 { }

            #endif

            public type Type3 { }
            """;
        var tree = Parse(code);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var expected = new SyntaxTree([
            new TypeDeclarationNode(AccessModifier.Public, "Type2", [], [], [], [], []),
            new TypeDeclarationNode(AccessModifier.Public, "Type3", [], [], [], [], []),
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void RemoveIfDirectiveDeclarationsTest()
    {
        const string code =
            """
            #if D1

            public type Type1 { }

            #endif

            public type Type3 { }
            """;
        var tree = Parse(code);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var expected = new SyntaxTree([
            new TypeDeclarationNode(AccessModifier.Public, "Type3", [], [], [], [], []),
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ReplaceIfDirectiveWithThenStatementsTest()
    {
        const string code =
            """
            function test(callback: () => void): i32 {
                callback();

            #if D1
                return 1;
            #else
                return 2;
            #endif
            }
            """;
        var tree = Parse(code);

        var lowering = new Lowering();
        lowering.Lower(tree, new LoweringOptions(["D1"]));

        var expected = new SyntaxTree([
            new FunctionDeclarationNode(
                "test",
                [new ParameterNode("callback", new FunctionTypeNode([], new TypeNode("void")))],
                new TypeNode("i32"),
                new BlockStatementNode([
                    new ExpressionStatementNode(
                        new CallExpressionNode(new MemberAccessExpressionNode("callback"), [])
                    ),
                    new ReturnStatementNode(LiteralExpressionNode.Number(1))
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ReplaceIfDirectiveWithElseStatementsTest()
    {
        const string code =
            """
            function test(callback: () => void): i32 {
                callback();

            #if D1
                return 1;
            #else
                return 2;
            #endif
            }
            """;
        var tree = Parse(code);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var expected = new SyntaxTree([
            new FunctionDeclarationNode(
                "test",
                [new ParameterNode("callback", new FunctionTypeNode([], new TypeNode("void")))],
                new TypeNode("i32"),
                new BlockStatementNode([
                    new ExpressionStatementNode(
                        new CallExpressionNode(new MemberAccessExpressionNode("callback"), [])
                    ),
                    new ReturnStatementNode(LiteralExpressionNode.Number(2))
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void RemoveIfDirectiveWithStatementsTest()
    {
        const string code =
            """
            function test(callback: () => void): i32 {
                callback();

            #if D1
                return 1;
            #endif

                return 2;
            }
            """;
        var tree = Parse(code);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var expected = new SyntaxTree([
            new FunctionDeclarationNode(
                "test",
                [new ParameterNode("callback", new FunctionTypeNode([], new TypeNode("void")))],
                new TypeNode("i32"),
                new BlockStatementNode([
                    new ExpressionStatementNode(
                        new CallExpressionNode(new MemberAccessExpressionNode("callback"), [])
                    ),
                    new ReturnStatementNode(LiteralExpressionNode.Number(2))
                ])
            )
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }
}
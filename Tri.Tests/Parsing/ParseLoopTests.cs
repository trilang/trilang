using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests.Parsing;

public class ParseLoopTests
{
    [Test]
    public void ParseWhileTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            function test(x: i32): void {
                while (x > 0) {
                }
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "test",
                [new ParameterNode("x", TypeNode.Create("i32"))],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new WhileNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.GreaterThan,
                            new MemberAccessExpressionNode("x"),
                            LiteralExpressionNode.Number(0)
                        ),
                        new BlockStatementNode([])
                    )
                ])
            ),
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseWhileMissingOpenParenTest()
    {
        var parser = new Parser();
        var source = """
                     function test(x: i32): void {
                         while x > 0) {
                         }
                     }
                     """;

        Assert.Throws<ParseException>(() => parser.Parse(source));
    }

    [Test]
    public void ParseWhileMissingConditionTest()
    {
        var parser = new Parser();
        var source = """
                     function test(x: i32): void {
                         while (;) {
                         }
                     }
                     """;

        Assert.Throws<ParseException>(() => parser.Parse(source));
    }

    [Test]
    public void ParseWhileMissingCloseParenTest()
    {
        var parser = new Parser();
        var source = """
                     function test(x: i32): void {
                         while (x > 0 {
                         }
                     }
                     """;

        Assert.Throws<ParseException>(() => parser.Parse(source));
    }

    [Test]
    public void ParseWhileMissingBlockTest()
    {
        var parser = new Parser();
        var source = """
                     function test(x: i32): void {
                         while (x > 0)
                            ;
                     }
                     """;

        Assert.Throws<ParseException>(() => parser.Parse(source));
    }

    [Test]
    public void ParseBreakTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            function test(x: i32): void {
                while (x > 0) {
                    break;
                }
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "test",
                [new ParameterNode("x", TypeNode.Create("i32"))],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new WhileNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.GreaterThan,
                            new MemberAccessExpressionNode("x"),
                            LiteralExpressionNode.Number(0)
                        ),
                        new BlockStatementNode([new BreakNode()])
                    )
                ])
            ),
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }

    [Test]
    public void ParseContinueTest()
    {
        var parser = new Parser();
        var tree = parser.Parse(
            """
            function test(x: i32): void {
                while (x > 0) {
                    continue;
                }
            }
            """);

        var expected = new SyntaxTree([
            FunctionDeclarationNode.Create(
                "test",
                [new ParameterNode("x", TypeNode.Create("i32"))],
                TypeNode.Create("void"),
                new BlockStatementNode([
                    new WhileNode(
                        new BinaryExpressionNode(
                            BinaryExpressionKind.GreaterThan,
                            new MemberAccessExpressionNode("x"),
                            LiteralExpressionNode.Number(0)
                        ),
                        new BlockStatementNode([new ContinueNode()])
                    )
                ])
            ),
        ]);

        Assert.That(tree, Is.EqualTo(expected));
    }
}
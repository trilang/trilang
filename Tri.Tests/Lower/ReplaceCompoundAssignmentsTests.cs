using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;
using static Trilang.Parsing.Ast.BinaryExpressionKind;

namespace Tri.Tests.Lower;

public class ReplaceCompoundAssignmentsTests
{
    private static SyntaxTree Parse(string code)
    {
        var parser = new Parser();
        var tree = parser.Parse(code);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        return tree;
    }

    [TestCase("+=", Addition)]
    [TestCase("-=", Subtraction)]
    [TestCase("*=", Multiplication)]
    [TestCase("/=", Division)]
    [TestCase("%=", Modulus)]
    [TestCase("&=", BitwiseAnd)]
    [TestCase("|=", BitwiseOr)]
    [TestCase("^=", BitwiseXor)]
    public void ReplaceCompoundAssignmentsTest(string op, BinaryExpressionKind kind)
    {
        var tree = Parse(
            $$"""
              function test(x: i32): void {
                  x {{op}} 1;
              }
              """);
        var expected = new SyntaxTree([
            new FunctionDeclarationNode(
                "test",
                [new ParameterNode("x", new TypeNode("i32") { Metadata = TypeMetadata.I32 })],
                new TypeNode("void") { Metadata = TypeMetadata.Void },
                new BlockStatementNode([
                    new ExpressionStatementNode(
                        new BinaryExpressionNode(
                            Assignment,
                            new MemberAccessExpressionNode("x")
                            {
                                ReturnTypeMetadata = TypeMetadata.I32,
                            },
                            new BinaryExpressionNode(
                                kind,
                                new MemberAccessExpressionNode("x")
                                {
                                    ReturnTypeMetadata = TypeMetadata.I32,
                                },
                                new LiteralExpressionNode(LiteralExpressionKind.Number, 1)
                                {
                                    ReturnTypeMetadata = TypeMetadata.I32,
                                }
                            )
                            {
                                ReturnTypeMetadata = TypeMetadata.I32,
                            }
                        )
                        {
                            ReturnTypeMetadata = TypeMetadata.I32,
                        }
                    )
                ])
            )
            {
                Metadata = new FunctionMetadata(
                    "test",
                    [new ParameterMetadata("x", TypeMetadata.I32)],
                    new FunctionTypeMetadata([TypeMetadata.I32], TypeMetadata.Void)
                )
            }
        ]);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        Assert.That(tree, Is.EqualTo(expected));
    }
}
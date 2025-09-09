using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using static Trilang.Semantics.Model.BinaryExpressionKind;
using Type = Trilang.Semantics.Model.Type;

namespace Tri.Tests.Lower;

public class ReplaceCompoundAssignmentsTests
{
    private static SemanticTree Parse(string code)
    {
        var parser = new Parser();
        var tree = parser.Parse(code);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        return semanticTree;
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
        var parameterMetadata = new ParameterMetadata("x", TypeMetadata.I32);
        var expected = new SemanticTree([
            new FunctionDeclaration(
                "test",
                [
                    new Parameter("x", new Type("i32") { Metadata = TypeMetadata.I32 })
                    {
                        Metadata = parameterMetadata,
                    }
                ],
                new Type("void") { Metadata = TypeMetadata.Void },
                new BlockStatement([
                    new ExpressionStatement(
                        new BinaryExpression(
                            Assignment,
                            new MemberAccessExpression("x")
                            {
                                Reference = parameterMetadata,
                                AccessKind = MemberAccessKind.Write,
                            },
                            new BinaryExpression(
                                kind,
                                new MemberAccessExpression("x")
                                {
                                    Reference = parameterMetadata,
                                    AccessKind = MemberAccessKind.Read,
                                },
                                new LiteralExpression(LiteralExpressionKind.Integer, 1)
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
                    [parameterMetadata],
                    new FunctionTypeMetadata([TypeMetadata.I32], TypeMetadata.Void)
                )
            }
        ]);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }
}
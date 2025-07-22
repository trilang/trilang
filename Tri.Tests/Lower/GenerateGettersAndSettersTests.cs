using Trilang;
using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Lower;

public class GenerateGettersAndSettersTests
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
    public void GenerateGetterAndSetterTest()
    {
        const string code =
            """
            public type Test {
                count: i32;
            }
            """;
        var tree = Parse(code);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var expected = new PropertyDeclarationNode(
            "count",
            new TypeNode("i32") { Metadata = TypeMetadata.I32 },
            new PropertyGetterNode(AccessModifier.Public, new BlockStatementNode([
                new ReturnStatementNode(
                    new MemberAccessExpressionNode(MemberAccessExpressionNode.Field)
                    {
                        ReturnTypeMetadata = TypeMetadata.I32,
                    }
                )
            ])),
            new PropertySetterNode(AccessModifier.Private, new BlockStatementNode([
                new ExpressionStatementNode(
                    new BinaryExpressionNode(
                        BinaryExpressionKind.Assignment,
                        new MemberAccessExpressionNode(MemberAccessExpressionNode.Field)
                        {
                            ReturnTypeMetadata = TypeMetadata.I32,
                        },
                        new MemberAccessExpressionNode(MemberAccessExpressionNode.Value)
                        {
                            ReturnTypeMetadata = TypeMetadata.I32,
                        }
                    )
                    {
                        ReturnTypeMetadata = TypeMetadata.I32,
                    }
                )
            ]))
        )
        {
            Metadata = new PropertyMetadata(
                new TypeMetadata("Test"),
                "count",
                TypeMetadata.I32
            ),
        };

        var property = tree.Find<PropertyDeclarationNode>();
        Assert.That(property, Is.Not.Null);
        Assert.That(property, Is.EqualTo(expected));
    }

    [Test]
    public void GenerateGetterAndSetterBodyTest()
    {
        const string code =
            """
            public type Test {
                count: i32 { public get; public set; }
            }
            """;
        var tree = Parse(code);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var expected = new PropertyDeclarationNode(
            "count",
            new TypeNode("i32") { Metadata = TypeMetadata.I32 },
            new PropertyGetterNode(AccessModifier.Public, new BlockStatementNode([
                new ReturnStatementNode(
                    new MemberAccessExpressionNode(MemberAccessExpressionNode.Field)
                    {
                        ReturnTypeMetadata = TypeMetadata.I32,
                    }
                )
            ])),
            new PropertySetterNode(AccessModifier.Public, new BlockStatementNode([
                new ExpressionStatementNode(
                    new BinaryExpressionNode(
                        BinaryExpressionKind.Assignment,
                        new MemberAccessExpressionNode(MemberAccessExpressionNode.Field)
                        {
                            ReturnTypeMetadata = TypeMetadata.I32,
                        },
                        new MemberAccessExpressionNode(MemberAccessExpressionNode.Value)
                        {
                            ReturnTypeMetadata = TypeMetadata.I32,
                        }
                    )
                    {
                        ReturnTypeMetadata = TypeMetadata.I32,
                    }
                )
            ]))
        )
        {
            Metadata = new PropertyMetadata(
                new TypeMetadata("Test"),
                "count",
                TypeMetadata.I32,
                AccessModifierMetadata.Public,
                AccessModifierMetadata.Public
            ),
        };

        var property = tree.Find<PropertyDeclarationNode>();
        Assert.That(property, Is.Not.Null);
        Assert.That(property, Is.EqualTo(expected));
    }

    [Test]
    public void ExistingGetterAndSetterUnchangedTest()
    {
        const string code =
            """
            public type Test {
                count: i32 {
                    public get {
                        return field;
                    }
                    public set {
                        field = value;
                    }
                }
            }
            """;
        var tree = Parse(code);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var expected = new PropertyDeclarationNode(
            "count",
            new TypeNode("i32") { Metadata = TypeMetadata.I32 },
            new PropertyGetterNode(AccessModifier.Public, new BlockStatementNode([
                new ReturnStatementNode(
                    new MemberAccessExpressionNode(MemberAccessExpressionNode.Field)
                    {
                        ReturnTypeMetadata = TypeMetadata.I32,
                    }
                )
            ])),
            new PropertySetterNode(AccessModifier.Public, new BlockStatementNode([
                new ExpressionStatementNode(
                    new BinaryExpressionNode(
                        BinaryExpressionKind.Assignment,
                        new MemberAccessExpressionNode(MemberAccessExpressionNode.Field)
                        {
                            ReturnTypeMetadata = TypeMetadata.I32,
                        },
                        new MemberAccessExpressionNode(MemberAccessExpressionNode.Value)
                        {
                            ReturnTypeMetadata = TypeMetadata.I32,
                        }
                    )
                    {
                        ReturnTypeMetadata = TypeMetadata.I32,
                    }
                )
            ]))
        )
        {
            Metadata = new PropertyMetadata(
                new TypeMetadata("Test"),
                "count",
                TypeMetadata.I32,
                AccessModifierMetadata.Public,
                AccessModifierMetadata.Public
            ),
        };

        var property = tree.Find<PropertyDeclarationNode>();
        Assert.That(property, Is.Not.Null);
        Assert.That(property, Is.EqualTo(expected));
    }
}
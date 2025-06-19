using Trilang;
using Trilang.Lower;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Lower;

public class AddThisAsParameterToInstanceMethodsTests
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
    public void AddThisParameterToMethodTest()
    {
        const string code =
            """
            public type Test {
                public test(a: i32): void { }
            }
            """;
        var tree = Parse(code);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var expected = new MethodDeclarationNode(
            AccessModifier.Public,
            false,
            "test",
            [
                new ParameterNode(MemberAccessExpressionNode.This, new TypeNode("Test")),
                new ParameterNode("a", new TypeNode("i32")),
            ],
            new TypeNode("void"),
            new BlockStatementNode([])
        );

        var returnStatement = tree.Find<MethodDeclarationNode>();
        Assert.That(returnStatement, Is.EqualTo(expected));
    }

    [Test]
    public void AddThisParameterToCtorTest()
    {
        const string code =
            """
            public type Test {
                public constructor(a: i32) { }
            }
            """;
        var tree = Parse(code);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var expected = new ConstructorDeclarationNode(
            AccessModifier.Public,
            [
                new ParameterNode(MemberAccessExpressionNode.This, new TypeNode("Test")),
                new ParameterNode("a", new TypeNode("i32")),
            ],
            new BlockStatementNode([])
        );

        var returnStatement = tree.Find<ConstructorDeclarationNode>();
        Assert.That(returnStatement, Is.EqualTo(expected));
    }

    [Test]
    public void AddThisParameterToGetterAndSetterTest()
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

        var expected = new ParameterNode(MemberAccessExpressionNode.This, new TypeNode("Test"));

        var getter = tree.Find<PropertyGetterNode>();
        Assert.That(getter, Is.Not.Null);
        Assert.That(getter.Parameters, Has.Count.EqualTo(1));
        Assert.That(getter.Parameters, Has.One.EqualTo(expected));

        var setter = tree.Find<PropertySetterNode>();
        Assert.That(setter, Is.Not.Null);
        Assert.That(setter.Parameters, Has.Count.EqualTo(2));
        Assert.That(setter.Parameters, Has.One.EqualTo(expected));
    }
}
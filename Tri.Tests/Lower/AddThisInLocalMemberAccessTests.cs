using Trilang;
using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Lower;

public class AddThisInLocalMemberAccessTests
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
    public void AddThisBeforePropertyTest()
    {
        const string code =
            """
            public type Test {
                count: i32;

                public getCount(): i32 {
                    return count;
                }
            }
            """;
        var tree = Parse(code);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var typeMetadata = new TypeMetadata("Test");
        var propertyMetadata = new PropertyMetadata(
            typeMetadata,
            "count",
            TypeMetadata.I32
        );
        var expected = new CallExpressionNode(
            new MemberAccessExpressionNode(
                new MemberAccessExpressionNode(MemberAccessExpressionNode.This)
                {
                    Reference = typeMetadata,
                    AccessKind = PropertyAccessKind.Read,
                },
                "<>_get_count")
            {
                Reference = propertyMetadata.Getter,
                AccessKind = PropertyAccessKind.Read,
            },
            []
        );

        var method = tree.Find<MethodDeclarationNode>();
        var returnStatement = method?.Body.Find<ReturnStatementNode>();
        Assert.That(returnStatement, Is.Not.Null);
        Assert.That(returnStatement.Expression, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }

    [Test]
    public void AddThisBeforeMethodTest()
    {
        const string code =
            """
            public type Test {
                public print(): void { }

                public test(): void {
                    print();
                }
            }
            """;
        var tree = Parse(code);

        var lowering = new Lowering();
        lowering.Lower(tree, LoweringOptions.Default);

        var typeMetadata = new TypeMetadata("Test");
        var methodMetadata = new MethodMetadata(
            typeMetadata,
            AccessModifierMetadata.Public,
            false,
            "print",
            [],
            new FunctionTypeMetadata([], TypeMetadata.Void)
        );

        var expected = new MemberAccessExpressionNode(
            new MemberAccessExpressionNode(MemberAccessExpressionNode.This)
            {
                Reference = typeMetadata,
                AccessKind = PropertyAccessKind.Read,
            },
            "print")
        {
            Reference = methodMetadata,
            AccessKind = PropertyAccessKind.Read,
        };

        var returnStatement = tree.Find<CallExpressionNode>();
        Assert.That(returnStatement, Is.Not.Null);
        Assert.That(returnStatement.Member, Is.EqualTo(expected).Using(SyntaxComparer.Instance));
    }
}
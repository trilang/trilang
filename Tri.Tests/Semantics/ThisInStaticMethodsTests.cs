using Tri.Tests.Builders;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class ThisInStaticMethodsTests
{
    [Test]
    public void ThisInStaticMethodsTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Test", t => t
                .DefineMethod("test", m => m
                    .Static()
                    .ReturnType("Test")
                    .Body(body => body
                        .Return(r => r.MemberAccess(MemberAccessExpressionNode.This)))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'this' keyword is not allowed in static methods."));
    }
}
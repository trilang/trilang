using Tri.Tests.Builders;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class VariableUsedBeforeDeclaredTests
{
    [Test]
    public void VariableUsedAfterDeclarationTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .Body(body => body
                    .DefineVariable("a", "i32", exp => exp.Number(1))
                    .Statement(exp => exp.MemberAccess("a"))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.Nothing);
    }

    [Test]
    public void ParameterUsedAfterDeclarationTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .DefineParameter("a", t => t.Type("i32"))
                .Body(body => body
                    .Statement(exp => exp.MemberAccess("a"))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.Nothing);
    }

    [Test]
    public void VariableUsedBeforeDeclarationTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .Body(body => body
                    .Statement(exp => exp.MemberAccess("a"))
                    .DefineVariable("a", "i32", exp => exp.Number(1))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'a' variable used before declaration."));
    }

    [Test]
    public void VariableInBlockUsedBeforeDeclarationTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .Body(body => body
                    .Block(block => block
                        .Statement(exp => exp.MemberAccess("a")))
                    .DefineVariable("a", "i32", exp => exp.Number(1))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'a' variable used before declaration."));
    }

    [Test]
    public void VariableInDifferentBlocksTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("main", builder => builder
                .Body(body => body
                    .Block(block => block.DefineVariable("a", "i32", exp => exp.Number(1)))
                    .Block(block => block.Statement(exp => exp.MemberAccess("a")))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Unknown symbol: a"));
    }

    [Test]
    public void VariableInDeclaredInDifferentFunctionTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("test", builder => builder
                .Body(body => body
                    .DefineVariable("a", "i32", exp => exp.Number(1))))
            .DefineFunction("main", builder => builder
                .Body(body => body
                    .Statement(exp => exp.MemberAccess("a"))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Unknown symbol: a"));
    }

    [Test]
    public void VariableInParentScopeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("test", builder => builder
                .ReturnType("i32")
                .Body(body => body
                    .DefineVariable("a", "i32", exp => exp.Number(1))
                    .Block(block => block
                        .Return(exp => exp.MemberAccess("a")))))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.Nothing);
    }
}
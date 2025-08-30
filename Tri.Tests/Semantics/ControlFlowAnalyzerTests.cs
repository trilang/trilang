using Trilang;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;
using Trilang.Semantics.Passes.ControlFlow;

namespace Tri.Tests.Semantics;

public class ControlFlowAnalyzerTests
{
    private static SyntaxTree Parse(string code)
    {
        var parser = new Parser();
        var tree = parser.Parse(code);

        return tree;
    }

    [Test]
    public void BuildControlFlowGraphTest()
    {
        const string code =
            """
            function add(a: i32, b: i32): i32 {
                return a + b;
            }
            """;
        var tree = Parse(code);

        var semantic = new SemanticAnalysis();
        var (_, _, graphs) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var entry = new SemanticBlock("entry", [
            tree.Find<ReturnStatementNode>()!
        ]);
        var expected = new ControlFlowGraph(entry, entry);
        var function = new FunctionMetadata(
            "add",
            [new ParameterMetadata("a", TypeMetadata.I32), new ParameterMetadata("b", TypeMetadata.I32)],
            new FunctionTypeMetadata([TypeMetadata.I32, TypeMetadata.I32], TypeMetadata.I32)
        );

        Assert.That(graphs.Functions, Has.Count.EqualTo(1));
        Assert.That(
            graphs.Functions,
            Contains.Key(function).WithValue(expected).Using(ControlFlowGraphComparer.Instance));
    }

    [Test]
    public void IfStatementControlGraphTest()
    {
        const string code =
            """
            function test(): void {
                if (true) {
                    return;
                }
            }
            """;
        var tree = Parse(code);

        var semantic = new SemanticAnalysis();
        var (_, _, graphs) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var entry = new SemanticBlock("entry", [
            tree.Find<IfStatementNode>()!
        ]);
        var thenBlock = new SemanticBlock("then_0", [
            tree.Find<ReturnStatementNode>()!
        ]);
        var endBlock = new SemanticBlock("endif_0");

        entry.AddNext(thenBlock);
        entry.AddNext(endBlock);
        thenBlock.AddNext(endBlock);

        var expected = new ControlFlowGraph(entry, endBlock);
        var function = new FunctionMetadata(
            "test",
            [],
            new FunctionTypeMetadata([], TypeMetadata.Void)
        );

        Assert.That(graphs.Functions, Has.Count.EqualTo(1));
        Assert.That(
            graphs.Functions,
            Contains.Key(function).WithValue(expected).Using(ControlFlowGraphComparer.Instance));
    }

    [Test]
    public void IfStatementWithElseControlGraphTest()
    {
        const string code =
            """
            function test(): void {
                if (true) {
                    return;
                } else {
                    return;
                }
            }
            """;
        var tree = Parse(code);

        var semantic = new SemanticAnalysis();
        var (_, _, graphs) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var returnStatements = tree.Where<ReturnStatementNode>().ToList();

        var entry = new SemanticBlock("entry", [
            tree.Find<IfStatementNode>()!
        ]);
        var thenBlock = new SemanticBlock("then_0", [returnStatements[0]]);
        var elseBlock = new SemanticBlock("else_0", [returnStatements[1]]);
        var endBlock = new SemanticBlock("endif_0");

        entry.AddNext(thenBlock);
        entry.AddNext(elseBlock);
        thenBlock.AddNext(endBlock);
        elseBlock.AddNext(endBlock);

        var expected = new ControlFlowGraph(entry, endBlock);
        var function = new FunctionMetadata(
            "test",
            [],
            new FunctionTypeMetadata([], TypeMetadata.Void)
        );

        Assert.That(graphs.Functions, Has.Count.EqualTo(1));
        Assert.That(
            graphs.Functions,
            Contains.Key(function).WithValue(expected).Using(ControlFlowGraphComparer.Instance));
    }

    [Test]
    public void NestedIfStatementControlGraphTest()
    {
        const string code =
            """
            function test(): void {
                if (true) {
                    if (false) {
                        return;
                    }
                }
            }
            """;
        var tree = Parse(code);

        var semantic = new SemanticAnalysis();
        var (_, _, graphs) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var ifs = tree.Where<IfStatementNode>().ToList();
        var outerIf = ifs[0];
        var innerIf = ifs[1];
        var returnStatement = tree.Find<ReturnStatementNode>()!;

        var entry = new SemanticBlock("entry", [outerIf]);
        var outerThenBlock = new SemanticBlock("then_0", [innerIf]);
        var innerThenBlock = new SemanticBlock("then_1", [returnStatement]);
        var outerEndBlock = new SemanticBlock("endif_0");
        var innerEndBlock = new SemanticBlock("endif_1");

        entry.AddNext(outerThenBlock);
        entry.AddNext(outerEndBlock);
        outerThenBlock.AddNext(innerThenBlock);
        outerThenBlock.AddNext(innerEndBlock);
        innerThenBlock.AddNext(innerEndBlock);
        innerEndBlock.AddNext(outerEndBlock);

        var expected = new ControlFlowGraph(entry, outerEndBlock);
        var function = new FunctionMetadata(
            "test",
            [],
            new FunctionTypeMetadata([], TypeMetadata.Void)
        );

        Assert.That(graphs.Functions, Has.Count.EqualTo(1));
        Assert.That(
            graphs.Functions,
            Contains.Key(function).WithValue(expected).Using(ControlFlowGraphComparer.Instance));
    }

    [Test]
    public void WhileControlGraphTest()
    {
        const string code =
            """
            function test(): void {
                while (true) {
                    return;
                }
            }
            """;
        var tree = Parse(code);

        var semantic = new SemanticAnalysis();
        var (_, _, graphs) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var entry = new SemanticBlock("entry");
        var conditionBlock = new SemanticBlock("loopcond_0", [
            tree.Find<WhileNode>()!
        ]);
        var bodyBlock = new SemanticBlock("loopbody_0", [
            tree.Find<ReturnStatementNode>()!
        ]);
        var endBlock = new SemanticBlock("loopend_0");

        entry.AddNext(conditionBlock);
        conditionBlock.AddNext(bodyBlock);
        conditionBlock.AddNext(endBlock);
        bodyBlock.AddNext(conditionBlock);

        var expected = new ControlFlowGraph(entry, endBlock);
        var function = new FunctionMetadata(
            "test",
            [],
            new FunctionTypeMetadata([], TypeMetadata.Void)
        );

        Assert.That(graphs.Functions, Has.Count.EqualTo(1));
        Assert.That(
            graphs.Functions,
            Contains.Key(function).WithValue(expected).Using(ControlFlowGraphComparer.Instance));
    }

    [Test]
    public void NestedWhileControlGraphTest()
    {
        const string code =
            """
            function test(): void {
                while (true) {
                    while (false) {
                        return;
                    }
                }
            }
            """;
        var tree = Parse(code);

        var semantic = new SemanticAnalysis();
        var (_, _, graphs) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var whileNodes = tree.Where<WhileNode>().ToArray();
        var outerWhile = whileNodes[0];
        var innerWhile = whileNodes[1];
        var returnStatement = tree.Find<ReturnStatementNode>()!;

        var entry = new SemanticBlock("entry");
        var outerConditionBlock = new SemanticBlock("loopcond_0", [outerWhile]);
        var outerBodyBlock = new SemanticBlock("loopbody_0");
        var innerConditionBlock = new SemanticBlock("loopcond_1", [innerWhile]);
        var innerBodyBlock = new SemanticBlock("loopbody_1", [returnStatement]);
        var outerEndBlock = new SemanticBlock("loopend_0");
        var innerEndBlock = new SemanticBlock("loopend_1");

        entry.AddNext(outerConditionBlock);
        outerConditionBlock.AddNext(outerBodyBlock);
        outerConditionBlock.AddNext(outerEndBlock);
        outerBodyBlock.AddNext(innerConditionBlock);
        innerConditionBlock.AddNext(innerBodyBlock);
        innerConditionBlock.AddNext(innerEndBlock);
        innerBodyBlock.AddNext(innerConditionBlock);
        innerEndBlock.AddNext(outerEndBlock);

        var expected = new ControlFlowGraph(entry, outerEndBlock);
        var function = new FunctionMetadata(
            "test",
            [],
            new FunctionTypeMetadata([], TypeMetadata.Void)
        );

        Assert.That(graphs.Functions, Has.Count.EqualTo(1));
        Assert.That(
            graphs.Functions,
            Contains.Key(function).WithValue(expected).Using(ControlFlowGraphComparer.Instance));
    }

    [Test]
    public void BreakControlGraphTest()
    {
        const string code =
            """
            function test(): void {
                while (true) {
                    break;
                }
            }
            """;
        var tree = Parse(code);

        var semantic = new SemanticAnalysis();
        var (_, _, graphs) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var whileStatement = tree.Find<WhileNode>()!;
        var breakStatement = tree.Find<BreakNode>()!;

        var entry = new SemanticBlock("entry");
        var conditionBlock = new SemanticBlock("loopcond_0", [whileStatement]);
        var bodyBlock = new SemanticBlock("loopbody_0", [breakStatement]);
        var endBlock = new SemanticBlock("loopend_0");

        entry.AddNext(conditionBlock);
        conditionBlock.AddNext(bodyBlock);
        conditionBlock.AddNext(endBlock);
        bodyBlock.AddNext(endBlock);

        var expected = new ControlFlowGraph(entry, endBlock);
        var function = new FunctionMetadata(
            "test",
            [],
            new FunctionTypeMetadata([], TypeMetadata.Void)
        );

        Assert.That(graphs.Functions, Has.Count.EqualTo(1));
        Assert.That(
            graphs.Functions,
            Contains.Key(function).WithValue(expected).Using(ControlFlowGraphComparer.Instance));
    }

    [Test]
    public void NestedLoopBreakControlGraphTest()
    {
        const string code =
            """
            function test(): void {
                while (true) {
                    while (false) {
                        break;
                    }
                }
            }
            """;
        var tree = Parse(code);

        var semantic = new SemanticAnalysis();
        var (_, _, graphs) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var outerWhile = tree.Find<WhileNode>()!;
        var innerWhile = tree.Where<WhileNode>().Skip(1).First();
        var breakStatement = tree.Find<BreakNode>()!;

        var entry = new SemanticBlock("entry");
        var outerConditionBlock = new SemanticBlock("loopcond_0", [outerWhile]);
        var outerBodyBlock = new SemanticBlock("loopbody_0");
        var innerConditionBlock = new SemanticBlock("loopcond_1", [innerWhile]);
        var innerBodyBlock = new SemanticBlock("loopbody_1", [breakStatement]);
        var outerEndBlock = new SemanticBlock("loopend_0");
        var innerEndBlock = new SemanticBlock("loopend_1");

        entry.AddNext(outerConditionBlock);
        outerConditionBlock.AddNext(outerBodyBlock);
        outerConditionBlock.AddNext(outerEndBlock);
        outerBodyBlock.AddNext(innerConditionBlock);
        innerConditionBlock.AddNext(innerBodyBlock);
        innerConditionBlock.AddNext(innerEndBlock);
        innerBodyBlock.AddNext(outerConditionBlock);
        innerEndBlock.AddNext(outerEndBlock);

        var expected = new ControlFlowGraph(entry, outerEndBlock);
        var function = new FunctionMetadata(
            "test",
            [],
            new FunctionTypeMetadata([], TypeMetadata.Void)
        );

        Assert.That(graphs.Functions, Has.Count.EqualTo(1));
        Assert.That(
            graphs.Functions,
            Contains.Key(function).WithValue(expected).Using(ControlFlowGraphComparer.Instance));
    }

    [Test]
    public void ContinueControlGraphTest()
    {
        const string code =
            """
            function test(): void {
                while (true) {
                    continue;
                }
            }
            """;
        var tree = Parse(code);

        var semantic = new SemanticAnalysis();
        var (_, _, graphs) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var whileStatement = tree.Find<WhileNode>()!;
        var continueStatement = tree.Find<ContinueNode>()!;

        var entry = new SemanticBlock("entry");
        var conditionBlock = new SemanticBlock("loopcond_0", [whileStatement]);
        var bodyBlock = new SemanticBlock("loopbody_0", [continueStatement]);
        var endBlock = new SemanticBlock("loopend_0");

        entry.AddNext(conditionBlock);
        conditionBlock.AddNext(bodyBlock);
        conditionBlock.AddNext(endBlock);
        bodyBlock.AddNext(conditionBlock);

        var expected = new ControlFlowGraph(entry, endBlock);
        var function = new FunctionMetadata(
            "test",
            [],
            new FunctionTypeMetadata([], TypeMetadata.Void)
        );

        Assert.That(graphs.Functions, Has.Count.EqualTo(1));
        Assert.That(
            graphs.Functions,
            Contains.Key(function).WithValue(expected).Using(ControlFlowGraphComparer.Instance));
    }

    [Test]
    public void NestedLoopContinueControlGraphTest()
    {
        const string code =
            """
            function test(): void {
                while (true) {
                    while (false) {
                        continue;
                    }
                }
            }
            """;
        var tree = Parse(code);

        var semantic = new SemanticAnalysis();
        var (_, _, graphs) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var outerWhile = tree.Find<WhileNode>()!;
        var innerWhile = tree.Where<WhileNode>().Skip(1).First();
        var continueStatement = tree.Find<ContinueNode>()!;

        var entry = new SemanticBlock("entry");
        var outerConditionBlock = new SemanticBlock("loopcond_0", [outerWhile]);
        var outerBodyBlock = new SemanticBlock("loopbody_0");
        var innerConditionBlock = new SemanticBlock("loopcond_1", [innerWhile]);
        var innerBodyBlock = new SemanticBlock("loopbody_1", [continueStatement]);
        var outerEndBlock = new SemanticBlock("loopend_0");
        var innerEndBlock = new SemanticBlock("loopend_1");

        entry.AddNext(outerConditionBlock);
        outerConditionBlock.AddNext(outerBodyBlock);
        outerConditionBlock.AddNext(outerEndBlock);
        outerBodyBlock.AddNext(innerConditionBlock);
        innerConditionBlock.AddNext(innerBodyBlock);
        innerConditionBlock.AddNext(innerEndBlock);
        innerBodyBlock.AddNext(outerConditionBlock);
        innerEndBlock.AddNext(outerEndBlock);

        var expected = new ControlFlowGraph(entry, outerEndBlock);
        var function = new FunctionMetadata(
            "test",
            [],
            new FunctionTypeMetadata([], TypeMetadata.Void)
        );

        Assert.That(graphs.Functions, Has.Count.EqualTo(1));
        Assert.That(
            graphs.Functions,
            Contains.Key(function).WithValue(expected).Using(ControlFlowGraphComparer.Instance));
    }

    [Test]
    public void IfInsideWhileControlGraphTest()
    {
        const string code =
            """
            function test(): void {
                while (true) {
                    if (false) {
                        return;
                    }
                }
            }
            """;
        var tree = Parse(code);

        var semantic = new SemanticAnalysis();
        var (_, _, graphs) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var whileStatement = tree.Find<WhileNode>()!;
        var ifStatement = tree.Find<IfStatementNode>()!;
        var returnStatement = tree.Find<ReturnStatementNode>()!;

        var entry = new SemanticBlock("entry");
        var loopcondBlock = new SemanticBlock("loopcond_0", [whileStatement]);
        var loopbodyBlock = new SemanticBlock("loopbody_0", [ifStatement]);
        var thenBlock = new SemanticBlock("then_0", [returnStatement]);
        var endIfBlock = new SemanticBlock("endif_0");
        var loopendBlock = new SemanticBlock("loopend_0");

        entry.AddNext(loopcondBlock);
        loopcondBlock.AddNext(loopbodyBlock);
        loopcondBlock.AddNext(loopendBlock);
        loopbodyBlock.AddNext(thenBlock);
        loopbodyBlock.AddNext(endIfBlock);
        thenBlock.AddNext(endIfBlock);
        endIfBlock.AddNext(loopcondBlock);

        var expected = new ControlFlowGraph(entry, loopendBlock);
        var function = new FunctionMetadata(
            "test",
            [],
            new FunctionTypeMetadata([], TypeMetadata.Void)
        );

        Assert.That(graphs.Functions, Has.Count.EqualTo(1));
        Assert.That(
            graphs.Functions,
            Contains.Key(function).WithValue(expected).Using(ControlFlowGraphComparer.Instance));
    }

    [Test]
    public void NestedIfWithElseTest()
    {
        const string code =
            """
            function test(a: i32): i32 {
                if (a > 0) {
                    if (a > 10) {
                        return 1;
                    } else {
                        return 2;
                    }
                } else {
                    return 0;
                }
            }
            """;
        var tree = Parse(code);

        var semantic = new SemanticAnalysis();
        var (_, _, graphs) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var ifs = tree.Where<IfStatementNode>().ToList();
        var returns = tree.Where<ReturnStatementNode>().ToList();

        var entry = new SemanticBlock("entry", [ifs[0]]);
        var then0Block = new SemanticBlock("then_0", [ifs[1]]);
        var else0Block = new SemanticBlock("else_0", [returns[0]]);
        var end0Block = new SemanticBlock("endif_0");
        var then1Block = new SemanticBlock("then_1", [returns[1]]);
        var else1Block = new SemanticBlock("else_1", [returns[2]]);
        var end1Block = new SemanticBlock("endif_1");

        entry.AddNext(then0Block);
        entry.AddNext(else0Block);
        then0Block.AddNext(then1Block);
        then0Block.AddNext(else1Block);
        then1Block.AddNext(end1Block);
        else1Block.AddNext(end1Block);
        end1Block.AddNext(end0Block);
        else0Block.AddNext(end0Block);

        var expected = new ControlFlowGraph(entry, end0Block);
        var function = new FunctionMetadata(
            "test",
            [new ParameterMetadata("a", TypeMetadata.I32)],
            new FunctionTypeMetadata([TypeMetadata.I32], TypeMetadata.I32)
        );

        Assert.That(graphs.Functions, Has.Count.EqualTo(1));
        Assert.That(
            graphs.Functions,
            Contains.Key(function).WithValue(expected).Using(ControlFlowGraphComparer.Instance));
    }
}
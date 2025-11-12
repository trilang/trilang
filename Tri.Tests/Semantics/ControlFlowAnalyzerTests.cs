using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using Trilang.Semantics.Passes.ControlFlow;

namespace Tri.Tests.Semantics;

public class ControlFlowAnalyzerTests
{
    private static readonly SourceFile file = new SourceFile("test.tri");

    private static (SyntaxTree, DiagnosticCollection) Parse(string code)
    {
        var diagnostics = new DiagnosticCollection();

        var lexer = new Lexer();
        var lexerOptions = new LexerOptions(new LexerDiagnosticReporter(diagnostics, file));
        var tokens = lexer.Tokenize(code, lexerOptions);

        var parser = new Parser();
        var parserOptions = new ParserOptions(file, new ParserDiagnosticReporter(diagnostics, file));
        var tree = parser.Parse(tokens, parserOptions);

        return (tree, diagnostics);
    }

    [Test]
    public void BuildControlFlowGraphTest()
    {
        const string code =
            """
            public add(a: i32, b: i32): i32 {
                return a + b;
            }
            """;
        var (tree, diagnostics) = Parse(code);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, graphs) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var returnStatement = semanticTree.Find<ReturnStatement>()!;
        var entry = new SemanticBlock("entry", (BlockStatement)returnStatement.Parent!, [
            returnStatement
        ]);
        var expected = new ControlFlowGraph(entry, entry);
        var function = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "add",
            [new ParameterMetadata(null, "a", TypeMetadata.I32), new ParameterMetadata(null, "b", TypeMetadata.I32)],
            new FunctionTypeMetadata(null, [TypeMetadata.I32, TypeMetadata.I32], TypeMetadata.I32),
            new FunctionGroupMetadata());

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
            public test(): void {
                if (true) {
                    return;
                }
            }
            """;
        var (tree, diagnostics) = Parse(code);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, graphs) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var block = semanticTree.Find<BlockStatement>()!;
        var returnStatement = semanticTree.Find<ReturnStatement>()!;
        var entry = new SemanticBlock("entry", block, [
            semanticTree.Find<IfStatement>()!
        ]);
        var thenBlock = new SemanticBlock("then_0", (BlockStatement)returnStatement.Parent!, [
            returnStatement
        ]);
        var endBlock = new SemanticBlock("endif_0", block);

        entry.AddNext(thenBlock);
        entry.AddNext(endBlock);
        thenBlock.AddNext(endBlock);

        var expected = new ControlFlowGraph(entry, endBlock);
        var function = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [],
            new FunctionTypeMetadata(null, [], TypeMetadata.Void),
            new FunctionGroupMetadata()
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
            public test(): void {
                if (true) {
                    return;
                } else {
                    return;
                }
            }
            """;
        var (tree, diagnostics) = Parse(code);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, graphs) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var returnStatements = semanticTree.Where<ReturnStatement>().ToList();
        var ifStatement = semanticTree.Find<IfStatement>()!;
        var entry = new SemanticBlock("entry", (BlockStatement)ifStatement.Parent!, [ifStatement]);
        var thenBlock = new SemanticBlock(
            "then_0",
            (BlockStatement)returnStatements[0].Parent!,
            [returnStatements[0]]
        );
        var elseBlock = new SemanticBlock(
            "else_0",
            (BlockStatement)returnStatements[1].Parent!,
            [returnStatements[1]]
        );
        var endBlock = new SemanticBlock("endif_0", (BlockStatement)ifStatement.Parent!);

        entry.AddNext(thenBlock);
        entry.AddNext(elseBlock);
        thenBlock.AddNext(endBlock);
        elseBlock.AddNext(endBlock);

        var expected = new ControlFlowGraph(entry, endBlock);
        var function = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [],
            new FunctionTypeMetadata(null, [], TypeMetadata.Void),
            new FunctionGroupMetadata()
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
            public test(): void {
                if (true) {
                    if (false) {
                        return;
                    }
                }
            }
            """;
        var (tree, diagnostics) = Parse(code);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, graphs) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var ifs = semanticTree.Where<IfStatement>().ToList();
        var outerIf = ifs[0];
        var innerIf = ifs[1];
        var returnStatement = semanticTree.Find<ReturnStatement>()!;

        var entry = new SemanticBlock("entry", (BlockStatement)outerIf.Parent!, [outerIf]);
        var outerThenBlock = new SemanticBlock("then_0", outerIf.Then, [innerIf]);
        var innerThenBlock = new SemanticBlock("then_1", innerIf.Then, [returnStatement]);
        var outerEndBlock = new SemanticBlock("endif_0", (BlockStatement)outerIf.Parent!);
        var innerEndBlock = new SemanticBlock("endif_1", (BlockStatement)innerIf.Parent!);

        entry.AddNext(outerThenBlock);
        entry.AddNext(outerEndBlock);
        outerThenBlock.AddNext(innerThenBlock);
        outerThenBlock.AddNext(innerEndBlock);
        innerThenBlock.AddNext(innerEndBlock);
        innerEndBlock.AddNext(outerEndBlock);

        var expected = new ControlFlowGraph(entry, outerEndBlock);
        var function = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [],
            new FunctionTypeMetadata(null, [], TypeMetadata.Void),
            new FunctionGroupMetadata()
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
            public test(): void {
                while (true) {
                    return;
                }
            }
            """;
        var (tree, diagnostics) = Parse(code);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, graphs) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var block = semanticTree.Find<BlockStatement>()!;
        var returnStatement = semanticTree.Find<ReturnStatement>()!;
        var entry = new SemanticBlock("entry", block);
        var conditionBlock = new SemanticBlock("loopcond_0", block, [
            semanticTree.Find<While>()!
        ]);
        var bodyBlock = new SemanticBlock("loopbody_0", (BlockStatement)returnStatement.Parent!, [
            returnStatement
        ]);
        var endBlock = new SemanticBlock("loopend_0", block);

        entry.AddNext(conditionBlock);
        conditionBlock.AddNext(bodyBlock);
        conditionBlock.AddNext(endBlock);
        bodyBlock.AddNext(conditionBlock);

        var expected = new ControlFlowGraph(entry, endBlock);
        var function = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [],
            new FunctionTypeMetadata(null, [], TypeMetadata.Void),
            new FunctionGroupMetadata()
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
            public test(): void {
                while (true) {
                    while (false) {
                        return;
                    }
                }
            }
            """;
        var (tree, diagnostics) = Parse(code);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, graphs) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var whileNodes = semanticTree.Where<While>().ToArray();
        var outerWhile = whileNodes[0];
        var innerWhile = whileNodes[1];
        var returnStatement = semanticTree.Find<ReturnStatement>()!;

        var entry = new SemanticBlock("entry", (BlockStatement)outerWhile.Parent!);
        var outerConditionBlock = new SemanticBlock("loopcond_0", (BlockStatement)outerWhile.Parent!, [outerWhile]);
        var outerBodyBlock = new SemanticBlock("loopbody_0", outerWhile.Body);
        var innerConditionBlock = new SemanticBlock("loopcond_1", outerWhile.Body, [innerWhile]);
        var innerBodyBlock = new SemanticBlock("loopbody_1", innerWhile.Body, [returnStatement]);
        var outerEndBlock = new SemanticBlock("loopend_0", (BlockStatement)outerWhile.Parent!);
        var innerEndBlock = new SemanticBlock("loopend_1", outerWhile.Body);

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
            null,
            AccessModifierMetadata.Public,
            "test",
            [],
            new FunctionTypeMetadata(null, [], TypeMetadata.Void),
            new FunctionGroupMetadata()
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
            public test(): void {
                while (true) {
                    break;
                }
            }
            """;
        var (tree, diagnostics) = Parse(code);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, graphs) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var whileStatement = semanticTree.Find<While>()!;
        var breakStatement = semanticTree.Find<Break>()!;

        var entry = new SemanticBlock("entry", (BlockStatement)whileStatement.Parent!);
        var conditionBlock = new SemanticBlock("loopcond_0", (BlockStatement)whileStatement.Parent!, [whileStatement]);
        var bodyBlock = new SemanticBlock("loopbody_0", whileStatement.Body, [breakStatement]);
        var endBlock = new SemanticBlock("loopend_0", (BlockStatement)whileStatement.Parent!);

        entry.AddNext(conditionBlock);
        conditionBlock.AddNext(bodyBlock);
        conditionBlock.AddNext(endBlock);
        bodyBlock.AddNext(endBlock);

        var expected = new ControlFlowGraph(entry, endBlock);
        var function = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [],
            new FunctionTypeMetadata(null, [], TypeMetadata.Void),
            new FunctionGroupMetadata()
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
            public test(): void {
                while (true) {
                    while (false) {
                        break;
                    }
                }
            }
            """;
        var (tree, diagnostics) = Parse(code);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, graphs) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var outerWhile = semanticTree.Find<While>()!;
        var innerWhile = semanticTree.Where<While>().Skip(1).First();
        var breakStatement = semanticTree.Find<Break>()!;

        var entry = new SemanticBlock("entry", (BlockStatement)outerWhile.Parent!);
        var outerConditionBlock = new SemanticBlock("loopcond_0", (BlockStatement)outerWhile.Parent!, [outerWhile]);
        var outerBodyBlock = new SemanticBlock("loopbody_0", outerWhile.Body);
        var innerConditionBlock = new SemanticBlock("loopcond_1", outerWhile.Body, [innerWhile]);
        var innerBodyBlock = new SemanticBlock("loopbody_1", innerWhile.Body, [breakStatement]);
        var outerEndBlock = new SemanticBlock("loopend_0", (BlockStatement)outerWhile.Parent!);
        var innerEndBlock = new SemanticBlock("loopend_1", outerWhile.Body);

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
            null,
            AccessModifierMetadata.Public,
            "test",
            [],
            new FunctionTypeMetadata(null, [], TypeMetadata.Void),
            new FunctionGroupMetadata()
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
            public test(): void {
                while (true) {
                    continue;
                }
            }
            """;
        var (tree, diagnostics) = Parse(code);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, graphs) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var whileStatement = semanticTree.Find<While>()!;
        var continueStatement = semanticTree.Find<Continue>()!;

        var entry = new SemanticBlock("entry", (BlockStatement)whileStatement.Parent!);
        var conditionBlock = new SemanticBlock("loopcond_0", (BlockStatement)whileStatement.Parent!, [whileStatement]);
        var bodyBlock = new SemanticBlock("loopbody_0", whileStatement.Body, [continueStatement]);
        var endBlock = new SemanticBlock("loopend_0", (BlockStatement)whileStatement.Parent!);

        entry.AddNext(conditionBlock);
        conditionBlock.AddNext(bodyBlock);
        conditionBlock.AddNext(endBlock);
        bodyBlock.AddNext(conditionBlock);

        var expected = new ControlFlowGraph(entry, endBlock);
        var function = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [],
            new FunctionTypeMetadata(null, [], TypeMetadata.Void),
            new FunctionGroupMetadata()
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
            public test(): void {
                while (true) {
                    while (false) {
                        continue;
                    }
                }
            }
            """;
        var (tree, diagnostics) = Parse(code);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, graphs) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var outerWhile = semanticTree.Find<While>()!;
        var innerWhile = semanticTree.Where<While>().Skip(1).First();
        var continueStatement = semanticTree.Find<Continue>()!;

        var entry = new SemanticBlock("entry", (BlockStatement)outerWhile.Parent!);
        var outerConditionBlock = new SemanticBlock("loopcond_0", (BlockStatement)outerWhile.Parent!, [outerWhile]);
        var outerBodyBlock = new SemanticBlock("loopbody_0", outerWhile.Body);
        var innerConditionBlock = new SemanticBlock("loopcond_1", outerWhile.Body, [innerWhile]);
        var innerBodyBlock = new SemanticBlock("loopbody_1", innerWhile.Body, [continueStatement]);
        var outerEndBlock = new SemanticBlock("loopend_0", (BlockStatement)outerWhile.Parent!);
        var innerEndBlock = new SemanticBlock("loopend_1", outerWhile.Body);

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
            null,
            AccessModifierMetadata.Public,
            "test",
            [],
            new FunctionTypeMetadata(null, [], TypeMetadata.Void),
            new FunctionGroupMetadata()
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
            public test(): void {
                while (true) {
                    if (false) {
                        return;
                    }
                }
            }
            """;
        var (tree, diagnostics) = Parse(code);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, graphs) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var whileStatement = semanticTree.Find<While>()!;
        var ifStatement = semanticTree.Find<IfStatement>()!;
        var returnStatement = semanticTree.Find<ReturnStatement>()!;

        var entry = new SemanticBlock("entry", (BlockStatement)whileStatement.Parent!);
        var loopcondBlock = new SemanticBlock("loopcond_0", (BlockStatement)whileStatement.Parent!, [whileStatement]);
        var loopbodyBlock = new SemanticBlock("loopbody_0", whileStatement.Body, [ifStatement]);
        var thenBlock = new SemanticBlock("then_0", ifStatement.Then, [returnStatement]);
        var endIfBlock = new SemanticBlock("endif_0", whileStatement.Body);
        var loopendBlock = new SemanticBlock("loopend_0", (BlockStatement)whileStatement.Parent!);

        entry.AddNext(loopcondBlock);
        loopcondBlock.AddNext(loopbodyBlock);
        loopcondBlock.AddNext(loopendBlock);
        loopbodyBlock.AddNext(thenBlock);
        loopbodyBlock.AddNext(endIfBlock);
        thenBlock.AddNext(endIfBlock);
        endIfBlock.AddNext(loopcondBlock);

        var expected = new ControlFlowGraph(entry, loopendBlock);
        var function = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [],
            new FunctionTypeMetadata(null, [], TypeMetadata.Void),
            new FunctionGroupMetadata()
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
            public test(a: i32): i32 {
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
        var (tree, diagnostics) = Parse(code);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, graphs) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var ifs = semanticTree.Where<IfStatement>().ToList();
        var returns = semanticTree.Where<ReturnStatement>().ToList();

        var entry = new SemanticBlock("entry", (BlockStatement)ifs[0].Parent!, [ifs[0]]);
        var then0Block = new SemanticBlock("then_0", ifs[0].Then, [ifs[1]]);
        var else0Block = new SemanticBlock("else_0", ifs[0].Else!, [returns[0]]);
        var end0Block = new SemanticBlock("endif_0", (BlockStatement)ifs[0].Parent!);
        var then1Block = new SemanticBlock("then_1", ifs[1].Then, [returns[1]]);
        var else1Block = new SemanticBlock("else_1", ifs[1].Else!, [returns[2]]);
        var end1Block = new SemanticBlock("endif_1", ifs[0].Then);

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
            null,
            AccessModifierMetadata.Public,
            "test",
            [new ParameterMetadata(null, "a", TypeMetadata.I32)],
            new FunctionTypeMetadata(null, [TypeMetadata.I32], TypeMetadata.I32),
            new FunctionGroupMetadata()
        );

        Assert.That(graphs.Functions, Has.Count.EqualTo(1));
        Assert.That(
            graphs.Functions,
            Contains.Key(function).WithValue(expected).Using(ControlFlowGraphComparer.Instance));
    }
}
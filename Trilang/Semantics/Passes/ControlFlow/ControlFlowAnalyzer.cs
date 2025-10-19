using System.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes.ControlFlow;

internal class ControlFlowAnalyzer : ISemanticPass
{
    private readonly Stack<(SemanticBlock Condition, SemanticBlock End)> whileLoops;

    private IEnumerable<string> directives = null!;
    private ControlFlowGraphMap? graph;
    private int ifCounter;
    private int loopCounter;

    public ControlFlowAnalyzer()
    {
        whileLoops = [];
        ifCounter = 0;
        loopCounter = 0;
    }

    public void Analyze(SemanticTree tree, SemanticPassContext context)
    {
        directives = context.Directives;
        graph = new ControlFlowGraphMap();

        foreach (var function in GetFunctions(tree))
            BuildControlFlowGraph(function.Metadata!, function.Body);

        foreach (var method in GetMethods(tree))
            BuildControlFlowGraph(method.Metadata!, method.Body);

        foreach (var constructor in GetConstructors(tree))
            BuildControlFlowGraph(constructor.Metadata!, constructor.Body);

        context.ControlFlowGraphs = graph;
    }

    private IEnumerable<FunctionDeclaration> GetFunctions(SemanticTree tree)
        => tree.Declarations.OfType<FunctionDeclaration>();

    private IEnumerable<MethodDeclaration> GetMethods(SemanticTree tree)
        => tree.Declarations.OfType<TypeDeclaration>().SelectMany(x => x.Methods);

    private IEnumerable<ConstructorDeclaration> GetConstructors(SemanticTree tree)
        => tree.Declarations.OfType<TypeDeclaration>().SelectMany(x => x.Constructors);

    private void BuildControlFlowGraph(IFunctionMetadata metadata, BlockStatement body)
    {
        var builder = new ControlFlowGraphBuilder(body);

        BuildControlFlowGraph(builder, body);

        graph!.Add(metadata, builder.Build());
    }

    private void BuildControlFlowGraph(ControlFlowGraphBuilder builder, BlockStatement block)
    {
        foreach (var statement in block.Statements)
            BuildControlFlowGraph(builder, block, statement);
    }

    private void BuildControlFlowGraph(ControlFlowGraphBuilder builder, BlockStatement block, IStatement statement)
    {
        if (statement is BlockStatement blockStatement)
        {
            BuildControlFlowGraph(builder, blockStatement);
        }
        else if (statement is IfStatement ifNode)
        {
            builder.Add(ifNode);

            var ifNumber = ifCounter++;
            var currentBlock = builder.CurrentBlock;
            var thenBlock = new SemanticBlock($"then_{ifNumber}", ifNode.Then);
            var endBlock = new SemanticBlock($"endif_{ifNumber}", block);

            currentBlock.AddNext(thenBlock);

            builder.CurrentBlock = thenBlock;
            BuildControlFlowGraph(builder, ifNode.Then);
            builder.CurrentBlock.AddNext(endBlock);

            if (ifNode.Else is not null)
            {
                var elseBlock = new SemanticBlock($"else_{ifNumber}", ifNode.Else);
                currentBlock.AddNext(elseBlock);

                builder.CurrentBlock = elseBlock;
                BuildControlFlowGraph(builder, ifNode.Else);
                builder.CurrentBlock.AddNext(endBlock);
            }
            else
            {
                currentBlock.AddNext(endBlock);
            }

            builder.CurrentBlock = endBlock;
        }
        else if (statement is While whileNode)
        {
            var whileNumber = loopCounter++;
            var currentBlock = builder.CurrentBlock;
            var conditionBlock = new SemanticBlock($"loopcond_{whileNumber}", block);
            var bodyBlock = new SemanticBlock($"loopbody_{whileNumber}", whileNode.Body);
            var endBlock = new SemanticBlock($"loopend_{whileNumber}", block);
            whileLoops.Push((conditionBlock, endBlock));

            currentBlock.AddNext(conditionBlock);
            conditionBlock.AddNext(bodyBlock);
            conditionBlock.AddNext(endBlock);

            builder.CurrentBlock = conditionBlock;
            builder.Add(whileNode);

            builder.CurrentBlock = bodyBlock;
            BuildControlFlowGraph(builder, whileNode.Body);
            builder.CurrentBlock.AddNext(conditionBlock);

            builder.CurrentBlock = endBlock;
            whileLoops.Pop();
        }
        else if (statement is Break { LoopNode: not null } breakNode)
        {
            Debug.Assert(whileLoops.Count > 0, "While loop is not found.");

            builder.Add(breakNode);

            var (_, endBlock) = whileLoops.Peek();
            builder.CurrentBlock.AddNext(endBlock);
        }
        else if (statement is Continue { LoopNode: not null } continueNode)
        {
            Debug.Assert(whileLoops.Count > 0, "While loop is not found.");

            builder.Add(continueNode);

            var (conditionBlock, _) = whileLoops.Peek();
            builder.CurrentBlock.AddNext(conditionBlock);
        }
        else if (statement is IfDirective ifDirectiveNode)
        {
            builder.Add(ifDirectiveNode);

            var statements = directives.Contains(ifDirectiveNode.DirectiveName)
                ? ifDirectiveNode.Then.OfType<IStatement>()
                : ifDirectiveNode.Else.OfType<IStatement>();

            foreach (var node in statements)
                BuildControlFlowGraph(builder, block, node);
        }
        else
        {
            builder.Add(statement);
        }
    }

    public string Name => nameof(ControlFlowAnalyzer);

    public IEnumerable<string> DependsOn => [nameof(TypeChecker)];
}
using System.Diagnostics;
using Trilang.Metadata;
using Trilang.Parsing.Ast;

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

    public void Analyze(SyntaxTree tree, SemanticPassContext context)
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

    private IEnumerable<FunctionDeclarationNode> GetFunctions(SyntaxTree tree)
        => tree.Declarations.OfType<FunctionDeclarationNode>();

    private IEnumerable<MethodDeclarationNode> GetMethods(SyntaxTree tree)
        => tree.Declarations.OfType<TypeDeclarationNode>().SelectMany(x => x.Methods);

    private IEnumerable<ConstructorDeclarationNode> GetConstructors(SyntaxTree tree)
        => tree.Declarations.OfType<TypeDeclarationNode>().SelectMany(x => x.Constructors);

    private void BuildControlFlowGraph(IFunctionMetadata metadata, BlockStatementNode body)
    {
        var builder = new ControlFlowGraphBuilder();

        BuildControlFlowGraphForBlock(builder, body.Statements);

        graph!.Add(metadata, builder.Build());
    }

    private void BuildControlFlowGraphForBlock(ControlFlowGraphBuilder builder, IEnumerable<IStatementNode> statements)
    {
        foreach (var statement in statements)
        {
            if (statement is BlockStatementNode blockStatement)
            {
                BuildControlFlowGraphForBlock(builder, blockStatement.Statements);
            }
            else if (statement is IfStatementNode ifNode)
            {
                builder.Add(ifNode);

                var ifNumber = ifCounter++;
                var currentBlock = builder.CurrentBlock;
                var thenBlock = new SemanticBlock($"then_{ifNumber}");
                var endBlock = new SemanticBlock($"endif_{ifNumber}");

                currentBlock.AddNext(thenBlock);

                builder.CurrentBlock = thenBlock;
                BuildControlFlowGraphForBlock(builder, ifNode.Then.Statements);
                builder.CurrentBlock.AddNext(endBlock);

                if (ifNode.Else is not null)
                {
                    var elseBlock = new SemanticBlock($"else_{ifNumber}");
                    currentBlock.AddNext(elseBlock);

                    builder.CurrentBlock = elseBlock;
                    BuildControlFlowGraphForBlock(builder, ifNode.Else.Statements);
                    builder.CurrentBlock.AddNext(endBlock);
                }
                else
                {
                    currentBlock.AddNext(endBlock);
                }

                builder.CurrentBlock = endBlock;
            }
            else if (statement is WhileNode whileNode)
            {
                var whileNumber = loopCounter++;
                var currentBlock = builder.CurrentBlock;
                var conditionBlock = new SemanticBlock($"loopcond_{whileNumber}");
                var bodyBlock = new SemanticBlock($"loopbody_{whileNumber}");
                var endBlock = new SemanticBlock($"loopend_{whileNumber}");
                whileLoops.Push((conditionBlock, endBlock));

                currentBlock.AddNext(conditionBlock);
                conditionBlock.AddNext(bodyBlock);
                conditionBlock.AddNext(endBlock);

                builder.CurrentBlock = conditionBlock;
                builder.Add(whileNode);

                builder.CurrentBlock = bodyBlock;
                BuildControlFlowGraphForBlock(builder, whileNode.Body.Statements);
                builder.CurrentBlock.AddNext(conditionBlock);

                builder.CurrentBlock = endBlock;
                whileLoops.Pop();
            }
            else if (statement is BreakNode breakNode)
            {
                Debug.Assert(whileLoops.Count > 0, "While loop is not found.");

                builder.Add(breakNode);

                var (_, endBlock) = whileLoops.Peek();
                builder.CurrentBlock.AddNext(endBlock);
            }
            else if (statement is ContinueNode continueNode)
            {
                Debug.Assert(whileLoops.Count > 0, "While loop is not found.");

                builder.Add(continueNode);

                var (conditionBlock, _) = whileLoops.Peek();
                builder.CurrentBlock.AddNext(conditionBlock);
            }

            else if (statement is IfDirectiveNode ifDirectiveNode)
            {
                builder.Add(ifDirectiveNode);

                BuildControlFlowGraphForBlock(
                    builder,
                    directives.Contains(ifDirectiveNode.DirectiveName)
                        ? ifDirectiveNode.Then.OfType<IStatementNode>()
                        : ifDirectiveNode.Else.OfType<IStatementNode>());
            }
            else
            {
                builder.Add(statement);
            }
        }
    }

    public string Name => nameof(ControlFlowAnalyzer);

    public IEnumerable<string> DependsOn => [nameof(TypeChecker)];
}
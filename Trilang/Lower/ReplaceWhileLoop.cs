using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Lower;

internal class ReplaceWhileLoop : Visitor
{
    private readonly Dictionary<WhileNode, string> loopLabels;
    private int loopCounter;

    public ReplaceWhileLoop()
    {
        loopLabels = [];
        loopCounter = 0;
    }

    private void ResetLabelCounter()
    {
        loopLabels.Clear();
        loopCounter = 0;
    }

    private string GetLoopLabel(WhileNode node)
    {
        if (!loopLabels.TryGetValue(node, out var label))
            loopLabels[node] = label = $"loop_{loopCounter++}";

        return label;
    }

    public override void VisitWhile(WhileNode node)
    {
        var label = GetLoopLabel(node);
        var loopStart = $"{label}_start";
        var loopEnd = $"{label}_end";

        var newBlock = new BlockStatementNode([
            // this 'goto' statement is needed to handle transition from the previous block correctly
            new GoToNode(loopStart),
            new LabelNode(loopStart),
            new IfStatementNode(
                node.Condition,
                new BlockStatementNode([
                    ..node.Body.Statements,
                    new GoToNode(loopStart),
                ]),
                new BlockStatementNode([
                    new GoToNode(loopEnd),
                ])
            ),
            new LabelNode(loopEnd),
        ]);

        var parentBlock = (BlockStatementNode)node.Parent!;
        parentBlock.Replace(node, newBlock);

        newBlock.Accept(this);
    }

    protected override void VisitBreakEnter(BreakNode node)
    {
        var loop = node.LoopNode!;
        var label = GetLoopLabel(loop);

        var parentBlock = (BlockStatementNode)node.Parent!;
        parentBlock.Replace(node, new GoToNode($"{label}_end"));
    }

    protected override void VisitContinueEnter(ContinueNode node)
    {
        var loop = node.LoopNode!;
        var label = GetLoopLabel(loop);

        var parentBlock = (BlockStatementNode)node.Parent!;
        parentBlock.Replace(node, new GoToNode($"{label}_start"));
    }

    protected override void VisitConstructorExit(ConstructorDeclarationNode node)
        => ResetLabelCounter();

    protected override void VisitFunctionExit(FunctionDeclarationNode node)
        => ResetLabelCounter();

    protected override void VisitMethodExit(MethodDeclarationNode node)
        => ResetLabelCounter();

    protected override void VisitGetterExit(PropertyGetterNode node)
        => ResetLabelCounter();

    protected override void VisitSetterExit(PropertySetterNode node)
        => ResetLabelCounter();
}
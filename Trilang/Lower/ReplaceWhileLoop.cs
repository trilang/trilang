using Trilang.Semantics;
using Trilang.Semantics.Model;

namespace Trilang.Lower;

internal class ReplaceWhileLoop : Visitor
{
    private readonly Dictionary<While, string> loopLabels;
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

    private string GetLoopLabel(While node)
    {
        if (!loopLabels.TryGetValue(node, out var label))
            loopLabels[node] = label = $"loop_{loopCounter++}";

        return label;
    }

    public override void VisitWhile(While node)
    {
        var label = GetLoopLabel(node);
        var loopStart = $"{label}_start";
        var loopEnd = $"{label}_end";

        var newBlock = new BlockStatement([
            // this 'goto' statement is needed to handle transition from the previous block correctly
            new GoTo(loopStart),
            new Label(loopStart),
            new IfStatement(
                node.Condition,
                new BlockStatement([
                    ..node.Body.Statements,
                    new GoTo(loopStart),
                ]),
                new BlockStatement([
                    new GoTo(loopEnd),
                ])
            ),
            new Label(loopEnd),
        ]);

        var parentBlock = (BlockStatement)node.Parent!;
        parentBlock.Replace(node, newBlock);

        newBlock.Accept(this);
    }

    protected override void VisitBreakEnter(Break node)
    {
        var loop = node.LoopNode!;
        var label = GetLoopLabel(loop);

        var parentBlock = (BlockStatement)node.Parent!;
        parentBlock.Replace(node, new GoTo($"{label}_end"));
    }

    protected override void VisitContinueEnter(Continue node)
    {
        var loop = node.LoopNode!;
        var label = GetLoopLabel(loop);

        var parentBlock = (BlockStatement)node.Parent!;
        parentBlock.Replace(node, new GoTo($"{label}_start"));
    }

    protected override void VisitConstructorExit(ConstructorDeclaration node)
        => ResetLabelCounter();

    protected override void VisitFunctionExit(FunctionDeclaration node)
        => ResetLabelCounter();

    protected override void VisitMethodExit(MethodDeclaration node)
        => ResetLabelCounter();

    protected override void VisitGetterExit(PropertyGetter node)
        => ResetLabelCounter();

    protected override void VisitSetterExit(PropertySetter node)
        => ResetLabelCounter();
}
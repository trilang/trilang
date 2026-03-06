using Trilang.Semantics;
using Trilang.Semantics.Model;

namespace Trilang.Lower;

internal class ReplaceWhileLoop : Visitor
{
    private readonly Dictionary<While, string> loopLabels;
    private int loopCounter;

    public ReplaceWhileLoop(ISet<string> directives) : base(directives)
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

        var newBlock = new BlockStatement(null, [
            // this 'goto' statement is needed to handle transition from the previous block correctly
            new GoTo(loopStart),
            new Label(loopStart),
            new IfStatement(
                null,
                node.Condition,
                new BlockStatement(null, [
                    ..node.Body.Statements,
                    new GoTo(loopStart),
                ]),
                new BlockStatement(null, [
                    new GoTo(loopEnd),
                ])
            ),
            new Label(loopEnd),
        ]);

        var parentBlock = (BlockStatement)node.Parent!;
        parentBlock.Replace(node, newBlock);

        newBlock.Accept(this);
    }

    public override void VisitBreak(Break node)
    {
        var loop = node.LoopNode!;
        var label = GetLoopLabel(loop);

        var parentBlock = (BlockStatement)node.Parent!;
        parentBlock.Replace(node, new GoTo($"{label}_end"));

        base.VisitBreak(node);
    }

    public override void VisitContinue(Continue node)
    {
        var loop = node.LoopNode!;
        var label = GetLoopLabel(loop);

        var parentBlock = (BlockStatement)node.Parent!;
        parentBlock.Replace(node, new GoTo($"{label}_start"));

        base.VisitContinue(node);
    }

    public override void VisitConstructor(ConstructorDeclaration node)
    {
        base.VisitConstructor(node);

        ResetLabelCounter();
    }

    public override void VisitFunction(FunctionDeclaration node)
    {
        base.VisitFunction(node);

        ResetLabelCounter();
    }

    public override void VisitMethod(MethodDeclaration node)
    {
        base.VisitMethod(node);

        ResetLabelCounter();
    }

    public override void VisitGetter(PropertyGetter node)
    {
        base.VisitGetter(node);

        ResetLabelCounter();
    }

    public override void VisitSetter(PropertySetter node)
    {
        base.VisitSetter(node);

        ResetLabelCounter();
    }
}
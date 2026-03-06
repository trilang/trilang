using Trilang.Semantics;
using Trilang.Semantics.Model;

namespace Trilang.Lower;

internal class RewriteIfStatement : Visitor
{
    private readonly Dictionary<IfStatement, string> ifLabels;
    private int ifCounter;

    public RewriteIfStatement(ISet<string> directives) : base(directives)
    {
        ifLabels = [];
        ifCounter = 0;
    }

    private void ResetLabelCounter()
    {
        ifLabels.Clear();
        ifCounter = 0;
    }

    private string GetLoopLabel(IfStatement node)
    {
        if (!ifLabels.TryGetValue(node, out var label))
            ifLabels[node] = label = $"if_{ifCounter++}";

        return label;
    }

    public override void VisitIf(IfStatement node)
    {
        var parentBlock = (IBlock)node.Parent!;

        var label = GetLoopLabel(node);
        var thenBlockName = $"{label}_then";
        var elseBlockName = $"{label}_else";
        var endBlockName = $"{label}_end";

        var thenBlock = node.Then;
        var elseBlock = node.Else;

        var generateThenBlock = false;
        var generateElseBlock = false;
        var generateEndBlock = false;

        if (thenBlock.Statements is not [GoTo])
        {
            generateThenBlock = true;
            thenBlock.Insert(0, new Label(thenBlockName));

            if (thenBlock.Statements[^1] is not GoTo)
            {
                generateEndBlock = true;
                thenBlock.Add(new GoTo(endBlockName));
            }
        }

        if (elseBlock is not null)
        {
            if (elseBlock.Statements is not [GoTo])
            {
                generateElseBlock = true;
                elseBlock.Insert(0, new Label(elseBlockName));

                if (elseBlock.Statements[^1] is not GoTo)
                {
                    generateEndBlock = true;
                    elseBlock.Add(new GoTo(endBlockName));
                }
            }
        }
        else
        {
            elseBlock = new BlockStatement(null, [
                new GoTo(endBlockName)
            ]);
            generateEndBlock = true;
        }

        var newIf = new IfStatement(
            null,
            node.Condition,
            generateThenBlock ? new BlockStatement(null, [new GoTo(thenBlockName)]) : thenBlock,
            generateElseBlock ? new BlockStatement(null, [new GoTo(elseBlockName)]) : elseBlock
        );
        parentBlock.Replace(node, newIf);

        if (generateEndBlock)
            parentBlock.InsertAfter(newIf, new Label(endBlockName));

        if (generateElseBlock)
            parentBlock.InsertAfter(newIf, elseBlock);

        if (generateThenBlock)
            parentBlock.InsertAfter(newIf, thenBlock);

        base.VisitIf(node);
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
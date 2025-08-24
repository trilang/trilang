using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Lower;

internal class RewriteIfStatement : Visitor
{
    private readonly Dictionary<IfStatementNode, string> ifLabels;
    private int ifCounter;

    public RewriteIfStatement()
    {
        ifLabels = [];
        ifCounter = 0;
    }

    private void ResetLabelCounter()
    {
        ifLabels.Clear();
        ifCounter = 0;
    }

    private string GetLoopLabel(IfStatementNode node)
    {
        if (!ifLabels.TryGetValue(node, out var label))
            ifLabels[node] = label = $"if_{ifCounter++}";

        return label;
    }

    protected override void VisitIfEnter(IfStatementNode node)
    {
        var parentBlock = (IBlockNode)node.Parent!;

        var label = GetLoopLabel(node);
        var thenBlockName = $"{label}_then";
        var elseBlockName = $"{label}_else";
        var endBlockName = $"{label}_end";

        var thenBlock = node.Then;
        var elseBlock = node.Else;

        var generateThenBlock = false;
        var generateElseBlock = false;
        var generateEndBlock = false;

        if (thenBlock.Statements is not [GoToNode])
        {
            generateThenBlock = true;
            thenBlock.Insert(0, new LabelNode(thenBlockName));

            if (thenBlock.Statements[^1] is not GoToNode)
            {
                generateEndBlock = true;
                thenBlock.Add(new GoToNode(endBlockName));
            }
        }

        if (elseBlock is not null)
        {
            if (elseBlock.Statements is not [GoToNode])
            {
                generateElseBlock = true;
                elseBlock.Insert(0, new LabelNode(elseBlockName));

                if (elseBlock.Statements[^1] is not GoToNode)
                {
                    generateEndBlock = true;
                    elseBlock.Add(new GoToNode(endBlockName));
                }
            }
        }
        else
        {
            elseBlock = new BlockStatementNode([
                new GoToNode(endBlockName)
            ]);
            generateEndBlock = true;
        }

        var newIf = new IfStatementNode(
            node.Condition,
            generateThenBlock ? new BlockStatementNode([new GoToNode(thenBlockName)]) : thenBlock,
            generateElseBlock ? new BlockStatementNode([new GoToNode(elseBlockName)]) : elseBlock
        );
        parentBlock.Replace(node, newIf);

        if (generateEndBlock)
            parentBlock.InsertAfter(newIf, new LabelNode(endBlockName));

        if (generateElseBlock)
            parentBlock.InsertAfter(newIf, elseBlock);

        if (generateThenBlock)
            parentBlock.InsertAfter(newIf, thenBlock);
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
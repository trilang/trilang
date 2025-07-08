using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics;

internal class BreakContinueWithinLoop : Visitor
{
    protected override void VisitBreakEnter(BreakNode node)
    {
        var whileLoop = node.FindInParent<WhileNode>() ??
                        throw new SemanticAnalysisException("The 'break' keyword can only be used within a loop.");

        node.LoopNode = whileLoop;
    }

    protected override void VisitContinueEnter(ContinueNode node)
    {
        var whileLoop = node.FindInParent<WhileNode>() ??
                        throw new SemanticAnalysisException("The 'continue' keyword can only be used within a loop.");

        node.LoopNode = whileLoop;
    }
}
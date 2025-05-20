using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics;

internal class BreakContinueWithinLoop : Visitor
{
    protected override void VisitEnter(BreakNode node)
    {
        var whileLoop = node.FindInParent<WhileNode>();
        if (whileLoop is null)
            throw new SemanticAnalysisException("The 'break' keyword can only be used within a loop.");
    }

    protected override void VisitEnter(ContinueNode node)
    {
        var whileLoop = node.FindInParent<WhileNode>();
        if (whileLoop is null)
            throw new SemanticAnalysisException("The 'continue' keyword can only be used within a loop.");
    }
}
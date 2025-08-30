using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.Semantics.Passes;

internal class BreakContinueWithinLoop : Visitor, ISemanticPass
{
    public void Analyze(SyntaxTree tree, SemanticPassContext context)
        => tree.Accept(this);

    protected override void VisitBreakEnter(BreakNode node)
        => node.LoopNode = node.FindInParent<WhileNode>() ??
                           throw new SemanticAnalysisException("The 'break' keyword can only be used within a loop.");

    protected override void VisitContinueEnter(ContinueNode node)
        => node.LoopNode = node.FindInParent<WhileNode>() ??
                           throw new SemanticAnalysisException("The 'continue' keyword can only be used within a loop.");

    public string Name => nameof(BreakContinueWithinLoop);

    public IEnumerable<string> DependsOn => [];
}
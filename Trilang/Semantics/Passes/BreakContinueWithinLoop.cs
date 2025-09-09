using Trilang.Semantics.Model;

namespace Trilang.Semantics.Passes;

internal class BreakContinueWithinLoop : Visitor, ISemanticPass
{
    public void Analyze(SemanticTree tree, SemanticPassContext context)
        => tree.Accept(this);

    protected override void VisitBreakEnter(Break node)
        => node.LoopNode = node.FindInParent<While>() ??
                           throw new SemanticAnalysisException("The 'break' keyword can only be used within a loop.");

    protected override void VisitContinueEnter(Continue node)
        => node.LoopNode = node.FindInParent<While>() ??
                           throw new SemanticAnalysisException("The 'continue' keyword can only be used within a loop.");

    public string Name => nameof(BreakContinueWithinLoop);

    public IEnumerable<string> DependsOn => [];
}
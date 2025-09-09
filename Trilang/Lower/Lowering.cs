using Trilang.Semantics.Model;

namespace Trilang.Lower;

public class Lowering
{
    public void Lower(SemanticTree tree, LoweringOptions options)
    {
        // TODO: immutable tree?
        tree.Accept(new ReplaceIfDirectives(options.Directives));
        new AddImplicitReturnStatements().InsertReturnStatements(options.ControlFlowGraphs);
        tree.Accept(new AddThisInLocalMemberAccess());
        tree.Transform(new ReplaceCompoundAssignments());
        tree.Transform(new ReplacePropertyFieldAndValueWithGeneratedField());
        tree.Transform(new ReplaceGettersAndSettersWithMethodCalls());
        tree.Transform(new ReplaceConditionalOperators());
        tree.Accept(new ReplaceWhileLoop());
        tree.Accept(new RewriteIfStatement());
    }
}
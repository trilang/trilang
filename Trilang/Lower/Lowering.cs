using Trilang.Metadata;
using Trilang.Semantics;
using Trilang.Semantics.Model;

namespace Trilang.Lower;

public class Lowering
{
    private readonly BuiltInTypes builtInTypes;

    public Lowering(BuiltInTypes builtInTypes)
        => this.builtInTypes = builtInTypes;

    public void Lower(SemanticTree tree, LoweringOptions options)
    {
        // TODO: immutable tree?
        tree.Accept(new ReplaceIfDirectives(options.Directives));
        new AddImplicitReturnStatements(builtInTypes).InsertReturnStatements(options.ControlFlowGraphs);
        tree.Accept(new AddThisInLocalMemberAccess(options.Directives));
        tree.Transform(new ReplaceCompoundAssignments(builtInTypes));
        tree.Transform(new ReplacePropertyFieldAndValueWithGeneratedField(builtInTypes));
        tree.Transform(new ReplaceGettersAndSettersWithMethodCalls(builtInTypes));
        tree.Transform(new ReplaceConditionalOperators(builtInTypes));
        tree.Accept(new ReplaceWhileLoop(options.Directives));
        tree.Accept(new RewriteIfStatement(options.Directives));
    }
}
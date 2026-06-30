using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics;
using Trilang.Semantics.Model;

namespace Trilang.Lower;

public class Lowering
{
    private readonly DiagnosticCollection diagnostics;
    private readonly BuiltInTypes builtInTypes;

    public Lowering(DiagnosticCollection diagnostics, BuiltInTypes builtInTypes)
    {
        this.diagnostics = diagnostics;
        this.builtInTypes = builtInTypes;
    }

    public SemanticTree Lower(SemanticTree tree, LoweringOptions options)
    {
        // TODO: immutable tree?
        tree.Accept(new ReplaceIfDirectives(options.Directives));
        new AddImplicitReturnStatements(builtInTypes).InsertReturnStatements(options.ControlFlowGraphs);
        tree.Accept(new AddThisInLocalMemberAccess(options.Directives, diagnostics, builtInTypes));
        tree = (SemanticTree)tree.Transform(new ReplaceCompoundAssignments(options.Directives, builtInTypes));
        tree = (SemanticTree)tree.Transform(new ReplacePropertyFieldAndValueWithGeneratedField(options.Directives, diagnostics, builtInTypes));
        tree = (SemanticTree)tree.Transform(new ReplaceGettersAndSettersWithMethodCalls(options.Directives, builtInTypes));
        tree = (SemanticTree)tree.Transform(new ReplaceConditionalOperators(options.Directives, builtInTypes));
        tree.Accept(new ReplaceWhileLoop(options.Directives));
        tree.Accept(new RewriteIfStatement(options.Directives));

        return tree;
    }
}
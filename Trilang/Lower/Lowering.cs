using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using Trilang.Semantics.Passes;

namespace Trilang.Lower;

public class Lowering
{
    private readonly DiagnosticCollection diagnostics;
    private readonly BuiltInTypes builtInTypes;
    private readonly MetadataProviderMap metadataProviderMap;

    public Lowering(
        DiagnosticCollection diagnostics,
        BuiltInTypes builtInTypes,
        MetadataProviderMap metadataProviderMap)
    {
        this.diagnostics = diagnostics;
        this.builtInTypes = builtInTypes;
        this.metadataProviderMap = metadataProviderMap;
    }

    public void Lower(SemanticTree tree, LoweringOptions options)
    {
        // TODO: immutable tree?
        tree.Accept(new ReplaceIfDirectives(options.Directives));
        new AddImplicitReturnStatements(builtInTypes).InsertReturnStatements(options.ControlFlowGraphs);
        tree.Accept(new AddThisInLocalMemberAccess(options.Directives, diagnostics, builtInTypes, metadataProviderMap));
        tree.Transform(new ReplaceCompoundAssignments(builtInTypes));
        tree.Transform(new ReplacePropertyFieldAndValueWithGeneratedField(diagnostics, builtInTypes, metadataProviderMap));
        tree.Transform(new ReplaceGettersAndSettersWithMethodCalls(builtInTypes));
        tree.Transform(new ReplaceConditionalOperators(builtInTypes));
        tree.Accept(new ReplaceWhileLoop(options.Directives));
        tree.Accept(new RewriteIfStatement(options.Directives));
    }
}
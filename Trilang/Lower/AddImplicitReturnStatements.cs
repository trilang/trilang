using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Semantics.Passes.ControlFlow;

namespace Trilang.Lower;

internal class AddImplicitReturnStatements
{
    private readonly BuiltInTypes builtInTypes;

    public AddImplicitReturnStatements(BuiltInTypes builtInTypes)
        => this.builtInTypes = builtInTypes;

    public void InsertReturnStatements(ControlFlowGraphMap cfgs)
    {
        foreach (var (function, cfg) in cfgs.Functions)
        {
            if (function is not ConstructorMetadata &&
                !function.Type.ReturnType.Equals(builtInTypes.Void))
                continue;

            var visited = new HashSet<SemanticBlock>();
            var q = new Queue<SemanticBlock>();
            q.Enqueue(cfg.Entry);

            while (q.TryDequeue(out var block))
            {
                if (!visited.Add(block))
                    continue;

                if (block.Statements.Any(x => x is ReturnStatement))
                    continue;

                if (block.Next.Count == 0)
                {
                    var returnStatement = new ReturnStatement(null)
                    {
                        SymbolTable = block.BlockNode.SymbolTable,
                        MetadataProvider = block.BlockNode.MetadataProvider,
                    };
                    block.BlockNode.Add(returnStatement);
                }

                foreach (var next in block.Next)
                    q.Enqueue(next);
            }
        }
    }
}
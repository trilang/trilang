using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Semantics.Passes.ControlFlow;

namespace Trilang.Lower;

internal class AddImplicitReturnStatements
{
    public void InsertReturnStatements(ControlFlowGraphMap cfgs)
    {
        foreach (var (function, cfg) in cfgs.Functions)
        {
            if (function is not ConstructorMetadata && !function.Type.ReturnType.Equals(TypeMetadata.Void))
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
                    block.BlockNode.Add(new ReturnStatement(null));

                foreach (var next in block.Next)
                    q.Enqueue(next);
            }
        }
    }
}
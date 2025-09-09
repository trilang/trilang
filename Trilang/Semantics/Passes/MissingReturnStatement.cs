using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Semantics.Passes.ControlFlow;

namespace Trilang.Semantics.Passes;

// TODO: handle endless loops
internal class MissingReturnStatement : ISemanticPass
{
    public void Analyze(SemanticTree tree, SemanticPassContext context)
    {
        var cfgs = context.ControlFlowGraphs;
        if (cfgs is null)
            return;

        foreach (var (function, cfg) in cfgs.Functions)
        {
            if (function is ConstructorMetadata || function.Type.ReturnType.Equals(TypeMetadata.Void))
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
                    throw new SemanticAnalysisException($"{function}. Not all paths return a value.");

                foreach (var next in block.Next)
                    q.Enqueue(next);
            }
        }
    }

    public string Name => nameof(MissingReturnStatement);

    public IEnumerable<string> DependsOn => [nameof(ControlFlowAnalyzer)];
}
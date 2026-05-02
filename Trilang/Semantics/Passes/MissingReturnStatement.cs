using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using Trilang.Semantics.Passes.ControlFlow;

namespace Trilang.Semantics.Passes;

// TODO: handle endless loops
internal class MissingReturnStatement : ISemanticPass
{
    private readonly SemanticDiagnosticReporter diagnostics;
    private readonly ControlFlowGraphMap map;

    public MissingReturnStatement(DiagnosticCollection diagnostics, ControlFlowGraphMap map)
    {
        this.diagnostics = diagnostics.ForSemantic();
        this.map = map;
    }

    public void Analyze(IEnumerable<SemanticTree> _)
    {
        var visited = new HashSet<SemanticBlock>();
        var q = new Queue<SemanticBlock>();

        foreach (var (function, cfg) in map.Functions)
        {
            if (function is ConstructorMetadata || function.Type.ReturnType.Equals(builtInTypes.Void))
                continue;

            visited.Clear();
            q.Clear();
            q.Enqueue(cfg.Entry);

            while (q.TryDequeue(out var block))
            {
                if (!visited.Add(block))
                    continue;

                if (block.Statements.Any(x => x is ReturnStatement))
                    continue;

                if (block.Next.Count == 0)
                {
                    diagnostics.NotAllPathsReturnValue(function);
                    continue;
                }

                foreach (var next in block.Next)
                    q.Enqueue(next);
            }
        }
    }

    public string Name => nameof(MissingReturnStatement);

    public IEnumerable<string> DependsOn => [nameof(ControlFlowAnalyzer)];
}
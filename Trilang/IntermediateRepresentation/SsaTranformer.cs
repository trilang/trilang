using Trilang.IntermediateRepresentation.Instructions;

namespace Trilang.IntermediateRepresentation;

internal class SsaTransformer
{
    public void Transform(IrCode code)
    {
        var blocks = GetAllBlocks(code.Entry);
        var dominators = ComputeDominators(code.Entry, blocks);
        var dominanceFrontiers = ComputeDominanceFrontiers(blocks, dominators);
        GeneratePhiFunctions(blocks, dominanceFrontiers);

        var renamer = new SsaRenamer(code.RegisterCounter, blocks, dominators);
        renamer.Rename();
    }

    private IReadOnlyList<Block> GetAllBlocks(Block entry)
    {
        var postorder = new List<Block>();
        var visited = new HashSet<Block>();

        GetAllBlocks(entry, postorder, visited);

        for (var i = 0; i < postorder.Count; i++)
            postorder[i].Order = i;

        postorder.Reverse();

        return postorder;
    }

    private void GetAllBlocks(Block block, List<Block> postorder, HashSet<Block> visited)
    {
        if (!visited.Add(block))
            return;

        foreach (var next in block.Next)
            GetAllBlocks(next, postorder, visited);

        postorder.Add(block);
    }

    private static Dictionary<Block, Block> ComputeDominators(Block entry, IReadOnlyList<Block> blocks)
    {
        var doms = new Dictionary<Block, Block?>();

        foreach (var b in blocks)
            doms[b] = null;

        doms[entry] = entry;

        var changed = true;
        while (changed)
        {
            changed = false;

            foreach (var b in blocks)
            {
                if (b == entry)
                    continue;

                var predecessors = b.Previous
                    .Where(p => doms[p] != null)
                    .ToArray();

                if (predecessors.Length == 0)
                    continue;

                var newIdom = predecessors[0];
                for (var i = 1; i < predecessors.Length; i++)
                    newIdom = Intersect(predecessors[i], newIdom, doms!);

                if (doms[b] != newIdom)
                {
                    doms[b] = newIdom;
                    changed = true;
                }
            }
        }

        return doms!;
    }

    private static Block Intersect(Block b1, Block b2, Dictionary<Block, Block> doms)
    {
        var finger1 = b1;
        var finger2 = b2;

        while (finger1.Order != finger2.Order)
        {
            while (finger1.Order < finger2.Order)
                finger1 = doms[finger1];

            while (finger2.Order < finger1.Order)
                finger2 = doms[finger2];
        }

        return finger1;
    }

    private static Dictionary<Block, HashSet<Block>> ComputeDominanceFrontiers(
        IReadOnlyList<Block> blocks,
        Dictionary<Block, Block> idoms)
    {
        var df = new Dictionary<Block, HashSet<Block>>();
        foreach (var b in blocks)
            df[b] = [];

        foreach (var b in blocks)
        {
            if (b.Previous.Count < 2)
                continue;

            foreach (var p in b.Previous)
            {
                var runner = p;
                while (runner != idoms[b])
                {
                    df[runner].Add(b);
                    runner = idoms[runner];
                }
            }
        }

        return df;
    }

    private void GeneratePhiFunctions(IReadOnlyList<Block> blocks, Dictionary<Block, HashSet<Block>> frontiers)
    {
        // TODO: handle nested definitions of variables with the same name
        var assignments = GetAssignmentMap(blocks);
        foreach (var (variable, worklist) in assignments)
        {
            var hasAlready = new HashSet<Block>();

            while (worklist.TryDequeue(out var block))
            {
                var df = frontiers[block];
                foreach (var d in df)
                {
                    if (!hasAlready.Add(d))
                        continue;

                    var register = block.GetAssignment(variable) ??
                                   throw new Exception($"Variable {variable} not defined in block {d}");

                    d.InsertInstruction(0, new PhiInstruction(register, []));

                    if (d.GetAssignment(variable) is not null)
                        continue;

                    d.AddAssignment(variable, register, false);
                    worklist.Enqueue(d);
                }
            }
        }
    }

    private Dictionary<string, Queue<Block>> GetAssignmentMap(IReadOnlyList<Block> blocks)
    {
        var definitions = new Dictionary<string, Queue<Block>>();

        foreach (var block in blocks)
        {
            foreach (var (variable, _) in block.Assignments)
            {
                if (!definitions.TryGetValue(variable, out var worklist))
                    definitions[variable] = worklist = [];

                worklist.Enqueue(block);
            }
        }

        return definitions;
    }
}
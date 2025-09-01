using Trilang.Semantics.Passes.ControlFlow;

namespace Tri.Tests;

internal class ControlFlowGraphComparer : IEqualityComparer<ControlFlowGraph>
{
    public static readonly ControlFlowGraphComparer Instance = new ControlFlowGraphComparer();

    public bool Equals(ControlFlowGraph? x, ControlFlowGraph? y)
    {
        if (ReferenceEquals(x, y))
            return true;

        if (x is null)
            return false;

        if (y is null)
            return false;

        if (x.GetType() != y.GetType())
            return false;

        return Equals(x.Entry, y.Entry) &&
               Equals(x.End, y.End);
    }

    private bool Equals(SemanticBlock x, SemanticBlock y)
    {
        var xBlocks = GetAllBlocks(x);
        var yBlocks = GetAllBlocks(y);

        if (xBlocks.Count != yBlocks.Count)
            throw new Exception("Block count doesn't match.");

        foreach (var (xBlock, yBlock) in xBlocks.Zip(yBlocks))
        {
            if (xBlock.Name != yBlock.Name)
                throw new Exception($"Block name doesn't match: '{xBlock.Name} != {yBlock.Name}'.");

            if (xBlock.BlockNode != yBlock.BlockNode)
                throw new Exception("Block node doesn't match.");

            foreach (var (xStatement, yStatement) in xBlock.Statements.Zip(yBlock.Statements))
                if (!SyntaxComparer.Instance.Equals(xStatement, yStatement))
                    throw new Exception("Statement doesn't match.");
        }

        return true;
    }

    private IReadOnlyList<SemanticBlock> GetAllBlocks(SemanticBlock entry)
    {
        var blocks = new List<SemanticBlock>();
        var visited = new HashSet<SemanticBlock>();

        GetAllBlocks(entry, blocks, visited);

        return blocks;
    }

    private void GetAllBlocks(SemanticBlock block, List<SemanticBlock> blocks, HashSet<SemanticBlock> visited)
    {
        if (!visited.Add(block))
            return;

        blocks.Add(block);

        foreach (var next in block.Next)
            GetAllBlocks(next, blocks, visited);
    }

    public int GetHashCode(ControlFlowGraph obj)
        => HashCode.Combine(obj.Entry, obj.End);
}
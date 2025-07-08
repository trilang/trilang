using Trilang.IntermediateRepresentation;

namespace Tri.Tests.IntermediateRepresentation;

internal sealed class IrFunctionComparer : IEqualityComparer<IrFunction>
{
    public static readonly IrFunctionComparer Instance = new IrFunctionComparer();

    public bool Equals(IrFunction? x, IrFunction? y)
    {
        if (ReferenceEquals(x, y))
            return true;

        if (x is null)
            return false;

        if (y is null)
            return false;

        if (x.GetType() != y.GetType())
            return false;

        return x.Name == y.Name &&
               Equals(x.Code.Entry, y.Code.Entry);
    }

    private bool Equals(Block x, Block y)
    {
        var xBlocks = GetAllBlocks(x);
        var yBlocks = GetAllBlocks(y);

        return xBlocks.SequenceEqual(yBlocks);
    }

    private IReadOnlyList<Block> GetAllBlocks(Block entry)
    {
        var blocks = new List<Block>();
        var visited = new HashSet<Block>();

        GetAllBlocks(entry, blocks, visited);

        return blocks;
    }

    private void GetAllBlocks(Block block, List<Block> blocks, HashSet<Block> visited)
    {
        if (!visited.Add(block))
            return;

        blocks.Add(block);

        foreach (var next in block.Next)
            GetAllBlocks(next, blocks, visited);
    }

    public int GetHashCode(IrFunction obj)
        => HashCode.Combine(obj.Name, obj.Code.Entry);
}
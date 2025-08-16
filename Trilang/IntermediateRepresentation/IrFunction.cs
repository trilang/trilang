using System.Text;
using Trilang.Metadata;

namespace Trilang.IntermediateRepresentation;

public record IrFunction(string Name, IrCode Code)
{
    public IrFunction(string name, Block block) : this(name, new IrCode(block))
    {
    }

    public override string ToString()
    {
        var sb = new StringBuilder();
        sb.AppendLine($"function {Name}:");

        var queue = new Queue<Block>();
        var visited = new HashSet<Block>();
        queue.Enqueue(Code.Entry);

        while (queue.TryDequeue(out var block))
        {
            if (!visited.Add(block))
                continue;

            sb.Append(block);

            foreach (var next in block.Next)
                if (!visited.Contains(next))
                    queue.Enqueue(next);
        }

        return sb.ToString();
    }

    public static IrFunction FromMethod(IFunctionMetadata method, IrCode code)
    {
        var name = new StringBuilder();
        if (method.DeclaringType is not null)
            name.Append(MangleTypeName(method.DeclaringType)).Append('_');

        name.Append(method.Name);

        if (method.IsStatic)
            name.Append("_s");

        return new IrFunction(name.ToString(), code);
    }

    private static string MangleTypeName(ITypeMetadata metadata)
        => metadata switch
        {
            TupleMetadata tuple
                => $"_{string.Join("_", tuple.Types.Select(MangleTypeName))}_",

            TypeArrayMetadata array
                => $"array_{MangleTypeName(array.ItemMetadata!)}_",

            TypeMetadata type
                => type.Name,

            _ => throw new ArgumentOutOfRangeException(nameof(metadata)),
        };
}
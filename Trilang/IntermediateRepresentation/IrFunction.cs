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

    public static IrFunction FromFunction(FunctionMetadata function, IrCode code)
    {
        var mangler = new Mangler();
        var name = $"{function.Name}_{function.Parameters.Count}_{mangler.Mangle(function.Type)}";

        return new IrFunction(name, code);
    }

    public static IrFunction FromMethod(MethodMetadata method, IrCode code)
    {
        var name = new StringBuilder(GetTypePrefix(method.DeclaringType));
        name.Append('_')
            .Append(method.Name);

        if (method.IsStatic)
            name.Append("_s");

        name.Append('_')
            .Append(method.Parameters.Count);

        var mangler = new Mangler();
        name.Append('_')
            .Append(mangler.Mangle(method.Type));

        return new IrFunction(name.ToString(), code);
    }

    public static IrFunction FromConstructor(ConstructorMetadata method, IrCode code)
    {
        var name = new StringBuilder(GetTypePrefix(method.DeclaringType));
        name.Append('_')
            .Append(method.Name);

        var mangler = new Mangler();
        name.Append('_')
            .Append(mangler.Mangle(method.Type));

        return new IrFunction(name.ToString(), code);
    }

    private static string GetTypePrefix(ITypeMetadata metadata)
        => metadata switch
        {
            ArrayMetadata array
                => $"array_{GetTypePrefix(array.ItemMetadata!)}",

            TupleMetadata tuple
                => $"tuple_{string.Join("_", tuple.Types.Select(GetTypePrefix))}",

            TypeMetadata { IsGeneric: false } type
                => type.Name,

            // TODO: support other types
            _ => throw new ArgumentOutOfRangeException(nameof(metadata)),
        };
}
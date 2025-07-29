using System.Text;
using Trilang.Parsing.Ast;

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

    public static IrFunction FromFunction(FunctionDeclarationNode node, IrCode code)
        => new IrFunction(node.Name, code);

    public static IrFunction FromMethod(MethodDeclarationNode node, IrCode code)
    {
        var type = (TypeDeclarationNode)node.Parent!;

        return new IrFunction($"{type.Name}_{node.Name}", code);
    }

    public static IrFunction FromConstructor(ConstructorDeclarationNode node, IrCode code)
    {
        var type = (TypeDeclarationNode)node.Parent!;

        return new IrFunction($"{type.Name}_ctor", code);
    }
}
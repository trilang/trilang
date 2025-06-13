using Trilang.Parsing.Ast;

namespace Trilang.IntermediateRepresentation;

public class Ir
{
    public IReadOnlyCollection<IrFunction> Generate(IReadOnlyList<SyntaxTree> syntaxTrees)
    {
        var generator = new IrGenerator();
        foreach (var tree in syntaxTrees)
            tree.Accept(generator);

        return generator.Functions;
    }
}
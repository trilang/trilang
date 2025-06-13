using Trilang.Parsing.Ast;

namespace Trilang.IntermediateRepresentation;

public record IrFunction(string Name, Block Block)
{
    public static IrFunction FromFunction(FunctionDeclarationNode node, Block block)
        => new IrFunction(node.Name, block);

    public static IrFunction FromMethod(MethodDeclarationNode node, Block block)
    {
        var type = (TypeDeclarationNode)node.Parent!;

        return new IrFunction($"{type.Name}_{node.Name}", block);
    }

    public static IrFunction FromConstructor(ConstructorDeclarationNode node, Block block)
    {
        var type = (TypeDeclarationNode)node.Parent!;

        return new IrFunction($"{type.Name}_ctor", block);
    }

    public static IrFunction FromSetter(PropertySetterNode node, Block block)
    {
        var property = (PropertyDeclarationNode)node.Parent!;
        var type = (TypeDeclarationNode)property.Parent!;

        return new IrFunction($"{type.Name}_{property.Name}_Setter", block);
    }

    public static IrFunction FromGetter(PropertyGetterNode node, Block block)
    {
        var property = (PropertyDeclarationNode)node.Parent!;
        var type = (TypeDeclarationNode)property.Parent!;

        return new IrFunction($"{type.Name}_{property.Name}_Getter", block);
    }
}
namespace Trilang.IntermediateRepresentation.Instructions;

public record Branch(
    Register Condition,
    string ThenBlock,
    string ElseBlock) : IInstruction
{
    public override string ToString()
        => $"br\t{Condition}, {ThenBlock}, {ElseBlock}";
}
namespace Trilang.IntermediateRepresentation.Instructions;

public record BranchInstruction(
    Register Condition,
    string ThenBlock,
    string ElseBlock) : IInstruction
{
    public override string ToString()
        => $"br {Condition}, {ThenBlock}, {ElseBlock}";
}
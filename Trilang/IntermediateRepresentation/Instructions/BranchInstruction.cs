namespace Trilang.IntermediateRepresentation.Instructions;

public record BranchInstruction(
    Register Condition,
    string ThenBlock,
    string? ElseBlock) : IInstruction
{
    public override string ToString()
        => ElseBlock is null
            ? $"br {Condition}, {ThenBlock}"
            : $"br {Condition}, {ThenBlock}, {ElseBlock}";
}
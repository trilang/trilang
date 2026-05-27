namespace Trilang.Metadata.Aggregate;

public sealed class ArgumentMismatch<T> : IResolutionResult<T> where T : IMetadata
{
    private ArgumentMismatch(T member, IReadOnlyList<ArgumentMismatchDetail> mismatches)
    {
        Member = member;
        Mismatches = mismatches;
    }

    public static ArgumentMismatch<IFunctionMetadata> ForFunction(IReadOnlyList<ArgumentMismatchDetail> mismatches)
        => new ArgumentMismatch<IFunctionMetadata>(FunctionMetadata.Invalid, mismatches);

    public static ArgumentMismatch<IMetadata> ForCallable(IReadOnlyList<ArgumentMismatchDetail> mismatches)
        => new ArgumentMismatch<IMetadata>(FunctionTypeMetadata.Invalid(), mismatches);

    public T Member { get; }

    public IReadOnlyList<ArgumentMismatchDetail> Mismatches { get; }
}
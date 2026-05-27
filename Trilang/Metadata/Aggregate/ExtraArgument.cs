namespace Trilang.Metadata.Aggregate;

public sealed class ExtraArgument<T> : IResolutionResult<T> where T : IMetadata
{
    private ExtraArgument(T member, IReadOnlyList<int> positions)
    {
        Member = member;
        Positions = positions;
    }

    public static ExtraArgument<IFunctionMetadata> ForFunction(int expectedCount, int actualCount)
    {
        var positions = Enumerable.Range(actualCount, actualCount - expectedCount).ToArray();

        return new ExtraArgument<IFunctionMetadata>(FunctionMetadata.Invalid, positions);
    }

    public static ExtraArgument<IMetadata> ForCallable(int expectedCount, int actualCount)
    {
        var positions = Enumerable.Range(actualCount, actualCount - expectedCount).ToArray();

        return new ExtraArgument<IMetadata>(FunctionTypeMetadata.Invalid(), positions);
    }

    public static ExtraArgument<IGenericMetadata> ForGeneric(int expectedCount, int actualCount)
    {
        var positions = Enumerable.Range(actualCount, actualCount - expectedCount).ToArray();

        return new ExtraArgument<IGenericMetadata>(TypeMetadata.InvalidType, positions);
    }

    public T Member { get; }

    public IReadOnlyList<int> Positions { get; }
}
using Trilang.Metadata;

namespace Trilang.Semantics.TypeMatchers.AggregateResults;

public sealed class MissingArgument<T> : IResolutionResult<T> where T : IMetadata
{
    private MissingArgument(T member, T match, IReadOnlyList<int> positions)
    {
        Member = member;
        Match = match;
        Positions = positions;
    }

    public static MissingArgument<IFunctionMetadata> ForFunction(
        IFunctionMetadata match,
        int expectedCount,
        int actualCount)
    {
        var positions = Enumerable.Range(actualCount, expectedCount - actualCount).ToArray();

        return new MissingArgument<IFunctionMetadata>(FunctionMetadata.Invalid, match, positions);
    }

    public static MissingArgument<IMetadata> ForCallable(IMetadata match, int expectedCount, int actualCount)
    {
        var positions = Enumerable.Range(actualCount, expectedCount - actualCount).ToArray();

        return new MissingArgument<IMetadata>(FunctionTypeMetadata.Invalid(), match, positions);
    }

    public static MissingArgument<IGenericMetadata> ForGeneric(
        IGenericMetadata match,
        int expectedCount,
        int actualCount)
    {
        var positions = Enumerable.Range(actualCount, expectedCount - actualCount).ToArray();

        return new MissingArgument<IGenericMetadata>(TypeMetadata.InvalidType, match, positions);
    }

    public T Member { get; }

    public T Match { get; }

    public IReadOnlyList<int> Positions { get; }
}
using Trilang.Metadata;

namespace Trilang.Semantics.TypeMatchers.AggregateResults;

public sealed class ConversionMatch<T> : IResolutionResult<T> where T : IMetadata
{
    public ConversionMatch(T member, IReadOnlyList<ConversionMatchDetails> conversions)
    {
        Member = member;
        Conversions = conversions;
    }

    public T Member { get; }

    public IReadOnlyList<ConversionMatchDetails> Conversions { get; }
}
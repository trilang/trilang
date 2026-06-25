using Trilang.Metadata;

namespace Trilang.Semantics.TypeMatchers.AggregateResults;

public sealed class MultipleMatches<T> : IResolutionResult<T> where T : IMetadata
{
    private MultipleMatches(T member, IEnumerable<T> candidates)
    {
        Member = member;
        Candidates = candidates;
    }

    public static MultipleMatches<IMetadata> ForMember(IEnumerable<IMetadata> candidates)
        => new MultipleMatches<IMetadata>(InvalidMemberMetadata.Instance, candidates);

    public static MultipleMatches<IFunctionMetadata> ForFunction(IEnumerable<IFunctionMetadata> candidates)
        => new MultipleMatches<IFunctionMetadata>(FunctionMetadata.Invalid, candidates);

    public static MultipleMatches<IGenericMetadata> ForGeneric(IEnumerable<IGenericMetadata> candidates)
        => new MultipleMatches<IGenericMetadata>(TypeMetadata.InvalidType, candidates);

    public static MultipleMatches<INamedMetadata> ForType(IEnumerable<INamedMetadata> candidates)
        => new MultipleMatches<INamedMetadata>(TypeMetadata.InvalidType, candidates);

    public T Member { get; }

    public IEnumerable<T> Candidates { get; }
}
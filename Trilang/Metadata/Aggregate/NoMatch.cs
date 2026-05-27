namespace Trilang.Metadata.Aggregate;

public sealed class NoMatch<T> : IResolutionResult<T> where T : IMetadata
{
    private NoMatch(T member)
        => Member = member;

    public static NoMatch<IMetadata> ForMember()
        => new NoMatch<IMetadata>(InvalidMemberMetadata.Instance);

    public static NoMatch<IFunctionMetadata> ForFunction()
        => new NoMatch<IFunctionMetadata>(FunctionMetadata.Invalid);

    public static NoMatch<IGenericMetadata> ForGeneric()
        => new NoMatch<IGenericMetadata>(TypeMetadata.InvalidType);

    public static NoMatch<INamedMetadata> ForType()
        => new NoMatch<INamedMetadata>(TypeMetadata.InvalidType);

    public T Member { get; }
}
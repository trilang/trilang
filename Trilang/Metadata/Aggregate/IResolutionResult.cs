namespace Trilang.Metadata.Aggregate;

public interface IResolutionResult<out T> where T : IMetadata
{
    T Member { get; }
}
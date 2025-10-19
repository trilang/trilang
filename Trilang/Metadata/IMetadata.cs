namespace Trilang.Metadata;

public interface IMetadata
{
    // TODO: handle multiple definitions of anonymous inlined types
    SourceLocation? Definition { get; }

    bool IsInvalid { get; }
}
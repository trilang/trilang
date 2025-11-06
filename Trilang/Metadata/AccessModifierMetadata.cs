using Trilang.Semantics.Model;

namespace Trilang.Metadata;

public enum AccessModifierMetadata
{
    Public,
    Internal,
    Private,
}

public static class AccessModifierMetadataExtensions
{
    public static AccessModifierMetadata ToMetadata(this AccessModifier accessModifier)
        => accessModifier switch
        {
            AccessModifier.Public => AccessModifierMetadata.Public,
            AccessModifier.Internal => AccessModifierMetadata.Internal,
            AccessModifier.Private => AccessModifierMetadata.Private,

            _ => throw new ArgumentOutOfRangeException(nameof(accessModifier), accessModifier, null)
        };

    public static AccessModifierMetadata? ToMetadata(this AccessModifier? accessModifier)
        => accessModifier?.ToMetadata();
}
using Trilang.Metadata;

namespace Trilang.Semantics.Model;

public enum AccessModifier
{
    Public,
    Internal,
    Private,
}

public static class AccessModifierExtensions
{
    public static AccessModifier ToSemanticModel(this AccessModifierMetadata accessModifier)
        => accessModifier switch
        {
            AccessModifierMetadata.Public => AccessModifier.Public,
            AccessModifierMetadata.Internal => AccessModifier.Internal,
            AccessModifierMetadata.Private => AccessModifier.Private,

            _ => throw new ArgumentOutOfRangeException(nameof(accessModifier), accessModifier, null)
        };

    public static AccessModifier? ToSemanticModel(this AccessModifierMetadata? accessModifier)
        => accessModifier?.ToSemanticModel();
}
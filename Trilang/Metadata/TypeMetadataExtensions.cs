namespace Trilang.Metadata;

public static class TypeMetadataExtensions
{
    public static ITypeMetadata? UnpackAlias(this ITypeMetadata? type)
    {
        while (type is AliasMetadata alias && !type.IsInvalid)
            type = alias.Type;

        return type;
    }
}
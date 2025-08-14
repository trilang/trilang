namespace Trilang.Metadata;

public static class TypeMetadataExtensions
{
    public static ITypeMetadata? UnpackAlias(this ITypeMetadata? type)
    {
        while (type is TypeAliasMetadata alias)
            type = alias.Type;

        return type;
    }
}
namespace Trilang.Metadata;

public static class TypeMetadataExtensions
{
    public static ITypeMetadata? Unpack(this ITypeMetadata? type)
    {
        while (type is TypeAliasMetadata alias)
            type = alias.Type;

        return type;
    }
}
using Trilang.Metadata;

namespace Trilang.Semantics.Providers;

internal static class MetadataProviderExtensions
{
    public static T GetOrDefine<T>(this IMetadataProvider provider, T type)
        where T : IAnonymousTypeMetadata
        => provider.FindTypes(type.ToString()!) switch
        {
            [] => provider.DefineType(type.ToString()!, type)
                ? type
                : throw new InvalidOperationException(),
            [T existingType] => existingType,
            _ => throw new InvalidOperationException(),
        };
}
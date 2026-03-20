using Trilang.Metadata;

namespace Trilang.Semantics.Providers;

internal static class MetadataProviderExtensions
{
    public static T GetOrDefine<T>(this IMetadataProvider provider, T type)
        where T : IAnonymousTypeMetadata
        => provider.QueryTypes(Query.From(type.ToString()!)) switch // TODO:
        {
            { IsTypeNotFound: true } => provider.DefineType(type.ToString()!, type)
                ? type
                : throw new InvalidOperationException(),
            { IsSuccess: true, Types: [T existingType] } => existingType,
            _ => throw new InvalidOperationException(),
        };
}
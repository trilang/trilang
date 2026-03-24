using System.Runtime.CompilerServices;

namespace Trilang.Metadata;

internal static class MetadataExtensions
{
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static ITypeMetadata Required(this ITypeMetadata? type, [CallerArgumentExpression(nameof(type))] string? paramName = null)
        => type ?? throw new InvalidOperationException($"Expected resolved type metadata, but '{paramName}' was null.");
}
using Trilang.Metadata;

namespace Trilang.Semantics.MetadataGenerators;

public class TypeArgumentMap
{
    private readonly IDictionary<string, ITypeMetadata> map;

    public TypeArgumentMap()
        => map = new Dictionary<string, ITypeMetadata>();

    public static TypeArgumentMap Create(
        IReadOnlyCollection<ITypeMetadata> closedTypes,
        IReadOnlyCollection<ITypeMetadata> openTypes)
    {
        var result = new TypeArgumentMap();

        foreach (var (specific, open) in closedTypes.Zip(openTypes))
        {
            if (specific is TypeArgumentMetadata || specific.Equals(open))
                continue;

            result.map.Add(open.Name, specific);
        }

        return result;
    }

    public ITypeMetadata Map(ITypeMetadata type)
        => type is TypeArgumentMetadata && map.TryGetValue(type.Name, out var mappedType)
            ? mappedType
            : type;
}
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Providers;

public abstract record Query
{
    public static Query From(string name)
        => new ByName(name);

    public static Query From(IInlineType type)
        => type switch
        {
            TypeRef typeRef => From(typeRef),
            _ => new ByName(type.Name)
        };

    public static Query From(TypeRef typeRef)
        => typeRef.Parts switch
        {
            [] => throw new InvalidOperationException(),
            [var name] => new ByName(name),
            _ => new ByQualifiedName(typeRef.Parts),
        };
}

public record ByName(string Name) : Query;

public record ByQualifiedName(IReadOnlyList<string> Parts) : Query;
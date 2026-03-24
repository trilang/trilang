using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Semantics.Providers;

public abstract record Query
{
    public static Query From(IInlineType type)
        => type switch
        {
            ArrayType array => new GetArray(array.ElementType.Metadata.Required()),

            DiscriminatedUnion union => new GetUnion(
                union.Types.Select(t => t.Metadata.Required()).ToArray()),

            FunctionType functionType => new GetFunctionType(
                functionType.ParameterTypes.Select(t => t.Metadata.Required()).ToArray(),
                functionType.ReturnType.Metadata.Required()),

            GenericApplication genericApplication => new GetGenericApplication(
                genericApplication.Type.Metadata.Required(),
                genericApplication.TypeArguments.Select(t => t.Metadata.Required()).ToArray()),

            Interface @interface => new GetInterface(
                @interface.Properties.Select(x =>
                    new GetInterfaceProperty(x.Name, x.Type.Metadata.Required())).ToArray(),
                @interface.Methods.Select(x =>
                    new GetInterfaceMethod(
                        x.Name,
                        x.ParameterTypes.Select(t => t.Metadata.Required()).ToArray(),
                        x.ReturnType.Metadata.Required())).ToArray()),

            TupleType tuple => new GetTuple(
                tuple.Types.Select(t => t.Metadata.Required()).ToArray()),

            TypeRef typeRef => From(typeRef),

            _ => throw new ArgumentOutOfRangeException(nameof(type)),
        };

    public static Query From(TypeRef typeRef)
        => typeRef.Parts switch
        {
            [] => throw new InvalidOperationException(),
            [var name] => new ByName(name),
            _ => new ByQualifiedName(typeRef.Parts),
        };

    public static Query From(IGenericDeclaration node)
    {
        var query = (Query)new ByName(node.Name);
        if (node.IsGeneric)
            query = new GetOpenGeneric(query, node.GenericArguments.Count);

        return query;
    }

    public static Query From(IGenericMetadata node)
    {
        var query = (Query)new ByName(node.Name);
        if (node.IsGeneric)
            query = new GetClosedGeneric(query, node.GenericArguments);

        return query;
    }
}

public record ByName(string Name) : Query;

public record ByQualifiedName(IReadOnlyList<string> Parts) : Query;

public record GetArray(ITypeMetadata ElementType) : Query;

public record GetUnion(IReadOnlyList<ITypeMetadata> Types) : Query;

public record GetFunctionType(IReadOnlyList<ITypeMetadata> Parameters, ITypeMetadata ReturnType) : Query;

public record GetOpenGeneric(Query BaseQuery, int TypeArgumentsCount) : Query;

public record GetClosedGeneric(Query BaseQuery, IReadOnlyList<ITypeMetadata> TypeArguments) : Query;

public record GetGenericApplication(ITypeMetadata OpenGeneric, IReadOnlyList<ITypeMetadata> TypeArguments) : Query;

public record GetInterface(IReadOnlyList<GetInterfaceProperty> Properties, IReadOnlyList<GetInterfaceMethod> Methods) : Query;

public record GetInterfaceProperty(string Name, ITypeMetadata Type);

public record GetInterfaceMethod(string Name, IReadOnlyList<ITypeMetadata> ParameterTypes, ITypeMetadata ReturnType);

public record GetTuple(IReadOnlyList<ITypeMetadata> Types) : Query;
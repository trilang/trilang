using Trilang.Metadata;

namespace Trilang.Semantics.Providers;

public class GenericMetadataProvider : IMetadataProvider
{
    private readonly IMetadataProvider parent;
    private readonly IGenericMetadata genericMetadata;

    public GenericMetadataProvider(IMetadataProvider parent, IGenericMetadata genericMetadata)
    {
        this.parent = parent;
        this.genericMetadata = genericMetadata;
    }

    public override string ToString()
        => $"{genericMetadata} Provider";

    public QueryTypesResult QueryTypes(Query query)
        => query switch
        {
            ByName byName => FindTypes(byName),

            _ => parent.QueryTypes(query),
        };

    private QueryTypesResult FindTypes(ByName query)
    {
        var arguments = genericMetadata.GenericArguments
            .OfType<TypeArgumentMetadata>()
            .Where(x => x.Name == query.Name)
            .ToArray();

        if (arguments.Length > 0)
            return QueryTypesResult.Success(arguments);

        return parent.QueryTypes(query);
    }

    public void DefineType(ITypeMetadata type)
    {
        if (type is TypeArgumentMetadata)
            return;

        parent.DefineType(type);
    }

    public IReadOnlyList<FunctionMetadata> FindFunctions(string name)
        => parent.FindFunctions(name);

    public void AddFunction(FunctionMetadata function)
        => throw new InvalidOperationException("Cannot add function to generic type.");
}
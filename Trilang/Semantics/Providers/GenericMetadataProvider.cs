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

    public IReadOnlyList<ITypeMetadata> FindTypes(string name)
    {
        var arguments = genericMetadata.GenericArguments
            .OfType<TypeArgumentMetadata>()
            .Where(x => x.Name == name)
            .ToArray();

        if (arguments.Length > 0)
            return arguments;

        return parent.FindTypes(name);
    }

    public bool DefineType(string name, ITypeMetadata type)
    {
        if (type is TypeArgumentMetadata)
            return true;

        return parent.DefineType(name, type);
    }

    public IReadOnlyList<FunctionMetadata> FindFunctions(string name)
        => parent.FindFunctions(name);

    public void AddFunction(FunctionMetadata function)
        => throw new InvalidOperationException("Cannot add function to generic type.");
}
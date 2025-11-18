using Trilang.Metadata;

namespace Trilang.IntermediateRepresentation;

internal class Mangler
{
    private ulong hash;

    public string Mangle(FunctionTypeMetadata functionType)
    {
        Reset();

        foreach (var parameterType in functionType.ParameterTypes)
            Mangle(parameterType);

        Mangle(functionType.ReturnType);

        return $"{hash:x16}";
    }

    private void Mangle(ITypeMetadata metadata)
    {
        if (metadata is ArrayMetadata arrayMetadata)
        {
            AddData("array");
            Mangle(arrayMetadata.ItemMetadata!);
        }
        else if (metadata is DiscriminatedUnionMetadata discriminatedUnionMetadata)
        {
            AddData("du");

            foreach (var type in discriminatedUnionMetadata.Types)
                Mangle(type);
        }
        else if (metadata is FunctionTypeMetadata functionTypeMetadata)
        {
            AddData("ft");

            foreach (var parameterType in functionTypeMetadata.ParameterTypes)
                Mangle(parameterType);

            Mangle(functionTypeMetadata.ReturnType);
        }
        else if (metadata is InterfaceMetadata interfaceMetadata)
        {
            AddData("i");

            foreach (var property in interfaceMetadata.Properties)
            {
                AddData(property.Name);
                Mangle(property.Type);
            }

            foreach (var method in interfaceMetadata.Methods)
            {
                AddData(method.Name);
                Mangle(method.Type);
            }
        }
        else if (metadata is TupleMetadata tupleMetadata)
        {
            AddData("tuple");

            foreach (var type in tupleMetadata.Types)
                Mangle(type);
        }
        else if (metadata is AliasMetadata typeAliasMetadata)
        {
            AddData("alias");

            Mangle(typeAliasMetadata.Type!);

            foreach (var genericArgument in typeAliasMetadata.GenericArguments)
                Mangle(genericArgument);
        }
        else if (metadata is TypeArgumentMetadata typeArgumentMetadata)
        {
            AddData("argument");
            AddData(typeArgumentMetadata.Name);
        }
        else if (metadata is TypeMetadata typeMetadata)
        {
            AddData(typeMetadata.Name);

            foreach (var genericArgument in typeMetadata.GenericArguments)
                Mangle(genericArgument);
        }
        else if (metadata is TypePointerMetadata typePointerMetadata)
        {
            AddData("pointer");

            Mangle(typePointerMetadata.Type);
        }
        else
        {
            throw new ArgumentOutOfRangeException(nameof(metadata));
        }
    }

    private void Reset()
        => hash = 0xcbf29ce484222325;

    private void AddData(string data)
    {
        foreach (var c in data)
        {
            hash ^= c;
            hash *= 0x100000001b3;
        }
    }
}
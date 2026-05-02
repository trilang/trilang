using System.Diagnostics;

namespace Trilang.Metadata;

public class RootNamespaceMetadata : INamespaceMetadata
{
    private readonly List<ITypeMetadata> types; // TODO: hashset/dictionary?
    private readonly List<RootNamespaceMetadata> imported;

    private RootNamespaceMetadata(BuiltInTypes builtInTypes)
    {
        types =
        [
            builtInTypes.Void,
            builtInTypes.Null,
            builtInTypes.I8,
            builtInTypes.I16,
            builtInTypes.I32,
            builtInTypes.I64,
            builtInTypes.U8,
            builtInTypes.U16,
            builtInTypes.U32,
            builtInTypes.U64,
            builtInTypes.F32,
            builtInTypes.F64,
            builtInTypes.Bool,
            builtInTypes.Char,
            builtInTypes.String
        ];
        imported = [];
    }

    public static RootNamespaceMetadata Create(BuiltInTypes builtInTypes)
        => new RootNamespaceMetadata(builtInTypes);

    public override string ToString()
        => "Root Namespace";

    public void AddType(ITypeMetadata type)
    {
        Debug.Assert(
            type is IAnonymousTypeMetadata or INamedMetadata { IsCompilerGenerated: true },
            "Only anonymous types can be added to root namespace.");
        Debug.Assert(type.Namespace is null, "Type already belongs to another namespace.");

        type.Namespace = this;

        types.Add(type);
    }

    public void AddFunction(FunctionMetadata function)
        => throw new NotSupportedException();

    public IEnumerable<FunctionMetadata> FindFunction(string name)
        => throw new NotSupportedException();

    public void AddImported(RootNamespaceMetadata namespaceMetadata)
        => imported.Add(namespaceMetadata);

    public IEnumerable<ITypeMetadata> EnumerateAllTypes()
    {
        foreach (var type in types)
            yield return type;

        foreach (var importedNamespace in imported)
        foreach (var type in importedNamespace.EnumerateAllTypes())
            yield return type;
    }

    public SourceLocation? Definition
        => null;

    public bool IsInvalid
        => false;

    public IReadOnlyCollection<ITypeMetadata> Types
        => types;

    public IReadOnlyCollection<RootNamespaceMetadata> Imported
        => imported;
}
namespace Trilang.Metadata;

public class DiscriminatedUnionMetadata : IAnonymousTypeMetadata
{
    private readonly List<ITypeMetadata> types;

    public DiscriminatedUnionMetadata(SourceLocation? definition, IEnumerable<ITypeMetadata> types)
    {
        Definition = definition;
        this.types = [.. types];
    }

    public override string ToString()
        => string.Join(" | ", types);

    public IMetadata? GetMember(string name)
        => null;

    public void MarkAsInvalid()
        => IsInvalid = true;

    public bool IsInvalid { get; private set; }

    public SourceLocation? Definition { get; }

    public bool IsValueType
        => true;

    public TypeLayout? Layout { get; set; }

    public INamespaceMetadata? Namespace { get; set; }

    public IReadOnlyList<ITypeMetadata> Types
        => types;
}
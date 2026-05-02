namespace Trilang.Metadata;

public class TypeArgumentMetadata : ITypeMetadata
{
    public TypeArgumentMetadata(SourceLocation? definition, string name)
    {
        Definition = definition;
        Name = name;
    }

    public static TypeArgumentMetadata Invalid(string name)
        => new TypeArgumentMetadata(null, name) { IsInvalid = true };

    public override int GetHashCode()
        => HashCode.Combine(Name);

    public override string ToString()
        => Name;

    public IMetadata? GetMember(string name)
        => null;

    public bool IsInvalid { get; private set; }

    public SourceLocation? Definition { get; }

    public bool IsValueType
        => throw new NotSupportedException();

    public TypeLayout? Layout { get; set; }

    public INamespaceMetadata? Namespace
    {
        get => throw new NotSupportedException();
        set => throw new NotSupportedException();
    }

    public string Name { get; }
}
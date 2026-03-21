namespace Trilang.Metadata;

public class ParameterMetadata : IMetadata
{
    public ParameterMetadata(SourceLocation? definition, string name, ITypeMetadata type)
    {
        Definition = definition;
        Name = name;
        Type = type;
    }

    public override string ToString()
        => $"{Name}: {Type}";

    public void MarkAsInvalid()
        => IsInvalid = true;

    public SourceLocation? Definition { get; }

    public bool IsInvalid { get; private set; }

    public string Name { get; }

    public ITypeMetadata Type { get; }
}
using Trilang.Metadata;

namespace Trilang.Symbols;

// TODO: immutable?
public class IdSymbol
{
    public IdSymbol(string name)
        => Name = name;

    public override string ToString()
        => $"Id: {Name}";

    public string Name { get; }

    public IMetadata? Metadata { get; set; }
}
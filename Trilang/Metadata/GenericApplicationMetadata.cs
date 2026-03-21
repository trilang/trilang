using System.Diagnostics;

namespace Trilang.Metadata;

public class GenericApplicationMetadata : IAnonymousTypeMetadata
{
    public GenericApplicationMetadata(
        SourceLocation? definition,
        IGenericMetadata openGeneric,
        IEnumerable<ITypeMetadata> arguments)
    {
        Debug.Assert(openGeneric is TypeMetadata or AliasMetadata);

        Definition = definition;
        OpenGeneric = openGeneric;
        Arguments = [..arguments];
    }

    public override string ToString()
        => OpenGeneric switch
        {
            AliasMetadata alias => $"{alias.Name}<{string.Join(", ", Arguments)}>",
            TypeMetadata type => $"{type.Name}<{string.Join(", ", Arguments)}>",
            _ => throw new InvalidOperationException(),
        };

    public IMetadata? GetMember(string name)
        => ClosedGeneric?.GetMember(name);

    public SourceLocation? Definition { get; }

    public bool IsInvalid
        => OpenGeneric.IsInvalid;

    public bool IsValueType
        => OpenGeneric.IsValueType;

    // TODO: generate
    public TypeLayout? Layout { get; set; }

    public NamespaceMetadata? Namespace { get; set; }

    public IGenericMetadata OpenGeneric { get; }

    public IReadOnlyList<ITypeMetadata> Arguments { get; }

    public ITypeMetadata? ClosedGeneric { get; set; }
}
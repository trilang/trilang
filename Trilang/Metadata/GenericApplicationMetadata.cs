using System.Diagnostics;
using Trilang.Metadata.Aggregate;

namespace Trilang.Metadata;

public class GenericApplicationMetadata : IAnonymousTypeMetadata
{
    private bool isFrozen;

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

    public AggregateMetadata GetMembers(string name)
        => ClosedGeneric?.GetMembers(name) ?? AggregateMetadata.Empty;

    public void Freeze()
    {
        isFrozen = true;

        ClosedGeneric?.Freeze();
    }

    private void EnsureNotFrozen()
    {
        if (isFrozen)
            throw new InvalidOperationException("Cannot modify frozen metadata.");
    }

    public SourceLocation? Definition { get; }

    public bool IsInvalid
        => OpenGeneric.IsInvalid;

    public bool IsValueType
        => OpenGeneric.IsValueType;

    // TODO: generate
    public TypeLayout? Layout
    {
        get;
        set
        {
            EnsureNotFrozen();
            field = value;
        }
    }

    public INamespaceMetadata? Namespace
    {
        get;
        set
        {
            EnsureNotFrozen();
            field = value;
        }
    }

    public IGenericMetadata OpenGeneric { get; }

    public IReadOnlyList<ITypeMetadata> Arguments { get; }

    public ITypeMetadata? ClosedGeneric
    {
        get;
        set
        {
            EnsureNotFrozen();
            field = value;
        }
    }
}
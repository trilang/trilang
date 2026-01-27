using System.Diagnostics;

namespace Trilang.Metadata;

public class GenericApplicationMetadata : ITypeMetadata, IEquatable<GenericApplicationMetadata>
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

    public static bool operator ==(GenericApplicationMetadata? left, GenericApplicationMetadata? right)
        => Equals(left, right);

    public static bool operator !=(GenericApplicationMetadata? left, GenericApplicationMetadata? right)
        => !Equals(left, right);

    public bool Equals(GenericApplicationMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return OpenGeneric.Equals(other.OpenGeneric) &&
               Arguments.Equals(other.Arguments);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((GenericApplicationMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(OpenGeneric, Arguments);

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

    public IGenericMetadata OpenGeneric { get; }

    public IReadOnlyList<ITypeMetadata> Arguments { get; }

    public ITypeMetadata? ClosedGeneric { get; set; }
}
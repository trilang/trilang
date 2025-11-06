namespace Trilang.Metadata;

public class DiscriminatedUnionMetadata : ITypeMetadata, IEquatable<DiscriminatedUnionMetadata>
{
    private readonly List<ITypeMetadata> types;

    public DiscriminatedUnionMetadata(SourceLocation? definition) : this(definition, [])
    {
    }

    public DiscriminatedUnionMetadata(SourceLocation? definition, IEnumerable<ITypeMetadata> types)
    {
        Definition = definition;
        this.types = [.. types];
    }

    public static bool operator ==(DiscriminatedUnionMetadata? left, DiscriminatedUnionMetadata? right)
        => Equals(left, right);

    public static bool operator !=(DiscriminatedUnionMetadata? left, DiscriminatedUnionMetadata? right)
        => !Equals(left, right);

    public bool Equals(DiscriminatedUnionMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        if (IsInvalid || other.IsInvalid)
            return false;

        return types.SequenceEqual(other.types);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((DiscriminatedUnionMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(types);

    public override string ToString()
        => string.Join(" | ", types);

    public void AddType(ITypeMetadata type)
        => types.Add(type);

    public bool HasType(TypeMetadata type)
        => types.Contains(type);

    public IMetadata? GetMember(string name)
        => null;

    public bool IsInvalid => false;

    public SourceLocation? Definition { get; }

    public bool IsValueType
        => true;

    public TypeLayout? Layout { get; set; }

    public IReadOnlyList<ITypeMetadata> Types => types;
}
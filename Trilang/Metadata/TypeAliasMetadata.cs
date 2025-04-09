namespace Trilang.Metadata;

public class TypeAliasMetadata : IMetadata, IEquatable<TypeAliasMetadata>
{
    public TypeAliasMetadata(string name, IMetadata type)
    {
        Name = name;
        Type = type;
    }

    public static bool operator ==(TypeAliasMetadata? left, TypeAliasMetadata? right)
        => Equals(left, right);

    public static bool operator !=(TypeAliasMetadata? left, TypeAliasMetadata? right)
        => !Equals(left, right);

    public bool Equals(TypeAliasMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name && Type.Equals(other.Type);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((TypeAliasMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, Type);

    public string Name { get; }

    public IMetadata Type { get; }
}
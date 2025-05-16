namespace Trilang.Metadata;

public class TypeArrayMetadata : ITypeMetadata, IEquatable<TypeArrayMetadata>
{
    public TypeArrayMetadata(string name)
        => Name = name;

    public static bool operator ==(TypeArrayMetadata? left, TypeArrayMetadata? right)
        => Equals(left, right);

    public static bool operator !=(TypeArrayMetadata? left, TypeArrayMetadata? right)
        => !Equals(left, right);

    public bool Equals(TypeArrayMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name;
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((TypeArrayMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name);

    public override string ToString()
        => Name;

    public string Name { get; }

    public ITypeMetadata? ItemMetadata { get; set; }
}
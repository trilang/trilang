namespace Trilang.Metadata;

public class TypeArgumentMetadata : ITypeMetadata, IEquatable<TypeArgumentMetadata>
{
    public TypeArgumentMetadata(string name)
        => Name = name;

    public static bool operator ==(TypeArgumentMetadata? left, TypeArgumentMetadata? right)
        => Equals(left, right);

    public static bool operator !=(TypeArgumentMetadata? left, TypeArgumentMetadata? right)
        => !Equals(left, right);

    public bool Equals(TypeArgumentMetadata? other)
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

        return Equals((TypeArgumentMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name);

    public override string ToString()
        => Name;

    public IMetadata? GetMember(string name)
        => null;

    public string Name { get; }

    public bool IsValueType
        => throw new NotSupportedException();

    public TypeLayout? Layout { get; set; }
}
namespace Trilang.Metadata;

public class TypeAliasMetadata : ITypeMetadata, IEquatable<TypeAliasMetadata>
{
    public TypeAliasMetadata(string name) : this(name, null)
    {
    }

    public TypeAliasMetadata(string name, ITypeMetadata? type)
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

        return Equals((TypeAliasMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name);

    public override string ToString()
        => Name;

    public string Name { get; }

    public ITypeMetadata? Type { get; set; }
}
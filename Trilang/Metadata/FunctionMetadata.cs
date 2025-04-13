namespace Trilang.Metadata;

public class FunctionMetadata : IMetadata, IEquatable<FunctionMetadata>
{
    public FunctionMetadata(string name, FunctionTypeMetadata typeMetadata)
    {
        Name = name;
        TypeMetadata = typeMetadata;
    }

    public static bool operator ==(FunctionMetadata? left, FunctionMetadata? right)
        => Equals(left, right);

    public static bool operator !=(FunctionMetadata? left, FunctionMetadata? right)
        => !Equals(left, right);

    public bool Equals(FunctionMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name &&
               TypeMetadata.Equals(other.TypeMetadata);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((FunctionMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, TypeMetadata);

    public string Name { get; }

    public FunctionTypeMetadata TypeMetadata { get; }
}
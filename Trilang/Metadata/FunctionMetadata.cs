namespace Trilang.Metadata;

public class FunctionMetadata : IMetadata, IEquatable<FunctionMetadata>
{
    public FunctionMetadata(string name, TypeMetadata[] parameterTypes, TypeMetadata returnType)
    {
        Name = name;
        ParameterTypes = parameterTypes;
        ReturnType = returnType;
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
               ParameterTypes.SequenceEqual(other.ParameterTypes) &&
               ReturnType.Equals(other.ReturnType);
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
        => HashCode.Combine(Name, ParameterTypes, ReturnType);

    public string Name { get; }

    public IReadOnlyList<TypeMetadata> ParameterTypes { get; }

    public TypeMetadata ReturnType { get; }
}
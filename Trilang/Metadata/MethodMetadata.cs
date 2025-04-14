namespace Trilang.Metadata;

public class MethodMetadata : IMetadata, IEquatable<MethodMetadata>
{
    public MethodMetadata(
        TypeMetadata declaringType,
        AccessModifierMetadata accessModifier,
        string name,
        FunctionTypeMetadata typeMetadata)
    {
        DeclaringType = declaringType;
        AccessModifier = accessModifier;
        Name = name;
        TypeMetadata = typeMetadata;
    }

    public static bool operator ==(MethodMetadata? left, MethodMetadata? right)
        => Equals(left, right);

    public static bool operator !=(MethodMetadata? left, MethodMetadata? right)
        => !Equals(left, right);

    public bool Equals(MethodMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return DeclaringType.Equals(other.DeclaringType) &&
               AccessModifier == other.AccessModifier &&
               Name == other.Name &&
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

        return Equals((MethodMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine((int)AccessModifier, Name, TypeMetadata);

    public override string ToString()
        => $"{Name}: {TypeMetadata}";

    public TypeMetadata DeclaringType { get; }

    public AccessModifierMetadata AccessModifier { get; }

    public string Name { get; }

    public FunctionTypeMetadata TypeMetadata { get; }
}
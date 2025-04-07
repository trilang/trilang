namespace Trilang.Metadata;

public class MethodMetadata : FunctionMetadata, IEquatable<MethodMetadata>
{
    public MethodMetadata(
        AccessModifierMetadata accessModifier,
        string name,
        IReadOnlyList<IMetadata> parameterTypes,
        IMetadata returnType)
        : base(name, parameterTypes, returnType)
    {
        AccessModifier = accessModifier;
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

        return base.Equals(other) && AccessModifier == other.AccessModifier;
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
        => HashCode.Combine(base.GetHashCode(), (int)AccessModifier);

    public AccessModifierMetadata AccessModifier { get; }
}
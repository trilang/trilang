namespace Trilang.Metadata;

public class MethodMetadata : IMetadata, IEquatable<MethodMetadata>
{
    public MethodMetadata(
        TypeMetadata declaringType,
        AccessModifierMetadata accessModifier,
        bool isStatic,
        string name,
        IReadOnlyList<ParameterMetadata> parameters,
        FunctionTypeMetadata typeMetadata)
    {
        DeclaringType = declaringType;
        AccessModifier = accessModifier;
        IsStatic = isStatic;
        Name = name;
        Parameters = parameters;
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
               IsStatic == other.IsStatic &&
               Name == other.Name &&
               Parameters.SequenceEqual(other.Parameters) &&
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
        => HashCode.Combine((int)AccessModifier, IsStatic, Name, Parameters, TypeMetadata);

    public override string ToString()
        => $"{Name}: {TypeMetadata}";

    public TypeMetadata DeclaringType { get; }

    public AccessModifierMetadata AccessModifier { get; }

    public bool IsStatic { get; }

    public string Name { get; }

    public IReadOnlyList<ParameterMetadata> Parameters { get; }

    public FunctionTypeMetadata TypeMetadata { get; }
}
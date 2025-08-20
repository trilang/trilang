namespace Trilang.Metadata;

public class ConstructorMetadata : IFunctionMetadata, IEquatable<ConstructorMetadata>
{
    public ConstructorMetadata(
        ITypeMetadata declaringType,
        AccessModifierMetadata accessModifier,
        IReadOnlyList<ParameterMetadata> parameters,
        FunctionTypeMetadata type)
    {
        DeclaringType = declaringType;
        AccessModifier = accessModifier;
        Parameters = parameters;
        Type = type;
    }

    public static bool operator ==(ConstructorMetadata? left, ConstructorMetadata? right)
        => Equals(left, right);

    public static bool operator !=(ConstructorMetadata? left, ConstructorMetadata? right)
        => !Equals(left, right);

    public bool Equals(ConstructorMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return DeclaringType.Equals(other.DeclaringType) &&
               AccessModifier == other.AccessModifier &&
               Parameters.SequenceEqual(other.Parameters) &&
               Type.Equals(other.Type);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((ConstructorMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(AccessModifier, Parameters, Type);

    public override string ToString()
        => $"ctor: {Type}";

    public ITypeMetadata DeclaringType { get; }

    public bool IsStatic => false;

    public string Name => "ctor";

    public AccessModifierMetadata AccessModifier { get; }

    public IReadOnlyList<ParameterMetadata> Parameters { get; }

    public FunctionTypeMetadata Type { get; }
}
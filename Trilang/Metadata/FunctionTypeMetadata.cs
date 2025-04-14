namespace Trilang.Metadata;

public class FunctionTypeMetadata : ITypeMetadata, IEquatable<FunctionTypeMetadata>
{
    public FunctionTypeMetadata(IReadOnlyList<ITypeMetadata> parameterTypes, ITypeMetadata returnType)
    {
        var parameters = string.Join(", ", parameterTypes.Select(p => p.Name));
        Name = $"({parameters}) => {returnType.Name}";
        ParameterTypes = parameterTypes;
        ReturnType = returnType;
    }

    public static bool operator ==(FunctionTypeMetadata? left, FunctionTypeMetadata? right)
        => Equals(left, right);

    public static bool operator !=(FunctionTypeMetadata? left, FunctionTypeMetadata? right)
        => !Equals(left, right);

    public bool Equals(FunctionTypeMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return ReturnType.Equals(other.ReturnType) &&
               ParameterTypes.SequenceEqual(other.ParameterTypes);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((FunctionTypeMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(ParameterTypes, ReturnType);

    public override string ToString()
        => Name;

    public string Name { get; }

    public IReadOnlyList<ITypeMetadata> ParameterTypes { get; }

    public ITypeMetadata ReturnType { get; }
}
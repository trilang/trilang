namespace Trilang.Metadata;

public class FunctionTypeMetadata : ITypeMetadata, IEquatable<FunctionTypeMetadata>
{
    private readonly HashSet<ITypeMetadata> parameterTypes;

    public FunctionTypeMetadata(string name)
    {
        Name = name;
        parameterTypes = [];
        ReturnType = null!;
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

        return Equals((FunctionTypeMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name);

    public override string ToString()
        => Name;

    public void AddParameter(ITypeMetadata parameter)
        => parameterTypes.Add(parameter);

    public string Name { get; }

    public IReadOnlyCollection<ITypeMetadata> ParameterTypes => parameterTypes;

    public ITypeMetadata ReturnType { get; set; }
}
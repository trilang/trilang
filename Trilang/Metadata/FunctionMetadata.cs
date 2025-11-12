namespace Trilang.Metadata;

public class FunctionMetadata : IFunctionMetadata, IEquatable<FunctionMetadata>
{
    private readonly List<ParameterMetadata> parameters;

    public FunctionMetadata(
        SourceLocation? definition,
        AccessModifierMetadata accessModifier,
        string name,
        IReadOnlyList<ParameterMetadata> parameters,
        FunctionTypeMetadata type,
        FunctionGroupMetadata group)
    {
        Definition = definition;
        AccessModifier = accessModifier;
        Name = name;
        this.parameters = [..parameters];
        Type = type;
        Group = group;

        group.AddFunction(this);
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

        if (IsInvalid || other.IsInvalid)
            return false;

        return AccessModifier == other.AccessModifier &&
               Name == other.Name &&
               parameters.SequenceEqual(other.parameters) &&
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

        return Equals((FunctionMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, parameters, Type);

    public override string ToString()
        => $"{Name}: {Type}";

    public void AddParameter(ParameterMetadata parameter)
        => parameters.Add(parameter);

    public void MarkAsInvalid()
        => IsInvalid = true;

    public SourceLocation? Definition { get; }

    public bool IsInvalid { get; private set; }

    public ITypeMetadata? DeclaringType => null;

    public bool IsStatic => true;

    public AccessModifierMetadata AccessModifier { get; }

    public string Name { get; }

    public IReadOnlyList<ParameterMetadata> Parameters => parameters;

    public FunctionTypeMetadata Type { get; set; }

    public FunctionGroupMetadata Group { get; }
}
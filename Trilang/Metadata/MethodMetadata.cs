namespace Trilang.Metadata;

public class MethodMetadata : IFunctionMetadata, IEquatable<MethodMetadata>
{
    public MethodMetadata(
        SourceLocation? definition,
        ITypeMetadata declaringType,
        AccessModifierMetadata accessModifier,
        bool isStatic,
        string name,
        IReadOnlyList<ParameterMetadata> parameters,
        FunctionTypeMetadata type,
        FunctionGroupMetadata group)
    {
        Definition = definition;
        DeclaringType = declaringType;
        AccessModifier = accessModifier;
        IsStatic = isStatic;
        Name = name;
        Parameters = parameters;
        Type = type;
        Group = group;

        group.AddFunction(this);
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

        if (IsInvalid || other.IsInvalid)
            return false;

        return DeclaringType.Equals(other.DeclaringType) &&
               AccessModifier == other.AccessModifier &&
               IsStatic == other.IsStatic &&
               Name == other.Name &&
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

        return Equals((MethodMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine((int)AccessModifier, IsStatic, Name, Parameters, Type);

    public override string ToString()
        => $"{Name}: {Type}";

    public void MarkAsInvalid()
        => IsInvalid = true;

    public SourceLocation? Definition { get; }

    public bool IsInvalid { get; private set; }

    public ITypeMetadata DeclaringType { get; }

    public AccessModifierMetadata AccessModifier { get; }

    public bool IsStatic { get; }

    public string Name { get; }

    public IReadOnlyList<ParameterMetadata> Parameters { get; }

    public FunctionTypeMetadata Type { get; }

    public FunctionGroupMetadata Group { get; }
}
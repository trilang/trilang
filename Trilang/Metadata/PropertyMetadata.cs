using Trilang.Semantics.Model;

namespace Trilang.Metadata;

public class PropertyMetadata : IMetadata, IEquatable<PropertyMetadata>
{
    public PropertyMetadata(ITypeMetadata declaringType,
        string name,
        ITypeMetadata type,
        AccessModifierMetadata? getterModifier = null,
        AccessModifierMetadata? setterModifier = null)
    {
        DeclaringType = declaringType;
        Name = name;
        Type = type;

        var hasGetter = getterModifier is not null;
        var hasSetter = setterModifier is not null;

        if (!hasGetter && !hasSetter)
        {
            Getter = GenerateGetter(AccessModifierMetadata.Public);
            Setter = GenerateSetter(AccessModifierMetadata.Private);
        }
        else
        {
            if (hasGetter)
                Getter = GenerateGetter(getterModifier!.Value);

            if (hasSetter)
                Setter = GenerateSetter(setterModifier!.Value);
        }
    }

    public PropertyMetadata(
        TypeMetadata declaringType,
        string name,
        ITypeMetadata type,
        MethodMetadata? getter,
        MethodMetadata? setter)
    {
        DeclaringType = declaringType;
        Name = name;
        Type = type;
        Getter = getter;
        Setter = setter;
    }

    public static bool operator ==(PropertyMetadata? left, PropertyMetadata? right)
        => Equals(left, right);

    public static bool operator !=(PropertyMetadata? left, PropertyMetadata? right)
        => !Equals(left, right);

    public bool Equals(PropertyMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Equals(DeclaringType, other.DeclaringType) &&
               Name == other.Name &&
               Type.Equals(other.Type) &&
               Getter == other.Getter &&
               Setter == other.Setter;
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((PropertyMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name, Type, Getter, Setter);

    public override string ToString()
        => $"{Name}: {Type}";

    private MethodMetadata GenerateGetter(AccessModifierMetadata getterModifier)
        => new MethodMetadata(
            DeclaringType,
            getterModifier,
            false,
            $"<>_get_{Name}",
            [],
            new FunctionTypeMetadata([], Type)
        );

    private MethodMetadata GenerateSetter(AccessModifierMetadata setterModifier)
        => new MethodMetadata(
            DeclaringType,
            setterModifier,
            false,
            $"<>_set_{Name}",
            [new ParameterMetadata(MemberAccessExpression.Value, Type)],
            new FunctionTypeMetadata([Type], TypeMetadata.Void)
        );

    public ITypeMetadata DeclaringType { get; }

    public string Name { get; }

    public MethodMetadata? Getter { get; }

    public MethodMetadata? Setter { get; }

    public ITypeMetadata Type { get; }
}
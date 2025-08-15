using Trilang.Parsing.Ast;

namespace Trilang.Metadata;

public class PropertyMetadata : IMetadata, IEquatable<PropertyMetadata>
{
    public PropertyMetadata(
        ITypeMetadata declaringType,
        string name,
        ITypeMetadata type,
        AccessModifierMetadata getterModifier = AccessModifierMetadata.Public,
        AccessModifierMetadata setterModifier = AccessModifierMetadata.Private)
    {
        DeclaringType = declaringType;
        Name = name;
        Type = type;
        Getter = new MethodMetadata(
            declaringType,
            getterModifier,
            false,
            $"<>_get_{name}",
            [],
            new FunctionTypeMetadata([], type)
        );
        Setter = new MethodMetadata(
            declaringType,
            setterModifier,
            false,
            $"<>_set_{name}",
            [new ParameterMetadata(MemberAccessExpressionNode.Value, type)],
            new FunctionTypeMetadata([type], TypeMetadata.Void)
        );
    }

    public PropertyMetadata(
        TypeMetadata declaringType,
        string name,
        ITypeMetadata type,
        MethodMetadata getter,
        MethodMetadata setter)
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

        return DeclaringType == other.DeclaringType &&
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

    public ITypeMetadata DeclaringType { get; }

    public string Name { get; }

    public MethodMetadata Getter { get; }

    public MethodMetadata Setter { get; }

    public ITypeMetadata Type { get; }
}
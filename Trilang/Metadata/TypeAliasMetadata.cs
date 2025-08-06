using System.Text;

namespace Trilang.Metadata;

public class TypeAliasMetadata : ITypeMetadata, IEquatable<TypeAliasMetadata>
{
    private readonly List<ITypeMetadata> genericArguments;

    public TypeAliasMetadata(string name) : this(name, [], null)
    {
    }

    public TypeAliasMetadata(string name, IEnumerable<ITypeMetadata> genericArguments, ITypeMetadata? type)
    {
        Name = name;
        this.genericArguments = [..genericArguments];
        Type = type;
    }

    public static bool operator ==(TypeAliasMetadata? left, TypeAliasMetadata? right)
        => Equals(left, right);

    public static bool operator !=(TypeAliasMetadata? left, TypeAliasMetadata? right)
        => !Equals(left, right);

    public bool Equals(TypeAliasMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        return Name == other.Name &&
               genericArguments.SequenceEqual(genericArguments);
    }

    public override bool Equals(object? obj)
    {
        if (obj is null)
            return false;

        if (ReferenceEquals(this, obj))
            return true;

        if (obj.GetType() != GetType())
            return false;

        return Equals((TypeAliasMetadata)obj);
    }

    public override int GetHashCode()
        => HashCode.Combine(Name);

    public override string ToString()
    {
        if (genericArguments.Count == 0)
            return Name;

        var sb = new StringBuilder();
        sb.Append(Name);
        sb.Append('<');

        for (var i = 0; i < genericArguments.Count; i++)
        {
            var genericArgument = genericArguments[i];
            if (genericArgument is not TypeArgumentMetadata)
                sb.Append(genericArgument);

            if (i < genericArguments.Count - 1)
                sb.Append(',');
        }

        sb.Append('>');

        return sb.ToString();
    }

    public void AddGenericArgument(ITypeMetadata genericArgument)
        => genericArguments.Add(genericArgument);

    public string Name { get; }

    public IReadOnlyCollection<ITypeMetadata> GenericArguments => genericArguments;

    public ITypeMetadata? Type { get; set; }

    public bool IsValueType
        => Type?.IsValueType ?? false;
}
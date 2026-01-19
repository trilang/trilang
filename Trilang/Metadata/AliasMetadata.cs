using System.Text;

namespace Trilang.Metadata;

public class AliasMetadata : ITypeMetadata, IEquatable<AliasMetadata>
{
    private readonly List<ITypeMetadata> genericArguments;

    public AliasMetadata(SourceLocation? definition, string name) : this(definition, name, [], null)
    {
    }

    public AliasMetadata(
        SourceLocation? definition,
        string name,
        IEnumerable<ITypeMetadata> genericArguments,
        ITypeMetadata? type)
    {
        Definition = definition;
        Name = name;
        this.genericArguments = [.. genericArguments];
        Type = type;
    }

    public static bool operator ==(AliasMetadata? left, AliasMetadata? right)
        => Equals(left, right);

    public static bool operator !=(AliasMetadata? left, AliasMetadata? right)
        => !Equals(left, right);

    public bool Equals(AliasMetadata? other)
    {
        if (other is null)
            return false;

        if (ReferenceEquals(this, other))
            return true;

        if (IsInvalid || other.IsInvalid)
            return false;

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

        return Equals((AliasMetadata)obj);
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

    public IMetadata? GetMember(string name)
    {
        if (IsInvalid)
            return null;

        return Type?.GetMember(name);
    }

    public void MarkAsInvalid()
        => IsInvalid = true;

    public bool IsInvalid { get; private set; }

    public SourceLocation? Definition { get; }

    public bool IsValueType
    {
        get
        {
            if (IsInvalid)
                return false;

            return Type?.IsValueType ?? false;
        }
    }

    public TypeLayout? Layout { get; set; }

    public string Name { get; }

    public IReadOnlyCollection<ITypeMetadata> GenericArguments
        => genericArguments;

    public ITypeMetadata? Type { get; set; }

    public AliasMetadata? OpenGenericType { get; set; }
}
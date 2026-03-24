using System.Text;

namespace Trilang.Metadata;

public class AliasMetadata : IGenericMetadata, INamedMetadata
{
    private readonly List<ITypeMetadata> genericArguments;

    public AliasMetadata(SourceLocation? definition, string name)
        : this(definition, name, [], null)
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

    public override string ToString()
    {
        if (!IsGeneric)
            return Name;

        var sb = new StringBuilder();

        if (genericArguments.Any(x => x is not TypeArgumentMetadata))
            sb.Append("<>_");

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

    public NamespaceMetadata? Namespace { get; set; }

    public string Name { get; }

    public IReadOnlyList<ITypeMetadata> GenericArguments
        => genericArguments;

    public ITypeMetadata? Type { get; set; }

    public bool IsGeneric => genericArguments.Count > 0;
}
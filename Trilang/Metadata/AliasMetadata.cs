using System.Text;

namespace Trilang.Metadata;

public class AliasMetadata : IGenericMetadata, INamedMetadata
{
    private readonly List<ITypeMetadata> genericArguments;
    private bool isFrozen;

    public AliasMetadata(SourceLocation? definition, string name)
        : this(definition, name, [], null, false)
    {
    }

    public AliasMetadata(
        SourceLocation? definition,
        string name,
        IEnumerable<ITypeMetadata> genericArguments,
        ITypeMetadata? type,
        bool isCompilerGenerated)
    {
        Definition = definition;
        Name = name;
        this.genericArguments = [.. genericArguments];
        Type = type;
        IsCompilerGenerated = isCompilerGenerated;
    }

    public override string ToString()
    {
        if (!IsGeneric)
            return Name;

        var sb = new StringBuilder();

        if (IsCompilerGenerated)
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
    {
        EnsureNotFrozen();
        genericArguments.Add(genericArgument);
    }

    public AggregateMetadata GetMembers(string name)
        => IsInvalid || Type is null
            ? AggregateMetadata.Empty
            : Type.GetMembers(name);

    public void MarkAsInvalid()
    {
        EnsureNotFrozen();
        IsInvalid = true;
    }

    public void Freeze()
    {
        isFrozen = true;

        foreach (var genericArgument in genericArguments)
            genericArgument.Freeze();
    }

    private void EnsureNotFrozen()
    {
        if (isFrozen)
            throw new InvalidOperationException("Cannot modify frozen metadata.");
    }

    public bool IsInvalid
    {
        get;
        private set
        {
            EnsureNotFrozen();
            field = value;
        }
    }

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

    public TypeLayout? Layout
    {
        get;
        set
        {
            EnsureNotFrozen();
            field = value;
        }
    }

    public INamespaceMetadata? Namespace
    {
        get;
        set
        {
            EnsureNotFrozen();
            field = value;
        }
    }

    public string Name { get; }

    public bool IsCompilerGenerated { get; }

    public IReadOnlyList<ITypeMetadata> GenericArguments
        => genericArguments;

    public ITypeMetadata? Type
    {
        get;
        set
        {
            EnsureNotFrozen();
            field = value;
        }
    }

    public bool IsGeneric => genericArguments.Count > 0;
}
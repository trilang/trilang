namespace Trilang.Metadata;

public class InterfacePropertyMetadata : ITypedMetadata
{
    private bool isFrozen;

    public InterfacePropertyMetadata(
        SourceLocation? definition,
        InterfaceMetadata declaringType,
        string name,
        ITypeMetadata type,
        AccessModifierMetadata? getterModifier,
        AccessModifierMetadata? setterModifier)
    {
        Definition = definition;
        DeclaringType = declaringType;
        Name = name;
        Type = type;
        GetterModifier = getterModifier;
        SetterModifier = setterModifier;
    }

    public override string ToString()
        => $"{Name}: {Type}";

    public void MarkAsInvalid()
    {
        EnsureNotFrozen();
        IsInvalid = true;
    }

    public void Freeze()
        => isFrozen = true;

    private void EnsureNotFrozen()
    {
        if (isFrozen)
            throw new InvalidOperationException("Cannot modify frozen metadata.");
    }

    public SourceLocation? Definition { get; }

    public bool IsInvalid
    {
        get;
        private set
        {
            EnsureNotFrozen();
            field = value;
        }
    }

    public InterfaceMetadata DeclaringType { get; }

    public string Name { get; }

    public ITypeMetadata Type { get; }

    public AccessModifierMetadata? GetterModifier { get; }

    public AccessModifierMetadata? SetterModifier { get; }
}
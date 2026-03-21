namespace Trilang.Metadata;

public class InterfacePropertyMetadata : IMetadata
{
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
        => IsInvalid = true;

    public SourceLocation? Definition { get; }

    public bool IsInvalid { get; private set; }

    public InterfaceMetadata DeclaringType { get; }

    public string Name { get; }

    public ITypeMetadata Type { get; }

    public AccessModifierMetadata? GetterModifier { get; }

    public AccessModifierMetadata? SetterModifier { get; }
}
namespace Trilang.OutputFormats.Elf;

internal enum SectionHeaderType : uint
{
    Null = 0x00000000,
    ProgramData = 0x00000001,
    SymbolTable = 0x00000002,
    StringTable = 0x00000003,
    RelocationEntries = 0x00000004,
    SymbolHashTable = 0x00000005,
    DynamicLinkingInfo = 0x00000006,
    Note = 0x00000007,
    // TODO: ...
}
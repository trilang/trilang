namespace Trilang.OutputFormats.Elf;

[Flags]
internal enum SectionHeaderFlags : ulong
{
    None = 0x00000000,
    Writable = 0x00000001,
    Alloc = 0x00000002,
    Executable = 0x00000004,
    Merge = 0x00000010,
    Strings = 0x00000020,
    InfoLink = 0x00000040,
    LinkOrder = 0x00000080,
    Group = 0x00000200,
    ThreadLocalStorage = 0x00000400,
}
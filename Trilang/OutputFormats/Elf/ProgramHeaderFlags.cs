namespace Trilang.OutputFormats.Elf;

[Flags]
internal enum ProgramHeaderFlags : uint
{
    Execute = 0x00000001,
    Write = 0x00000002,
    Read = 0x00000004,
}
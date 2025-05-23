namespace Trilang.OutputFormats.Elf;

internal enum ElfInstructionSet : ushort
{
    None = 0x00_00,
    X86_64 = 0x00_3E,
    Arm64 = 0x00_B7,
}
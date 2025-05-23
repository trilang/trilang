namespace Trilang.OutputFormats.Elf;

internal enum ElfFileType : ushort
{
    None = 0x00_00,
    RelocationFile = 0x00_01,
    Executable = 0x00_02,
    SharedObject = 0x00_03,
    CoreFile = 0x00_04
}
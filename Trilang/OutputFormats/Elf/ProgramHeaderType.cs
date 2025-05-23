namespace Trilang.OutputFormats.Elf;

internal enum ProgramHeaderType : uint
{
    Null = 0x00000000,
    Load = 0x00000001,
    Dynamic = 0x00000002,
    Interpreter = 0x00000003,
    Note = 0x00000004,
    ProgramHeader = 0x00000006,
    ThreadLocalStorage = 0x00000007
}
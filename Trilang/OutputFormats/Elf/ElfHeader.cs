namespace Trilang.OutputFormats.Elf;

internal class ElfHeader : ISegment
{
    public const ushort ElfHeaderSize = 0x40;
    public const ushort PhSize = 0x38;
    public const ushort ShSize = 0x40;

    public ElfHeader(
        string segmentName,
        ElfEndianness endianness,
        ElfInstructionSet instructionSet,
        ulong entryPoint,
        ulong programHeaderOffset,
        ulong sectionHeaderOffset,
        ushort programHeaderCount,
        ushort sectionHeaderCount,
        ushort sectionNameIndex)
    {
        SegmentName = segmentName;
        FileOffset = 0;
        FileSize = ElfHeaderSize;

        MagicNumber = [0x7F, 0x45, 0x4C, 0x46];
        Class = 0x02;
        Endianness = endianness;
        IdentVersion = 0x01;
        OsAbi = 0x00;
        AbiVersion = 0x00;
        FileType = ElfFileType.Executable;
        InstructionSet = instructionSet;
        Version = 0x01;
        EntryPoint = entryPoint;
        ProgramHeaderOffset = programHeaderOffset;
        SectionHeaderOffset = sectionHeaderOffset;
        Flags = 0x00;
        ElfSize = ElfHeaderSize;
        ProgramHeaderSize = PhSize;
        ProgramHeaderCount = programHeaderCount;
        SectionHeaderSize = ShSize;
        SectionHeaderCount = sectionHeaderCount;
        SectionNameIndex = sectionNameIndex;
    }

    public void WriteTo(BinaryStream stream)
    {
        stream.Write(MagicNumber);
        stream.Write(Class);
        stream.Write((byte)Endianness);
        stream.Write(IdentVersion);
        stream.Write(OsAbi);
        stream.Write(AbiVersion);
        stream.Write(new byte[7]);
        stream.Write((ushort)FileType);
        stream.Write((ushort)InstructionSet);
        stream.Write(Version);
        stream.Write(EntryPoint);
        stream.Write(ProgramHeaderOffset);
        stream.Write(SectionHeaderOffset);
        stream.Write(Flags);
        stream.Write(ElfSize);
        stream.Write(ProgramHeaderSize);
        stream.Write(ProgramHeaderCount);
        stream.Write(SectionHeaderSize);
        stream.Write(SectionHeaderCount);
        stream.Write(SectionNameIndex);
    }

    public string SegmentName { get; }

    public long FileOffset { get; set; }

    public long FileSize { get; }

    public byte[] MagicNumber { get; }

    public byte Class { get; }

    public ElfEndianness Endianness { get; }

    public byte IdentVersion { get; }

    public byte OsAbi { get; }

    public byte AbiVersion { get; }

    public ElfFileType FileType { get; }

    public ElfInstructionSet InstructionSet { get; }

    public uint Version { get; }

    public ulong EntryPoint { get; }

    public ulong ProgramHeaderOffset { get; }

    public ulong SectionHeaderOffset { get; }

    public uint Flags { get; }

    public ushort ElfSize { get; }

    public ushort ProgramHeaderSize { get; }

    public ushort ProgramHeaderCount { get; }

    public ushort SectionHeaderSize { get; }

    public ushort SectionHeaderCount { get; }

    public ushort SectionNameIndex { get; }
}
namespace Trilang.OutputFormats.Elf;

internal class SectionHeader : ISegment
{
    public SectionHeader(
        string segmentName,
        uint nameOffset,
        SectionHeaderType type,
        SectionHeaderFlags flags,
        ulong address,
        ulong offset,
        ulong size,
        uint link,
        uint info,
        ulong addressAlignment,
        ulong entrySize)
    {
        SegmentName = segmentName;
        FileOffset = 0;
        FileSize = ElfHeader.ShSize;

        NameOffset = nameOffset;
        Type = type;
        Flags = flags;
        Address = address;
        Offset = offset;
        Size = size;
        Link = link;
        Info = info;
        AddressAlignment = addressAlignment;
        EntrySize = entrySize;
    }

    public static SectionHeader CreateNullSection(string segmentName)
        => new SectionHeader(
            segmentName: segmentName,
            nameOffset: 0,
            type: SectionHeaderType.Null,
            flags: SectionHeaderFlags.None,
            address: 0,
            offset: 0,
            size: 0,
            link: 0,
            info: 0,
            addressAlignment: 0,
            entrySize: 0);

    public static SectionHeader CreateProgBits(
        string segmentName,
        uint nameOffset,
        ulong address,
        ulong offset,
        ulong size,
        ulong alignment)
        => new SectionHeader(
            segmentName: segmentName,
            nameOffset: nameOffset,
            type: SectionHeaderType.ProgramData,
            flags: SectionHeaderFlags.Alloc | SectionHeaderFlags.Executable,
            address: address,
            offset: offset,
            size: size,
            link: 0,
            info: 0,
            addressAlignment: alignment,
            entrySize: 0);

    public static SectionHeader CreateShStrTab(
        string segmentName,
        uint nameOffset,
        ulong offset,
        ulong size,
        ulong alignment)
        => new SectionHeader(
            segmentName,
            nameOffset,
            SectionHeaderType.StringTable,
            SectionHeaderFlags.None,
            0,
            offset,
            size,
            0,
            0,
            alignment,
            0);

    public void WriteTo(BinaryStream stream)
    {
        stream.Write(NameOffset);
        stream.Write((uint)Type);
        stream.Write((ulong)Flags);
        stream.Write(Address);
        stream.Write(Offset);
        stream.Write(Size);
        stream.Write(Link);
        stream.Write(Info);
        stream.Write(AddressAlignment);
        stream.Write(EntrySize);
    }

    public string SegmentName { get; }

    public long FileOffset { get; set; }

    public long FileSize { get; }

    public uint NameOffset { get; }

    public SectionHeaderType Type { get; }

    public SectionHeaderFlags Flags { get; }

    public ulong Address { get; }

    public ulong Offset { get; }

    public ulong Size { get; }

    public uint Link { get; }

    public uint Info { get; }

    public ulong AddressAlignment { get; }

    public ulong EntrySize { get; }
}
namespace Trilang.OutputFormats.Elf;

internal class ProgramHeader : ISegment
{
    private ProgramHeader(
        string segmentName,
        ProgramHeaderType type,
        ProgramHeaderFlags flags,
        ulong offset,
        ulong virtualAddress,
        ulong physicalAddress,
        ulong phFileSize,
        ulong memorySize,
        ulong alignment)
    {
        // TODO: support other page sizes
        const ulong pageSize = 0x1000;
        if (virtualAddress % pageSize != offset % pageSize)
            throw new ArgumentException("Virtual address must be aligned to page size");

        if (alignment != 0 && virtualAddress % alignment != offset % alignment)
            throw new ArgumentException("Virtual address must be aligned to alignment");

        SegmentName = segmentName;
        FileOffset = 0;
        FileSize = ElfHeader.PhSize;

        Type = type;
        Flags = flags;
        Offset = offset;
        VirtualAddress = virtualAddress;
        PhysicalAddress = physicalAddress;
        PhFileSize = phFileSize;
        MemorySize = memorySize;
        Alignment = alignment;
    }

    public static ProgramHeader CreateProgramHeader(
        string segmentName,
        ulong offset,
        ulong address,
        ulong phFileSize,
        ulong alignment)
        => new ProgramHeader(
            segmentName: segmentName,
            type: ProgramHeaderType.ProgramHeader,
            flags: ProgramHeaderFlags.Read,
            offset: offset,
            virtualAddress: address,
            physicalAddress: address,
            phFileSize: phFileSize,
            memorySize: phFileSize,
            alignment: alignment);

    public static ProgramHeader CreateProgramHeaderCode(
        string segmentName,
        ulong offset,
        ulong address,
        ulong phFileSize,
        ulong alignment)
        => new ProgramHeader(
            segmentName: segmentName,
            type: ProgramHeaderType.Load,
            flags: ProgramHeaderFlags.Read | ProgramHeaderFlags.Execute,
            offset: offset,
            virtualAddress: address,
            physicalAddress: address,
            phFileSize: phFileSize,
            memorySize: phFileSize,
            alignment: alignment);

    public static ProgramHeader CreateProgramHeaderData(
        string segmentName,
        ulong offset,
        ulong address,
        ulong phFileSize,
        ulong alignment)
        => new ProgramHeader(
            segmentName: segmentName,
            type: ProgramHeaderType.Load,
            flags: ProgramHeaderFlags.Read | ProgramHeaderFlags.Write,
            offset: offset,
            virtualAddress: address,
            physicalAddress: address,
            phFileSize: phFileSize,
            memorySize: phFileSize,
            alignment: alignment);

    public void WriteTo(BinaryStream stream)
    {
        stream.Write((uint)Type);
        stream.Write((uint)Flags);
        stream.Write(Offset);
        stream.Write(VirtualAddress);
        stream.Write(PhysicalAddress);
        stream.Write(PhFileSize);
        stream.Write(MemorySize);
        stream.Write(Alignment);
    }

    public string SegmentName { get; }

    public long FileOffset { get; set; }

    public long FileSize { get; }

    public ProgramHeaderType Type { get; }

    public ProgramHeaderFlags Flags { get; }

    public ulong Offset { get; }

    public ulong VirtualAddress { get; }

    public ulong PhysicalAddress { get; }

    public ulong PhFileSize { get; }

    public ulong MemorySize { get; }

    public ulong Alignment { get; }
}
namespace Trilang.OutputFormats;

internal class NullSegment : ISegment
{
    public NullSegment(string name, long fileSize) : this(name, 0, fileSize)
    {
    }

    public NullSegment(string name, long fileOffset, long fileSize)
    {
        FileOffset = fileOffset;
        FileSize = fileSize;
        SegmentName = name;
    }

    public void WriteTo(BinaryStream stream)
        => stream.Write(new byte[FileSize]);

    public string SegmentName { get; }

    public long FileOffset { get; set; }

    public long FileSize { get; }
}
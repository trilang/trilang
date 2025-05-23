namespace Trilang.OutputFormats;

internal class DataSegment : ISegment
{
    public DataSegment(string name, byte[] data) : this(name, 0, data)
    {
    }

    public DataSegment(string name, long fileOffset, byte[] data)
    {
        FileOffset = fileOffset;
        FileSize = data.Length;
        Data = data;
        SegmentName = name;
    }

    public void WriteTo(BinaryStream stream)
        => stream.Write(Data);

    public string SegmentName { get; }

    public long FileOffset { get; set; }

    public long FileSize { get; }

    public byte[] Data { get; }
}
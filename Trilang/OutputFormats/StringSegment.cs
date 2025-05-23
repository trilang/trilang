using System.Text;

namespace Trilang.OutputFormats;

internal class StringSegment : ISegment
{
    private readonly List<string> strings;

    public StringSegment(string segmentName) : this(segmentName, 0)
    {
    }

    public StringSegment(string segmentName, long fileOffset)
    {
        strings = [];

        SegmentName = segmentName;
        FileOffset = fileOffset;
        FileSize = 1;
    }

    public long Add(string s)
    {
        strings.Add(s);

        var offset = FileSize;
        FileSize += Encoding.ASCII.GetByteCount(s) + 1;

        return offset;
    }

    public void WriteTo(BinaryStream stream)
    {
        stream.Write(0x00);

        foreach (var s in strings)
        {
            stream.Write(Encoding.UTF8.GetBytes(s));
            stream.Write(0x00);
        }
    }

    public string SegmentName { get; }

    public long FileOffset { get; set; }

    public long FileSize { get; private set; }
}
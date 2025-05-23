namespace Trilang.OutputFormats;

internal class OutputFile
{
    private readonly List<ISegment> segments;

    public OutputFile()
        => segments = [];

    public void Add(ISegment segment)
    {
        var lastSegment = segments.LastOrDefault();
        var lastSegmentOffset = lastSegment?.FileOffset ?? 0;
        var lastSegmentSize = lastSegment?.FileSize ?? 0;
        var segmentOffset = lastSegmentOffset + lastSegmentSize;
        segment.FileOffset = segmentOffset;

        segments.Add(segment);
    }

    public void Set(string name, ISegment segment)
    {
        var index = segments.FindIndex(x => x.SegmentName == name);
        if (index < 0)
            throw new InvalidOperationException($"Segment {name} does not exist");

        var existing = segments[index];
        if (segment.FileSize != existing.FileSize)
            throw new InvalidOperationException("Cannot set segment with different offset or size");

        segment.FileOffset = existing.FileOffset;

        segments[index] = segment;
    }

    public void WriteTo(BinaryStream stream)
    {
        foreach (var segment in Segments)
        {
            stream.Seek(segment.FileOffset, SeekOrigin.Begin);

            segment.WriteTo(stream);
        }
    }

    public IReadOnlyList<ISegment> Segments
        => segments;
}
namespace Trilang.OutputFormats;

internal interface ISegment
{
    string SegmentName { get; }
    long FileOffset { get; set; }
    long FileSize { get; }

    void WriteTo(BinaryStream stream);
}
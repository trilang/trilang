namespace Trilang.OutputFormats;

internal class BinaryStream : Stream
{
    private readonly Stream stream;
    private readonly bool isLittleEndian;

    public BinaryStream(Stream stream, bool isLittleEndian)
    {
        this.stream = stream;
        this.isLittleEndian = isLittleEndian;
    }

    protected override void Dispose(bool disposing)
    {
        try
        {
            if (disposing)
            {
                try
                {
                    Flush();
                }
                finally
                {
                    stream.Dispose();
                }
            }
        }
        finally
        {
            base.Dispose(disposing);
        }
    }

    public override void Flush()
        => stream.Flush();

    public override int Read(byte[] buffer, int offset, int count)
        => stream.Read(buffer, offset, count);

    public override long Seek(long offset, SeekOrigin origin)
        => stream.Seek(offset, origin);

    public override void SetLength(long value)
        => stream.SetLength(value);

    public override void Write(byte[] buffer, int offset, int count)
        => stream.Write(buffer, offset, count);

    public void Write(byte value)
        => WriteByte(value);

    public void Write(ushort value)
    {
        if (isLittleEndian)
            Write([
                (byte)(value & 0xFF),
                (byte)((value >> 8) & 0xFF)
            ]);
        else
            Write([
                (byte)((value >> 8) & 0xFF),
                (byte)(value & 0xFF)
            ]);
    }

    public void Write(uint value)
    {
        if (isLittleEndian)
            Write([
                (byte)(value & 0xFF),
                (byte)((value >> 8) & 0xFF),
                (byte)((value >> 16) & 0xFF),
                (byte)((value >> 24) & 0xFF)
            ]);
        else
            Write([
                (byte)((value >> 24) & 0xFF),
                (byte)((value >> 16) & 0xFF),
                (byte)((value >> 8) & 0xFF),
                (byte)(value & 0xFF)
            ]);
    }

    public void Write(ulong value)
    {
        if (isLittleEndian)
            Write([
                (byte)(value & 0xFF),
                (byte)((value >> 8) & 0xFF),
                (byte)((value >> 16) & 0xFF),
                (byte)((value >> 24) & 0xFF),
                (byte)((value >> 32) & 0xFF),
                (byte)((value >> 40) & 0xFF),
                (byte)((value >> 48) & 0xFF),
                (byte)((value >> 56) & 0xFF)
            ]);
        else
            Write([
                (byte)((value >> 56) & 0xFF),
                (byte)((value >> 48) & 0xFF),
                (byte)((value >> 40) & 0xFF),
                (byte)((value >> 32) & 0xFF),
                (byte)((value >> 24) & 0xFF),
                (byte)((value >> 16) & 0xFF),
                (byte)((value >> 8) & 0xFF),
                (byte)(value & 0xFF)
            ]);
    }

    public override bool CanRead
        => stream.CanRead;

    public override bool CanSeek
        => stream.CanSeek;

    public override bool CanWrite
        => stream.CanWrite;

    public override long Length
        => stream.Length;

    public override long Position
    {
        get => stream.Position;
        set => stream.Position = value;
    }
}
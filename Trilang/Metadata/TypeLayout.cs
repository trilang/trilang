namespace Trilang.Metadata;

public class TypeLayout
{
    private readonly List<FieldLayout> fields;

    public TypeLayout() : this([])
    {
    }

    internal TypeLayout(IEnumerable<FieldLayout> fields)
    {
        this.fields = [];
        foreach (var field in fields)
            AddField(field);
    }

    private void AddField(FieldLayout field)
    {
        fields.Add(field);
        Size += field.Size;
    }

    public void AddField(FieldMetadata field, int size)
    {
        var offset = fields.Count > 0 ? fields[^1].Offset + fields[^1].Size : 0;
        AddField(new FieldLayout(field, offset, size));
    }

    public void AddGap(int size)
    {
        var offset = fields.Count > 0 ? fields[^1].Offset + fields[^1].Size : 0;
        AddField(new FieldLayout(null, offset, size));
    }

    public int Size { get; private set; }

    public IReadOnlyList<FieldLayout> Fields
        => fields;
}
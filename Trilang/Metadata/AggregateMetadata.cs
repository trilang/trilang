namespace Trilang.Metadata;

public class AggregateMetadata : IMetadata
{
    private readonly List<IMetadata> members;

    public AggregateMetadata(IEnumerable<IMetadata> members)
        => this.members = [..members];

    public IMetadata this[int index]
        => members[index];

    public AggregateMetadata Combine(AggregateMetadata other)
    {
        if (IsEmpty)
            return other;

        if (other.IsEmpty)
            return this;

        var combined = new List<IMetadata>(members);
        combined.AddRange(other.members);

        return new AggregateMetadata(combined);
    }

    public IEnumerable<IMetadata> MatchFunction(IEnumerable<ITypeMetadata> actualParameters)
        => members
            .OfType<IFunctionMetadata>()
            .Where(x => x.Type.ParameterTypes.SequenceEqual(actualParameters));

    public SourceLocation? Definition
        => null;

    public bool IsInvalid
        => false;

    public IReadOnlyList<IMetadata> Members
        => members;

    public bool IsEmpty
        => members.Count == 0;
}
namespace Trilang.Metadata;

public class FunctionGroupMetadata : ITypeMetadata
{
    private readonly List<IFunctionMetadata> functions;

    public FunctionGroupMetadata()
        => functions = [];

    public IMetadata? GetMember(string name)
        => null;

    public void AddFunction(IFunctionMetadata function)
        => functions.Add(function);

    public IEnumerable<IMetadata> Match(IEnumerable<ITypeMetadata> actualParameters)
        => functions.Where(x => x.Type.ParameterTypes.SequenceEqual(actualParameters));

    public SourceLocation? Definition => null;

    public bool IsInvalid => false;

    public bool IsValueType => true;

    public TypeLayout? Layout { get; set; }

    public IReadOnlyList<IFunctionMetadata> Functions => functions;
}
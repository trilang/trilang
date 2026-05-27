using System.Diagnostics.CodeAnalysis;

namespace Trilang.Metadata.Aggregate;

public class AggregateMetadata : IMetadata
{
    public static readonly AggregateMetadata Empty = new AggregateMetadata([]);

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

    public IEnumerable<IFunctionMetadata> MatchFunction(IEnumerable<ITypeMetadata> actualParameters)
        => members
            .OfType<IFunctionMetadata>()
            .Where(x => x.Type.ParameterTypes.SequenceEqual(actualParameters));

    public IResolutionResult<IMetadata> ResolveSingle()
        => members switch
        {
            [] => NoMatch<IMetadata>.ForMember(),
            [var single] => new SuccessfulMatch<IMetadata>(single),
            _ => MultipleMatches<IMetadata>.ForMember(members),
        };

    private static IMetadata GetType(IMetadata metadata)
        => metadata switch
        {
            ITypeMetadata typeMetadata => typeMetadata,
            ITypedMetadata typedMetadata => typedMetadata.Type,

            _ => throw new ArgumentOutOfRangeException(nameof(metadata)),
        };

    private static bool TryGetCallable(IMetadata metadata, [MaybeNullWhen(false)] out FunctionTypeMetadata callable)
    {
        var type = metadata switch
        {
            FieldMetadata fieldMetadata => fieldMetadata.Type,
            InterfacePropertyMetadata interfacePropertyMetadata => interfacePropertyMetadata.Type,
            ParameterMetadata parameterMetadata => parameterMetadata.Type,
            PropertyMetadata propertyMetadata => propertyMetadata.Type,
            VariableMetadata variableMetadata => variableMetadata.Type,
            _ => null,
        };

        if (type is FunctionTypeMetadata functionTypeMetadata)
        {
            callable = functionTypeMetadata;
            return true;
        }

        callable = null;
        return false;
    }

    public IResolutionResult<IMetadata> ResolveFunction(IReadOnlyList<ITypeMetadata> actualParameters)
        => members switch
        {
            [INamedMetadata named]
                => named.GetMembers(ConstructorMetadata.Name).members switch
                {
                    [] => NoMatch<IFunctionMetadata>.ForFunction(),
                    [ConstructorMetadata ctor] => ResolveSingleFunction(ctor, actualParameters),
                    [.. var ctors] => ResolveMultipleFunction(ctors, actualParameters),
                },

            [IFunctionMetadata function]
                => ResolveSingleFunction(function, actualParameters),

            [var single] when TryGetCallable(single, out var callable)
                => ResolveSingleFunctionType(single, callable, actualParameters),

            [var single]
                => new NotFunction(GetType(single)),

            _ => ResolveMultipleFunction(members, actualParameters),
        };

    private static IResolutionResult<IFunctionMetadata> ResolveSingleFunction(
        IFunctionMetadata function,
        IReadOnlyList<ITypeMetadata> actualParameters)
    {
        var expectedCount = function.Type.ParameterTypes.Count;
        var actualCount = actualParameters.Count;

        if (actualCount > expectedCount)
            return ExtraArgument<IFunctionMetadata>.ForFunction(expectedCount, actualCount);

        if (actualCount < expectedCount)
            return MissingArgument<IFunctionMetadata>.ForFunction(function, expectedCount, actualCount);

        var mismatches = new List<ArgumentMismatchDetail>();
        for (var i = 0; i < Math.Min(expectedCount, actualCount); i++)
        {
            var actual = actualParameters[i];
            var expected = function.Type.ParameterTypes[i];
            if (actual.IsInvalid || expected.IsInvalid)
                continue;

            if (!expected.Equals(actual))
                mismatches.Add(new ArgumentMismatchDetail(i, expected, actual));
        }

        if (mismatches.Count > 0)
            return ArgumentMismatch<IFunctionMetadata>.ForFunction(mismatches);

        return new SuccessfulMatch<IFunctionMetadata>(function);
    }

    private static IResolutionResult<IMetadata> ResolveSingleFunctionType(
        IMetadata single,
        FunctionTypeMetadata function,
        IReadOnlyList<ITypeMetadata> actualParameters)
    {
        var expectedCount = function.ParameterTypes.Count;
        var actualCount = actualParameters.Count;

        if (actualCount > expectedCount)
            return ExtraArgument<IMetadata>.ForCallable(expectedCount, actualCount);

        if (actualCount < expectedCount)
            return MissingArgument<IMetadata>.ForCallable(single, expectedCount, actualCount);

        var mismatches = new List<ArgumentMismatchDetail>();
        for (var i = 0; i < Math.Min(expectedCount, actualCount); i++)
        {
            var actual = actualParameters[i];
            var expected = function.ParameterTypes[i];
            if (actual.IsInvalid || expected.IsInvalid)
                continue;

            if (!expected.Equals(actual))
                mismatches.Add(new ArgumentMismatchDetail(i, expected, actual));
        }

        if (mismatches.Count > 0)
            return ArgumentMismatch<IMetadata>.ForCallable(mismatches);

        return new SuccessfulMatch<IMetadata>(single);
    }

    private static IResolutionResult<IFunctionMetadata> ResolveMultipleFunction(
        IReadOnlyList<IMetadata> members,
        IReadOnlyList<ITypeMetadata> actualParameters)
    {
        var functionCandidates = members.OfType<IFunctionMetadata>();
        var ctorCandidates = members
            .OfType<INamedMetadata>()
            .SelectMany(x => x.GetMembers(ConstructorMetadata.Name).Members)
            .Cast<IFunctionMetadata>();

        var candidates = functionCandidates
            .Concat(ctorCandidates)
            .Where(x => x.Type.ParameterTypes.SequenceEqual(actualParameters))
            .ToArray();

        return candidates.Length switch
        {
            0 => NoMatch<IFunctionMetadata>.ForFunction(),
            > 1 => MultipleMatches<IFunctionMetadata>.ForFunction(candidates),
            _ => new SuccessfulMatch<IFunctionMetadata>(candidates[0]),
        };
    }

    public IResolutionResult<IGenericMetadata> ResolveGeneric(int actual)
    {
        if (members is [var candidate])
        {
            if (candidate is not IGenericMetadata type)
                return new NotGeneric(candidate);

            var expected = type.GenericArguments.Count;

            if (actual > expected)
                return ExtraArgument<IGenericMetadata>.ForGeneric(expected, actual);

            if (actual < expected)
                return MissingArgument<IGenericMetadata>.ForGeneric(type, expected, actual);

            return new SuccessfulMatch<IGenericMetadata>(type);
        }

        var candidates = members
            .OfType<IGenericMetadata>()
            .Where(x => x.GenericArguments.Count == actual)
            .ToArray();

        return candidates switch
        {
            [] => NoMatch<IGenericMetadata>.ForGeneric(),
            [var single] => new SuccessfulMatch<IGenericMetadata>(single),
            _ => MultipleMatches<IGenericMetadata>.ForGeneric(candidates),
        };
    }

    public SourceLocation? Definition
        => null;

    public bool IsInvalid
        => false;

    public IReadOnlyList<IMetadata> Members
        => members;

    public bool IsEmpty
        => members.Count == 0;

    public void Freeze()
    {
    }
}
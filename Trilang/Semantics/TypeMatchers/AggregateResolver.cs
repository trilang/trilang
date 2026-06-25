using System.Diagnostics.CodeAnalysis;
using Trilang.Metadata;
using Trilang.Semantics.TypeMatchers.AggregateResults;

namespace Trilang.Semantics.TypeMatchers;

public class AggregateResolver
{
    private readonly TypeMatcher typeMatcher;

    public AggregateResolver(TypeMatcher typeMatcher)
        => this.typeMatcher = typeMatcher;

    public IResolutionResult<IMetadata> ResolveSingle(AggregateMetadata aggregate)
        => aggregate.Members switch
        {
            [] => NoMatch<IMetadata>.ForMember(),
            [var single] => new SuccessfulMatch<IMetadata>(single),
            _ => MultipleMatches<IMetadata>.ForMember(aggregate.Members),
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

    public IResolutionResult<IMetadata> ResolveFunction(
        AggregateMetadata aggregate,
        IReadOnlyList<ITypeMetadata> actualParameters)
        => aggregate.Members switch
        {
            [INamedMetadata named]
                => named.GetMembers(ConstructorMetadata.Name).Members switch
                {
                    [] => NoMatch<IFunctionMetadata>.ForFunction(),
                    [ConstructorMetadata ctor] => ResolveSingleFunction(ctor, actualParameters),
                    var ctors => ResolveMultipleFunction(ctors, actualParameters),
                },

            [IFunctionMetadata function]
                => ResolveSingleFunction(function, actualParameters),

            [var single] when TryGetCallable(single, out var callable)
                => ResolveSingleFunctionType(single, callable, actualParameters),

            [var single]
                => new NotFunction(GetType(single)),

            _ => ResolveMultipleFunction(aggregate.Members, actualParameters),
        };

    private IResolutionResult<IFunctionMetadata> ResolveSingleFunction(
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
        var conversions = new List<ConversionMatchDetails>();
        for (var i = 0; i < Math.Min(expectedCount, actualCount); i++)
        {
            var actual = actualParameters[i];
            var expected = function.Type.ParameterTypes[i];
            var result = typeMatcher.Match(actual, expected);
            if (result is FailedMatch)
                mismatches.Add(new ArgumentMismatchDetail(i, expected, actual));
            else if (result is ImplicitConversion implicitConversion)
                conversions.Add(new ConversionMatchDetails(i, implicitConversion.Target));
        }

        if (mismatches.Count > 0)
            return ArgumentMismatch<IFunctionMetadata>.ForFunction(mismatches);

        if (conversions.Count > 0)
            return new ConversionMatch<IFunctionMetadata>(function, conversions);

        return new SuccessfulMatch<IFunctionMetadata>(function);
    }

    private IResolutionResult<IMetadata> ResolveSingleFunctionType(
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
            var result = typeMatcher.Match(actual, expected);
            if (result is FailedMatch or ImplicitConversion)
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

    public IResolutionResult<IGenericMetadata> ResolveGeneric(AggregateMetadata aggregate, int actual)
    {
        if (aggregate.Members is [var candidate])
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

        var candidates = aggregate.Members
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
}
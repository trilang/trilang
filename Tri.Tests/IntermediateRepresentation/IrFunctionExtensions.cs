using Trilang.IntermediateRepresentation;

namespace Tri.Tests.IntermediateRepresentation;

internal static class IrFunctionExtensions
{
    public static string Dump(this IEnumerable<IrFunction> functions)
        => string.Join("\n", functions);
}
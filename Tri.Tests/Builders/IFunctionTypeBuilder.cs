namespace Tri.Tests.Builders;

public interface IFunctionTypeBuilder
{
    IFunctionTypeBuilder DefineParameter(string type);

    IFunctionTypeBuilder ReturnType(string type);
}
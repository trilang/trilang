using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface IMethodBuilder
{
    IMethodBuilder AccessModifier(AccessModifier modifier);

    IMethodBuilder DefineParameter(string name, string type);

    IMethodBuilder DefineParameter(string name, TypeNode type);

    IMethodBuilder ReturnType(string type);

    IMethodBuilder Body(Action<IBlockBuilder>? action = null);

    IMethodBuilder Static();

    MethodDeclarationNode Build();
}
using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface ISyntaxTreeBuilder
{
    ISyntaxTreeBuilder DefineFunction(string name, Action<IFunctionBuilder> action);

    ISyntaxTreeBuilder DefineType(string name, Action<ITypeBuilder>? action = null);

    ISyntaxTreeBuilder DefineAliasType(string name, TypeNode aliasType);

    ISyntaxTreeBuilder DefineAliasType(string name, Action<ITypeAliasBuilder> action);

    SyntaxTree Build();
}
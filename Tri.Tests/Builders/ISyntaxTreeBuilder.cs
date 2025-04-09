using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface ISyntaxTreeBuilder
{
    ISyntaxTreeBuilder DefineFunction(string name, Action<IFunctionBuilder> action);

    ISyntaxTreeBuilder DefineType(string name, Action<ITypeBuilder> action);

    ISyntaxTreeBuilder DefineAliasType(string name, string aliasType);

    SyntaxTree Build();
}
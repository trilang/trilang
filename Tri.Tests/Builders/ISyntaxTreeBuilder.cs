using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface ISyntaxTreeBuilder
{
    ISyntaxTreeBuilder DefineFunction(string name, Action<IFunctionBuilder> action);

    SyntaxTree Build();
}
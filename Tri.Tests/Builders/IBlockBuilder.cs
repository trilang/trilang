using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface IBlockBuilder
{
    IBlockBuilder DefineVariable(string name, string type, Action<IExpressionBuilder> action);

    IBlockBuilder Return(Action<IExpressionBuilder> action);

    IBlockBuilder Statement(Action<IExpressionBuilder> action);

    IBlockBuilder If(
        Action<IExpressionBuilder> condition,
        Action<IBlockBuilder> then,
        Action<IBlockBuilder>? @else = null);

    IBlockBuilder While(
        Action<IExpressionBuilder> condition,
        Action<IBlockBuilder> body);

    BlockStatementNode Build();
}
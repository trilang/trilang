using Trilang.Parsing.Ast;

namespace Tri.Tests.Builders;

public interface IExpressionBuilder
{
    IExpressionBuilder Number(int number);
    IExpressionBuilder True();
    IExpressionBuilder False();
    IExpressionBuilder Char(char c);
    IExpressionBuilder String(string str);

    IExpressionBuilder Variable(string name);

    IExpressionBuilder UnaryPlus();
    IExpressionBuilder UnaryMinus();
    IExpressionBuilder LogicalNot();

    IExpressionBuilder Add();
    IExpressionBuilder Sub();
    IExpressionBuilder Mul();
    IExpressionBuilder Div();

    IExpressionBuilder Call(string name);

    IExpressionNode Build();
}
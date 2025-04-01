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

    IExpressionBuilder Unary(UnaryExpressionKind kind);
    IExpressionBuilder UnaryPlus();
    IExpressionBuilder UnaryMinus();
    IExpressionBuilder LogicalNot();

    IExpressionBuilder BinaryExpression(BinaryExpressionKind kind);
    IExpressionBuilder Add();
    IExpressionBuilder Sub();
    IExpressionBuilder Mul();
    IExpressionBuilder Div();

    IExpressionBuilder Assign();
    IExpressionBuilder AddAssign();
    IExpressionBuilder SubAssign();
    IExpressionBuilder MulAssign();
    IExpressionBuilder DivAssign();
    IExpressionBuilder ModAssign();
    IExpressionBuilder AndAssign();
    IExpressionBuilder OrAssign();
    IExpressionBuilder XorAssign();

    IExpressionBuilder Call(string name);

    IExpressionNode Build();
}
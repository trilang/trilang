using Trilang.Parsing.Ast;

namespace Trilang.Parsing.Formatters;

public partial class CommonFormatter : IFormatter
{
    private readonly Writer writer;

    public CommonFormatter()
        => writer = new Writer();

    public void Visit(BinaryExpressionNode node)
    {
        node.Left.Accept(this);
        writer.Write(' ');

        writer.Write(node.Kind switch
        {
            BinaryExpressionKind.Addition => "+",
            BinaryExpressionKind.Subtraction => "-",
            BinaryExpressionKind.Multiplication => "*",
            BinaryExpressionKind.Division => "/",

            BinaryExpressionKind.BitwiseAnd => "&",
            BinaryExpressionKind.BitwiseOr => "|",
            BinaryExpressionKind.BitwiseXor => "^",

            BinaryExpressionKind.ConditionalAnd => "&&",
            BinaryExpressionKind.ConditionalOr => "||",
            BinaryExpressionKind.Equality => "==",
            BinaryExpressionKind.Inequality => "!=",
            BinaryExpressionKind.LessThan => "<",
            BinaryExpressionKind.LessThanOrEqual => "<=",
            BinaryExpressionKind.GreaterThan => ">",
            BinaryExpressionKind.GreaterThanOrEqual => ">=",

            BinaryExpressionKind.Assignment => "=",

            BinaryExpressionKind.AdditionAssignment => "+=",
            BinaryExpressionKind.SubtractionAssignment => "-=",
            BinaryExpressionKind.MultiplicationAssignment => "*=",
            BinaryExpressionKind.DivisionAssignment => "/=",
            BinaryExpressionKind.ModulusAssignment => "%=",

            BinaryExpressionKind.BitwiseAndAssignment => "&=",
            BinaryExpressionKind.BitwiseOrAssignment => "|=",
            BinaryExpressionKind.BitwiseXorAssignment => "^=",

            _ => throw new ArgumentOutOfRangeException(nameof(node.Kind)),
        });

        writer.Write(' ');
        node.Right.Accept(this);
    }

    public void Visit(BlockStatementNode node)
    {
        writer.WriteLine('{');
        writer.Scoped(_ =>
        {
            foreach (var statement in node.Statements)
                statement.Accept(this);
        });
        writer.Write('}');
    }

    public void Visit(BreakNode node)
        => writer.WriteLine("break;");

    public void Visit(CallExpressionNode node)
    {
        writer.Write(node.FunctionName);
        writer.Write('(');

        for (var i = 0; i < node.Parameters.Count; i++)
        {
            var parameter = node.Parameters[i];
            parameter.Accept(this);

            if (i < node.Parameters.Count - 1)
                writer.Write(", ");
        }

        writer.Write(")");
    }

    public void Visit(ContinueNode node)
        => writer.WriteLine("continue;");

    public void Visit(ExpressionStatementNode node)
    {
        node.Expression.Accept(this);
        writer.WriteLine(';');
    }

    public void Visit(FunctionParameterNode node)
    {
        writer.Write(node.Name);
        writer.Write(": ");
        node.Type.Accept(this);
    }

    public void Visit(FunctionDeclarationNode node)
    {
        if (node.IsExternal)
            writer.Write("external ");

        writer.Write("function ");
        writer.Write(node.Name);
        writer.Write('(');

        for (var i = 0; i < node.Parameters.Count; i++)
        {
            node.Parameters[i].Accept(this);

            if (i < node.Parameters.Count - 1)
                writer.Write(", ");
        }

        writer.Write("): ");
        node.ReturnType.Accept(this);
        writer.Write(' ');

        node.Body?.Accept(this);
    }

    public void Visit(IfStatementNode node)
    {
        writer.Write("if (");
        node.Condition.Accept(this);
        writer.Write(") ");
        node.Then.Accept(this);

        if (node.Else is not null)
        {
            writer.Write(" else ");
            node.Else.Accept(this);
        }

        writer.WriteLine();
    }

    public void Visit(LiteralExpressionNode node)
    {
        switch (node.Kind)
        {
            case LiteralExpressionKind.Number:
                writer.Write(node.Value.ToString()!);
                break;
            case LiteralExpressionKind.Boolean:
                writer.Write(node.Value.ToString()!.ToLowerInvariant());
                break;
            case LiteralExpressionKind.String:
                writer.Write('"');
                writer.Write(node.Value.ToString()!);
                writer.Write('"');
                break;
            case LiteralExpressionKind.Char:
                writer.Write('\'');
                writer.Write(node.Value.ToString()!);
                writer.Write('\'');
                break;
            default:
                throw new ArgumentOutOfRangeException(nameof(node.Kind));
        }
    }

    public void Visit(ReturnStatementNode node)
    {
        writer.Write("return ");
        node.Expression.Accept(this);
        writer.WriteLine(';');
    }

    public void Visit(SyntaxTree node)
    {
        for (var i = 0; i < node.Functions.Count; i++)
        {
            node.Functions[i].Accept(this);

            if (i < node.Functions.Count - 1)
            {
                writer.WriteLine();
                writer.WriteLine();
            }
        }
    }

    public void Visit(TypeNode node)
    {
        writer.Write(node.Name);

        if (node.IsArray)
            writer.Write("[]");
    }

    public void Visit(UnaryExpressionNode node)
    {
        writer.Write(node.Kind switch
        {
            UnaryExpressionKind.UnaryPlus => '+',
            UnaryExpressionKind.UnaryMinus => '-',
            UnaryExpressionKind.LogicalNot => '!',
            UnaryExpressionKind.BitwiseNot => '~',

            _ => throw new ArgumentOutOfRangeException(nameof(node.Kind)),
        });

        node.Operand.Accept(this);
    }

    public void Visit(VariableExpressionNode node)
        => writer.Write(node.Name);

    public void Visit(VariableDeclarationStatementNode node)
    {
        writer.Write("var ");
        writer.Write(node.Name);
        writer.Write(": ");
        node.Type.Accept(this);
        writer.Write(" = ");
        node.Expression.Accept(this);
        writer.WriteLine(';');
    }

    public void Visit(WhileNode node)
    {
        writer.Write("while (");
        node.Condition.Accept(this);
        writer.Write(") ");
        node.Body.Accept(this);
        writer.WriteLine();
    }

    public override string ToString()
        => writer.ToString();
}
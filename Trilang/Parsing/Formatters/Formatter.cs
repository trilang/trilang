using Trilang.Parsing.Ast;

namespace Trilang.Parsing.Formatters;

public partial class Formatter : IFormatter
{
    private readonly Writer writer;

    public Formatter()
        => writer = new Writer();

    private void WriteAccessModifier(AccessModifier accessModifier)
    {
        writer.Write(accessModifier switch
        {
            AccessModifier.Public => "public",
            AccessModifier.Private => "private",

            _ => throw new ArgumentOutOfRangeException(nameof(accessModifier)),
        });
    }

    private void WriteGenericArguments(IHasGenericArguments node)
    {
        if (node.GenericArguments.Count <= 0)
            return;

        writer.Write('<');

        for (var i = 0; i < node.GenericArguments.Count; i++)
        {
            node.GenericArguments[i].Accept(this);

            if (i < node.GenericArguments.Count - 1)
                writer.Write(", ");
        }

        writer.Write('>');
    }

    public void Visit(ArrayAccessExpressionNode node)
    {
        node.Member.Accept(this);
        writer.Write('[');
        node.Index.Accept(this);
        writer.Write(']');
    }

    public void Visit(ArrayTypeNode node)
    {
        node.ElementType.Accept(this);
        writer.Write("[]");
    }

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
        writer.Scoped(() =>
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
        node.Member.Accept(this);
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

    public void Visit(ConstructorDeclarationNode node)
    {
        WriteAccessModifier(node.AccessModifier);
        writer.Write(" constructor(");

        for (var i = 0; i < node.Parameters.Count; i++)
        {
            node.Parameters[i].Accept(this);

            if (i < node.Parameters.Count - 1)
                writer.Write(", ");
        }

        writer.Write(") ");

        node.Body.Accept(this);
    }

    public void Visit(ContinueNode node)
        => writer.WriteLine("continue;");

    public void Visit(DiscriminatedUnionNode node)
    {
        for (var i = 0; i < node.Types.Count; i++)
        {
            node.Types[i].Accept(this);

            if (i < node.Types.Count - 1)
                writer.Write(" | ");
        }
    }

    public void Visit(ExpressionStatementNode node)
    {
        node.Expression.Accept(this);
        writer.WriteLine(';');
    }

    public void Visit(PropertyDeclarationNode node)
    {
        writer.Write(node.Name);
        writer.Write(": ");
        node.Type.Accept(this);

        var hasGetter = node.Getter is not null;
        var hasSetter = node.Setter is not null;
        if (hasGetter || hasSetter)
        {
            writer.WriteLine(" {");

            writer.Scoped(() =>
            {
                node.Getter?.Accept(this);

                if (hasGetter && hasSetter)
                    writer.WriteLine();

                node.Setter?.Accept(this);

                writer.WriteLine();
            });

            writer.Write('}');
        }
        else
        {
            writer.Write(';');
        }
    }

    public void Visit(PropertyGetterNode node)
    {
        WriteAccessModifier(node.AccessModifier);
        writer.Write(" get");

        if (node.Body is null)
        {
            writer.Write(';');
        }
        else
        {
            writer.Write(' ');
            node.Body.Accept(this);
        }
    }

    public void Visit(PropertySetterNode node)
    {
        WriteAccessModifier(node.AccessModifier);
        writer.Write(" set");

        if (node.Body is null)
        {
            writer.Write(';');
        }
        else
        {
            writer.Write(' ');
            node.Body.Accept(this);
        }
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

    public void Visit(FunctionTypeNode node)
    {
        writer.Write('(');

        for (var i = 0; i < node.ParameterTypes.Count; i++)
        {
            node.ParameterTypes[i].Accept(this);

            if (i < node.ParameterTypes.Count - 1)
                writer.Write(", ");
        }

        writer.Write(") => ");
        node.ReturnType.Accept(this);
    }

    public void Visit(GenericTypeNode node)
    {
        writer.Write(node.PrefixName);
        writer.Write('<');

        for (var i = 0; i < node.TypeArguments.Count; i++)
        {
            node.TypeArguments[i].Accept(this);

            if (i < node.TypeArguments.Count - 1)
                writer.Write(", ");
        }

        writer.Write('>');
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

    public void Visit(InterfaceNode node)
    {
        if (node.Parent is TypeAliasDeclarationNode)
        {
            writer.WriteLine("{");

            writer.Scoped(() =>
            {
                foreach (var property in node.Properties)
                {
                    property.Accept(this);
                    writer.WriteLine();
                }

                if (node.Methods.Count > 0)
                    writer.WriteLine();

                foreach (var method in node.Methods)
                {
                    method.Accept(this);
                    writer.WriteLine();
                }
            });

            writer.Write("}");
        }
        else
        {
            writer.Write("{ ");

            foreach (var property in node.Properties)
            {
                property.Accept(this);
                writer.Write(" ");
            }

            foreach (var method in node.Methods)
            {
                method.Accept(this);
                writer.Write(" ");
            }

            writer.Write("}");
        }
    }

    public void Visit(InterfacePropertyNode node)
    {
        writer.Write(node.Name);
        writer.Write(": ");
        node.Type.Accept(this);
        writer.Write(";");
    }

    public void Visit(InterfaceMethodNode node)
    {
        writer.Write(node.Name);
        writer.Write("(");

        for (var i = 0; i < node.Parameters.Count; i++)
        {
            node.Parameters[i].Accept(this);

            if (i < node.Parameters.Count - 1)
                writer.Write(", ");
        }

        writer.Write("): ");
        node.ReturnType.Accept(this);
        writer.Write(";");
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

    public void Visit(MemberAccessExpressionNode node)
    {
        if (node.Member is not null)
        {
            node.Member.Accept(this);
            writer.Write('.');
        }

        writer.Write(node.Name);
    }

    public void Visit(MethodDeclarationNode node)
    {
        WriteAccessModifier(node.AccessModifier);
        writer.Write(' ');
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

        node.Body.Accept(this);
    }

    public void Visit(NewArrayExpressionNode node)
    {
        writer.Write("new ");
        node.Type.ElementType.Accept(this);
        writer.Write('[');
        node.Size.Accept(this);
        writer.Write(']');
    }

    public void Visit(NewObjectExpressionNode node)
    {
        writer.Write("new ");
        node.Type.Accept(this);
        writer.Write('(');

        for (var i = 0; i < node.Parameters.Count; i++)
        {
            node.Parameters[i].Accept(this);

            if (i < node.Parameters.Count - 1)
                writer.Write(", ");
        }

        writer.Write(')');
    }

    public void Visit(NullExpressionNode node)
        => writer.Write("null");

    public void Visit(ParameterNode node)
    {
        writer.Write(node.Name);
        writer.Write(": ");
        node.Type.Accept(this);
    }

    public void Visit(ReturnStatementNode node)
    {
        if (node.Expression is not null)
        {
            writer.Write("return ");
            node.Expression.Accept(this);
            writer.WriteLine(';');
        }
        else
        {
            writer.WriteLine("return;");
        }
    }

    public void Visit(SyntaxTree node)
    {
        for (var i = 0; i < node.Declarations.Count; i++)
        {
            node.Declarations[i].Accept(this);

            if (i < node.Declarations.Count - 1)
            {
                writer.WriteLine();
                writer.WriteLine();
            }
        }
    }

    public void Visit(TupleExpressionNode node)
    {
        writer.Write('(');

        for (var i = 0; i < node.Expressions.Count; i++)
        {
            node.Expressions[i].Accept(this);

            if (i < node.Expressions.Count - 1)
                writer.Write(", ");
        }

        writer.Write(')');
    }

    public void Visit(TupleTypeNode node)
    {
        writer.Write('(');

        for (var i = 0; i < node.Types.Count; i++)
        {
            node.Types[i].Accept(this);

            if (i < node.Types.Count - 1)
                writer.Write(", ");
        }

        writer.Write(')');
    }

    public void Visit(TypeAliasDeclarationNode node)
    {
        WriteAccessModifier(node.AccessModifier);
        writer.Write(" type ");
        writer.Write(node.Name);
        writer.Write(" = ");
        node.Type.Accept(this);

        if (node.Type is not InterfaceNode)
            writer.Write(';');
    }

    public void Visit(TypeDeclarationNode node)
    {
        WriteAccessModifier(node.AccessModifier);
        writer.Write(" type ");
        writer.Write(node.Name);
        WriteGenericArguments(node);

        if (node.Interfaces.Count > 0)
        {
            writer.Write(" : ");

            for (var i = 0; i < node.Interfaces.Count; i++)
            {
                node.Interfaces[i].Accept(this);

                if (i < node.Interfaces.Count - 1)
                    writer.Write(", ");
            }
        }

        writer.WriteLine(" {");

        writer.Scoped(() =>
        {
            foreach (var property in node.Properties)
            {
                property.Accept(this);
                writer.WriteLine();
            }

            foreach (var constructor in node.Constructors)
            {
                constructor.Accept(this);
                writer.WriteLine();
            }

            foreach (var method in node.Methods)
            {
                method.Accept(this);
                writer.WriteLine();
            }
        });

        writer.Write('}');
    }

    public void Visit(TypeNode node)
    {
        writer.Write(node.Name);
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
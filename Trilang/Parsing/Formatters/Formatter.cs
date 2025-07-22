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

    public void VisitArrayAccess(ArrayAccessExpressionNode node)
    {
        node.Member.Accept(this);
        writer.Write('[');
        node.Index.Accept(this);
        writer.Write(']');
    }

    public void VisitArrayType(ArrayTypeNode node)
    {
        node.ElementType.Accept(this);
        writer.Write("[]");
    }

    public void VisitAsExpression(AsExpressionNode node)
    {
        node.Expression.Accept(this);
        writer.Write(" as ");
        node.Type.Accept(this);
    }

    public void VisitBinaryExpression(BinaryExpressionNode node)
    {
        node.Left.Accept(this);
        writer.Write(' ');

        writer.Write(node.Kind switch
        {
            BinaryExpressionKind.Addition => "+",
            BinaryExpressionKind.Subtraction => "-",
            BinaryExpressionKind.Multiplication => "*",
            BinaryExpressionKind.Division => "/",
            BinaryExpressionKind.Modulus => "%",

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

    public void VisitBlock(BlockStatementNode node)
    {
        writer.WriteLine('{');
        writer.Scoped(() =>
        {
            foreach (var statement in node.Statements)
                statement.Accept(this);
        });
        writer.WriteLine('}');
    }

    public void VisitBreak(BreakNode node)
        => writer.WriteLine("break;");

    public void VisitCall(CallExpressionNode node)
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

    public void VisitConstructor(ConstructorDeclarationNode node)
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

    public void VisitContinue(ContinueNode node)
        => writer.WriteLine("continue;");

    public void VisitDiscriminatedUnion(DiscriminatedUnionNode node)
    {
        for (var i = 0; i < node.Types.Count; i++)
        {
            node.Types[i].Accept(this);

            if (i < node.Types.Count - 1)
                writer.Write(" | ");
        }
    }

    public void VisitExpressionStatement(ExpressionStatementNode node)
    {
        node.Expression.Accept(this);
        writer.WriteLine(';');
    }

    public void VisitProperty(PropertyDeclarationNode node)
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
                node.Setter?.Accept(this);
            });

            writer.Write('}');
        }
        else
        {
            writer.Write(';');
        }
    }

    public void VisitGetter(PropertyGetterNode node)
    {
        WriteAccessModifier(node.AccessModifier);
        writer.Write(" get");

        if (node.Body is null)
        {
            writer.WriteLine(';');
        }
        else
        {
            writer.Write(' ');
            node.Body.Accept(this);
        }
    }

    public void VisitSetter(PropertySetterNode node)
    {
        WriteAccessModifier(node.AccessModifier);
        writer.Write(" set");

        if (node.Body is null)
        {
            writer.WriteLine(';');
        }
        else
        {
            writer.Write(' ');
            node.Body.Accept(this);
        }
    }

    public void VisitFunction(FunctionDeclarationNode node)
    {
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

        node.Body.Accept(this);
    }

    public void VisitFunctionType(FunctionTypeNode node)
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

    public void VisitGenericType(GenericTypeNode node)
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

    public void VisitGoTo(GoToNode node)
    {
        writer.Write("goto ");
        writer.Write(node.Label);
        writer.WriteLine(';');
    }

    public void VisitIfDirective(IfDirectiveNode node)
    {
        writer.Write("#if ");
        writer.WriteLine(node.DirectiveName);
        writer.WriteLine();

        foreach (var then in node.Then)
        {
            then.Accept(this);
            writer.WriteLine();
        }

        if (node.Else.Count > 0)
        {
            writer.WriteLine("#else");
            writer.WriteLine();

            foreach (var @else in node.Else)
            {
                @else.Accept(this);
                writer.WriteLine();
            }
        }

        writer.WriteLine("#endif");
    }

    public void VisitIf(IfStatementNode node)
    {
        writer.Write("if (");
        node.Condition.Accept(this);
        writer.Write(") ");
        node.Then.Accept(this);

        if (node.Else is not null)
        {
            writer.RemoveLastNewLine();

            writer.Write(" else ");
            node.Else.Accept(this);
        }
    }

    public void VisitInterface(InterfaceNode node)
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

    public void VisitInterfaceProperty(InterfacePropertyNode node)
    {
        writer.Write(node.Name);
        writer.Write(": ");
        node.Type.Accept(this);

        var hasGetter = node.GetterModifier is not null;
        var hasSetter = node.SetterModifier is not null;
        if (hasGetter || hasSetter)
        {
            writer.Write(" {");

            if (hasGetter)
            {
                writer.Write(' ');
                WriteAccessModifier(node.GetterModifier!.Value);
                writer.Write(" get;");
            }

            if (hasSetter)
            {
                writer.Write(' ');
                WriteAccessModifier(node.SetterModifier!.Value);
                writer.Write(" set;");
            }

            writer.Write(" }");
        }
        else
        {
            writer.Write(';');
        }
    }

    public void VisitInterfaceMethod(InterfaceMethodNode node)
    {
        writer.Write(node.Name);
        writer.Write("(");

        for (var i = 0; i < node.ParameterTypes.Count; i++)
        {
            node.ParameterTypes[i].Accept(this);

            if (i < node.ParameterTypes.Count - 1)
                writer.Write(", ");
        }

        writer.Write("): ");
        node.ReturnType.Accept(this);
        writer.Write(";");
    }

    public void VisitLabel(LabelNode node)
    {
        writer.CancelIndent();
        writer.Write(node.Name);
        writer.WriteLine(':');
    }

    public void VisitLiteral(LiteralExpressionNode node)
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

    public void VisitMemberAccess(MemberAccessExpressionNode node)
    {
        if (node.Member is not null)
        {
            node.Member.Accept(this);
            writer.Write('.');
        }

        writer.Write(node.Name);
    }

    public void VisitMethod(MethodDeclarationNode node)
    {
        WriteAccessModifier(node.AccessModifier);

        if (node.IsStatic)
            writer.Write(" static");

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

    public void VisitNewArray(NewArrayExpressionNode node)
    {
        writer.Write("new ");
        node.Type.ElementType.Accept(this);
        writer.Write('[');
        node.Size.Accept(this);
        writer.Write(']');
    }

    public void VisitNewObject(NewObjectExpressionNode node)
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

    public void VisitNull(NullExpressionNode node)
        => writer.Write("null");

    public void VisitParameter(ParameterNode node)
    {
        writer.Write(node.Name);
        writer.Write(": ");
        node.Type.Accept(this);
    }

    public void VisitReturn(ReturnStatementNode node)
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

    public void VisitTree(SyntaxTree node)
    {
        var count = node.Declarations.Count;
        for (var i = 0; i < count; i++)
        {
            node.Declarations[i].Accept(this);

            if (i < count - 1)
                writer.WriteLine();
        }

        writer.RemoveLastNewLine();
    }

    public void VisitTuple(TupleExpressionNode node)
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

    public void VisitTupleType(TupleTypeNode node)
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

    public void VisitTypeAlias(TypeAliasDeclarationNode node)
    {
        WriteAccessModifier(node.AccessModifier);
        writer.Write(" type ");
        writer.Write(node.Name);
        WriteGenericArguments(node);
        writer.Write(" = ");
        node.Type.Accept(this);

        if (node.Type is not InterfaceNode)
            writer.Write(';');
    }

    public void VisitType(TypeDeclarationNode node)
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

            for (var i = 0; i < node.Methods.Count; i++)
            {
                node.Methods[i].Accept(this);

                if (i < node.Methods.Count - 1)
                    writer.WriteLine();
            }
        });

        writer.WriteLine('}');
    }

    public void VisitTypeNode(TypeNode node)
    {
        writer.Write(node.Name);
    }

    public void VisitUnaryExpression(UnaryExpressionNode node)
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

    public void VisitVariable(VariableDeclarationStatementNode node)
    {
        writer.Write("var ");
        writer.Write(node.Name);
        writer.Write(": ");
        node.Type.Accept(this);
        writer.Write(" = ");
        node.Expression.Accept(this);
        writer.WriteLine(';');
    }

    public void VisitWhile(WhileNode node)
    {
        writer.Write("while (");
        node.Condition.Accept(this);
        writer.Write(") ");
        node.Body.Accept(this);
    }

    public override string ToString()
        => writer.ToString();
}
using System.Text;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Tri.Tests;

public static class SyntaxNodeExtensions
{
    public static string Dump(this SyntaxTree node)
    {
        var visitor = new DumpVisitor();
        node.Accept(visitor);
        return visitor.ToString();
    }

    private class DumpVisitor : INodeVisitor
    {
        private readonly StringBuilder sb = new StringBuilder();
        private int indentLevel;

        public override string ToString()
            => sb.Length > 0 && sb[^1] == '\n'
                ? sb.ToString(0, sb.Length - 1)
                : sb.ToString();

        private void WriteLine(string text)
        {
            for (var i = 0; i < indentLevel; i++)
                sb.Append("  ");

            sb.AppendLine(text);
        }

        private void VisitChildren()
            => indentLevel++;

        private void EndChildren()
            => indentLevel--;

        private string FormatAccessModifier(AccessModifier mod)
            => mod switch
            {
                AccessModifier.Public => "public",
                AccessModifier.Internal => "internal",
                AccessModifier.Private => "private",
                _ => string.Empty
            };

        private string FormatTypeParts(IReadOnlyList<string> parts)
            => string.Join(".", parts);

        public void VisitTypeAlias(AliasDeclarationNode node)
        {
            WriteLine($"TypeAlias: {node.Name}");
            VisitChildren();
            WriteLine($"AccessModifier: {FormatAccessModifier(node.AccessModifier)}");
            node.Type.Accept(this);
            EndChildren();
        }

        public void VisitArrayAccess(ArrayAccessExpressionNode node)
        {
            WriteLine("ArrayAccess");
            VisitChildren();
            node.Member.Accept(this);
            node.Index.Accept(this);
            EndChildren();
        }

        public void VisitArrayType(ArrayTypeNode node)
        {
            WriteLine("ArrayType");
            VisitChildren();
            node.ElementType.Accept(this);
            EndChildren();
        }

        public void VisitBinaryExpression(BinaryExpressionNode node)
        {
            WriteLine($"BinaryExpression: {node.Kind}");
            VisitChildren();
            node.Left.Accept(this);
            node.Right.Accept(this);
            EndChildren();
        }

        public void VisitBlock(BlockStatementNode node)
        {
            WriteLine("BlockStatement");
            VisitChildren();

            if (node.Statements.Count > 0)
            {
                WriteLine("Statements");
                VisitChildren();

                foreach (var stmt in node.Statements)
                    stmt.Accept(this);

                EndChildren();
            }

            EndChildren();
        }

        public void VisitBreak(BreakNode node)
            => WriteLine("Break");

        public void VisitCall(CallExpressionNode node)
        {
            WriteLine("Call");
            VisitChildren();
            node.Member.Accept(this);

            if (node.Parameters.Count > 0)
            {
                WriteLine("Parameters");
                VisitChildren();

                foreach (var param in node.Parameters)
                    param.Accept(this);

                EndChildren();
            }

            EndChildren();
        }

        public void VisitCast(CastExpressionNode node)
        {
            WriteLine("Cast");
            VisitChildren();
            node.Type.Accept(this);
            node.Expression.Accept(this);
            EndChildren();
        }

        public void VisitConstructor(ConstructorDeclarationNode node)
        {
            WriteLine($"Constructor");
            VisitChildren();
            WriteLine($"AccessModifier: {FormatAccessModifier(node.AccessModifier)}");
            node.Body.Accept(this);
            EndChildren();
        }

        public void VisitContinue(ContinueNode node)
            => WriteLine("Continue");

        public void VisitDiscriminatedUnion(DiscriminatedUnionNode node)
        {
            WriteLine("DiscriminatedUnion");
            VisitChildren();

            if (node.Types.Count > 0)
            {
                WriteLine("Types");
                VisitChildren();

                foreach (var type in node.Types)
                    type.Accept(this);

                EndChildren();
            }

            EndChildren();
        }

        public void VisitExpressionStatement(ExpressionStatementNode node)
        {
            WriteLine("ExpressionStatement");
            VisitChildren();
            node.Expression.Accept(this);
            EndChildren();
        }

        public void VisitFakeDeclaration(FakeDeclarationNode node)
            => WriteLine("FakeDeclaration");

        public void VisitFakeExpression(FakeExpressionNode node)
            => WriteLine("FakeExpression");

        public void VisitFakeStatement(FakeStatementNode node)
            => WriteLine("FakeStatement");

        public void VisitFakeType(FakeTypeNode node)
            => WriteLine($"FakeType: {node.Name}");

        public void VisitFunction(FunctionDeclarationNode node)
        {
            WriteLine($"Function: {node.Name}");
            VisitChildren();
            WriteLine($"AccessModifier: {FormatAccessModifier(node.AccessModifier)}");

            if (node.Parameters.Count > 0)
            {
                WriteLine("Parameters");
                VisitChildren();

                foreach (var param in node.Parameters)
                    param.Accept(this);

                EndChildren();
            }

            node.ReturnType.Accept(this);
            node.Body.Accept(this);
            EndChildren();
        }

        public void VisitFunctionType(FunctionTypeNode node)
        {
            WriteLine("FunctionType");
            VisitChildren();

            if (node.ParameterTypes.Count > 0)
            {
                WriteLine("Parameters");
                VisitChildren();

                foreach (var paramType in node.ParameterTypes)
                    paramType.Accept(this);

                EndChildren();
            }

            node.ReturnType.Accept(this);
            EndChildren();
        }

        public void VisitGenericType(GenericApplicationNode node)
        {
            WriteLine("GenericApplication");
            VisitChildren();
            node.Type.Accept(this);

            if (node.TypeArguments.Count > 0)
            {
                WriteLine("TypeArguments");
                VisitChildren();

                foreach (var arg in node.TypeArguments)
                    arg.Accept(this);

                EndChildren();
            }

            EndChildren();
        }

        public void VisitIfDirective(IfDirectiveNode node)
        {
            WriteLine("IfDirective");
            VisitChildren();
            WriteLine($"DirectiveName: {node.DirectiveName}");

            if (node.Then.Count > 0)
            {
                WriteLine("Then");
                VisitChildren();

                foreach (var item in node.Then)
                    item.Accept(this);

                EndChildren();
            }

            if (node.Else.Count > 0)
            {
                WriteLine("Else");
                VisitChildren();

                foreach (var item in node.Else)
                    item.Accept(this);

                EndChildren();
            }

            EndChildren();
        }

        public void VisitIf(IfStatementNode node)
        {
            WriteLine("IfStatement");
            VisitChildren();
            node.Condition.Accept(this);
            node.Then.Accept(this);
            node.Else?.Accept(this);
            EndChildren();
        }

        public void VisitInterface(InterfaceNode node)
        {
            WriteLine("Interface");
            VisitChildren();

            if (node.Properties.Count > 0)
            {
                WriteLine("Properties");
                VisitChildren();

                foreach (var prop in node.Properties)
                    prop.Accept(this);

                EndChildren();
            }

            if (node.Methods.Count > 0)
            {
                WriteLine("Methods");
                VisitChildren();

                foreach (var method in node.Methods)
                    method.Accept(this);

                EndChildren();
            }

            EndChildren();
        }

        public void VisitInterfaceProperty(InterfacePropertyNode node)
        {
            WriteLine($"InterfaceProperty: {node.Name}");
            VisitChildren();
            node.Type.Accept(this);
            EndChildren();
        }

        public void VisitInterfaceMethod(InterfaceMethodNode node)
        {
            WriteLine($"InterfaceMethod: {node.Name}");
            VisitChildren();

            if (node.ParameterTypes.Count > 0)
            {
                WriteLine("Parameters");
                VisitChildren();

                foreach (var paramType in node.ParameterTypes)
                    paramType.Accept(this);

                EndChildren();
            }

            node.ReturnType.Accept(this);
            EndChildren();
        }

        public void VisitIsExpression(IsExpressionNode node)
        {
            WriteLine("IsExpression");
            VisitChildren();
            node.Expression.Accept(this);
            node.Type.Accept(this);
            EndChildren();
        }

        public void VisitLiteral(LiteralExpressionNode node)
            => WriteLine($"Literal: {node.Kind} = {node.Value}");

        public void VisitMemberAccess(MemberAccessExpressionNode node)
        {
            WriteLine("MemberAccess");
            VisitChildren();
            node.Member?.Accept(this);
            WriteLine($"Name: {node.Name}");
            EndChildren();
        }

        public void VisitMethod(MethodDeclarationNode node)
        {
            WriteLine($"Method: {node.Name}");
            VisitChildren();
            WriteLine($"AccessModifier: {FormatAccessModifier(node.AccessModifier)}");

            if (node.IsStatic)
                WriteLine("Static: true");

            node.ReturnType.Accept(this);
            node.Body.Accept(this);
            EndChildren();
        }

        public void VisitNamespace(NamespaceNode node)
        {
            WriteLine("Namespace");
            VisitChildren();
            WriteLine($"Parts: {FormatTypeParts(node.Parts)}");
            EndChildren();
        }

        public void VisitNewArray(NewArrayExpressionNode node)
        {
            WriteLine("NewArray");
            VisitChildren();
            node.Type.Accept(this);
            node.Size.Accept(this);
            EndChildren();
        }

        public void VisitNewObject(NewObjectExpressionNode node)
        {
            WriteLine("NewObject");
            VisitChildren();
            node.Type.Accept(this);

            if (node.Parameters.Count > 0)
            {
                WriteLine("Parameters");
                VisitChildren();

                foreach (var param in node.Parameters)
                    param.Accept(this);

                EndChildren();
            }

            EndChildren();
        }

        public void VisitNull(NullExpressionNode node)
            => WriteLine("Null");

        public void VisitParameter(ParameterNode node)
        {
            WriteLine($"Parameter: {node.Name}");
            VisitChildren();
            node.Type.Accept(this);
            EndChildren();
        }

        public void VisitProperty(PropertyDeclarationNode node)
        {
            WriteLine($"Property: {node.Name}");
            VisitChildren();
            node.Type.Accept(this);
            node.Getter?.Accept(this);
            node.Setter?.Accept(this);
            EndChildren();
        }

        public void VisitGetter(PropertyGetterNode node)
        {
            WriteLine("Getter");
            VisitChildren();
            WriteLine($"AccessModifier: {FormatAccessModifier(node.AccessModifier)}");
            node.Body?.Accept(this);
            EndChildren();
        }

        public void VisitSetter(PropertySetterNode node)
        {
            WriteLine("Setter");
            VisitChildren();
            WriteLine($"AccessModifier: {FormatAccessModifier(node.AccessModifier)}");
            node.Body?.Accept(this);
            EndChildren();
        }

        public void VisitReturn(ReturnStatementNode node)
        {
            WriteLine("ReturnStatement");
            VisitChildren();
            node.Expression?.Accept(this);
            EndChildren();
        }

        public void VisitTree(SyntaxTree node)
        {
            WriteLine("SyntaxTree");
            VisitChildren();
            node.Namespace.Accept(this);

            if (node.UseNodes.Count > 0)
            {
                WriteLine("UseNodes");
                VisitChildren();

                foreach (var use in node.UseNodes)
                    use.Accept(this);

                EndChildren();
            }

            if (node.Declarations.Count > 0)
            {
                WriteLine("Declarations");
                VisitChildren();

                foreach (var decl in node.Declarations)
                    decl.Accept(this);

                EndChildren();
            }

            EndChildren();
        }

        public void VisitTuple(TupleExpressionNode node)
        {
            WriteLine("TupleExpression");
            VisitChildren();

            if (node.Expressions.Count > 0)
            {
                WriteLine("Expressions");
                VisitChildren();

                foreach (var expr in node.Expressions)
                    expr.Accept(this);

                EndChildren();
            }

            EndChildren();
        }

        public void VisitTupleType(TupleTypeNode node)
        {
            WriteLine("TupleType");
            VisitChildren();

            if (node.Types.Count > 0)
            {
                WriteLine("Types");
                VisitChildren();

                foreach (var type in node.Types)
                    type.Accept(this);

                EndChildren();
            }

            EndChildren();
        }

        public void VisitType(TypeDeclarationNode node)
        {
            WriteLine($"Type: {node.Name}");
            VisitChildren();
            WriteLine($"AccessModifier: {FormatAccessModifier(node.AccessModifier)}");

            if (node.Interfaces.Count > 0)
            {
                WriteLine("Interfaces");
                VisitChildren();

                foreach (var @interface in node.Interfaces)
                    @interface.Accept(this);

                EndChildren();
            }

            if (node.GenericArguments.Count > 0)
            {
                WriteLine("Generic Arguments");
                VisitChildren();

                foreach (var arg in node.GenericArguments)
                    arg.Accept(this);

                EndChildren();
            }

            if (node.Properties.Count > 0)
            {
                WriteLine("Properties");
                VisitChildren();

                foreach (var property in node.Properties)
                    property.Accept(this);

                EndChildren();
            }

            if (node.Constructors.Count > 0)
            {
                WriteLine("Constructors");
                VisitChildren();

                foreach (var ctor in node.Constructors)
                    ctor.Accept(this);

                EndChildren();
            }

            if (node.Methods.Count > 0)
            {
                WriteLine("Methods");
                VisitChildren();

                foreach (var method in node.Methods)
                    method.Accept(this);

                EndChildren();
            }

            EndChildren();
        }

        public void VisitTypeNode(TypeRefNode node)
        {
            if (node.Package is not null)
                WriteLine($"TypeRef: {node.Package}::{FormatTypeParts(node.Parts)}");
            else
                WriteLine($"TypeRef: {FormatTypeParts(node.Parts)}");
        }

        public void VisitUnaryExpression(UnaryExpressionNode node)
        {
            WriteLine($"UnaryExpression: {node.Kind}");
            VisitChildren();
            node.Operand.Accept(this);
            EndChildren();
        }

        public void VisitUse(UseNode node)
        {
            WriteLine("Use");
            VisitChildren();

            if (node.Package is not null)
                WriteLine($"Package: {node.Package}");

            WriteLine($"Parts: {FormatTypeParts(node.Parts)}");
            EndChildren();
        }

        public void VisitVariable(VariableDeclarationNode node)
        {
            WriteLine($"Variable: {node.Name}");
            VisitChildren();
            node.Type.Accept(this);
            node.Expression.Accept(this);
            EndChildren();
        }

        public void VisitWhile(WhileNode node)
        {
            WriteLine("While");
            VisitChildren();
            node.Condition.Accept(this);
            node.Body.Accept(this);
            EndChildren();
        }
    }
}
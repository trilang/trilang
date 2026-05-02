using System.Diagnostics;
using Trilang.Compilation;
using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Trilang.Semantics.Passes;

internal class SymbolFinder : ISemanticPass
{
    private readonly ISet<string> directives;
    private readonly SymbolTableMap map;

    public SymbolFinder(ISet<string> directives, SymbolTableMap map)
    {
        this.directives = directives;
        this.map = map;
    }

    public void Analyze(Project project)
    {
        var semanticTrees = project.SourceFiles.Select(x => x.SemanticTree!);
        var visitor = new SymbolFinderVisitor(directives, map);
        foreach (var tree in semanticTrees)
            tree.Accept(visitor, new SymbolTable());
    }

    public string Name => nameof(SymbolFinder);

    public IEnumerable<string> DependsOn => [];

    private sealed class SymbolFinderVisitor : IVisitor<SymbolTable>
    {
        private readonly ISet<string> directives;
        private readonly SymbolTableMap map;

        public SymbolFinderVisitor(ISet<string> directives, SymbolTableMap map)
        {
            this.directives = directives;
            this.map = map;
        }

        public void VisitAlias(AliasDeclaration node, SymbolTable context)
        {
            map.Add(node, context);

            var child = context.CreateChild();

            foreach (var genericArgument in node.GenericArguments)
                genericArgument.Accept(this, child);

            node.Type.Accept(this, child);
        }

        public void VisitArrayAccess(ArrayAccessExpression node, SymbolTable context)
        {
            map.Add(node, context);

            node.Member.Accept(this, context);
            node.Index.Accept(this, context);
        }

        public void VisitArrayType(ArrayType node, SymbolTable context)
        {
            map.Add(node, context);

            node.ElementType.Accept(this, context);
        }

        public void VisitBinaryExpression(BinaryExpression node, SymbolTable context)
        {
            map.Add(node, context);

            node.Left.Accept(this, context);
            node.Right.Accept(this, context);
        }

        private void VisitBlockWithoutScope(BlockStatement node, SymbolTable context)
        {
            map.Add(node, context);

            foreach (var statement in node.Statements)
                statement.Accept(this, context);
        }

        public void VisitBlock(BlockStatement node, SymbolTable context)
            => VisitBlockWithoutScope(node, context.CreateChild());

        public void VisitBreak(Break node, SymbolTable context)
            => map.Add(node, context);

        public void VisitCall(CallExpression node, SymbolTable context)
        {
            map.Add(node, context);

            node.Member.Accept(this, context);

            foreach (var parameter in node.Parameters)
                parameter.Accept(this, context);
        }

        public void VisitCast(CastExpression node, SymbolTable context)
        {
            map.Add(node, context);

            node.Type.Accept(this, context);
            node.Expression.Accept(this, context);
        }

        public void VisitConstructor(ConstructorDeclaration node, SymbolTable context)
        {
            map.Add(node, context);

            var child = context.CreateChild();
            child.AddId(MemberAccessExpression.This, node.Parent!);

            foreach (var parameter in node.Parameters)
                parameter.Accept(this, child);

            VisitBlockWithoutScope(node.Body, child);
        }

        public void VisitContinue(Continue node, SymbolTable context)
            => map.Add(node, context);

        public void VisitDiscriminatedUnion(DiscriminatedUnion node, SymbolTable context)
        {
            var child = context.CreateChild();
            map.Add(node, child);

            foreach (var type in node.Types)
                type.Accept(this, child);
        }

        public void VisitExpressionBlock(ExpressionBlock node, SymbolTable context)
            => Debug.Fail("Expression blocks are the compiler's internal feature and are not directly supported in the programming language.");

        public void VisitExpressionStatement(ExpressionStatement node, SymbolTable context)
        {
            map.Add(node, context);

            node.Expression.Accept(this, context);
        }

        public void VisitFakeDeclaration(FakeDeclaration node, SymbolTable context)
            => map.Add(node, context);

        public void VisitFakeExpression(FakeExpression node, SymbolTable context)
            => map.Add(node, context);

        public void VisitFakeStatement(FakeStatement node, SymbolTable context)
            => map.Add(node, context);

        public void VisitFakeType(FakeType node, SymbolTable context)
            => map.Add(node, context);

        public void VisitFunction(FunctionDeclaration node, SymbolTable context)
        {
            map.Add(node, context);

            var child = context.CreateChild();
            foreach (var parameter in node.Parameters)
                parameter.Accept(this, child);

            node.ReturnType.Accept(this, context);
            VisitBlockWithoutScope(node.Body, child);
        }

        public void VisitFunctionType(FunctionType node, SymbolTable context)
        {
            map.Add(node, context);

            foreach (var parameterType in node.ParameterTypes)
                parameterType.Accept(this, context);

            node.ReturnType.Accept(this, context);
        }

        public void VisitGenericType(GenericApplication node, SymbolTable context)
        {
            map.Add(node, context);

            foreach (var typeArgument in node.TypeArguments)
                typeArgument.Accept(this, context);
        }

        public void VisitGoTo(GoTo node, SymbolTable context)
            => Debug.Fail("`goto` is the compiler's internal feature and are not directly supported in the programming language.");

        public void VisitIfDirective(IfDirective node, SymbolTable context)
        {
            if (directives.Contains(node.DirectiveName))
            {
                foreach (var then in node.Then)
                    then.Accept(this, context);
            }
            else
            {
                foreach (var @else in node.Else)
                    @else.Accept(this, context);
            }
        }

        public void VisitIf(IfStatement node, SymbolTable context)
        {
            map.Add(node, context);

            node.Condition.Accept(this, context);
            node.Then.Accept(this, context);
            node.Else?.Accept(this, context);
        }

        public void VisitInterface(Interface node, SymbolTable context)
        {
            var child = context.CreateChild();
            map.Add(node, child);

            foreach (var property in node.Properties)
                property.Accept(this, child);

            foreach (var method in node.Methods)
                method.Accept(this, child);
        }

        public void VisitInterfaceProperty(InterfaceProperty node, SymbolTable context)
        {
            map.Add(node, context);

            node.Type.Accept(this, context);
        }

        public void VisitInterfaceMethod(InterfaceMethod node, SymbolTable context)
        {
            map.Add(node, context);

            var child = context.CreateChild();
            foreach (var parameter in node.ParameterTypes)
                parameter.Accept(this, child);

            node.ReturnType.Accept(this, child);
        }

        public void VisitIsExpression(IsExpression node, SymbolTable context)
        {
            map.Add(node, context);

            node.Expression.Accept(this, context);
            node.Type.Accept(this, context);
        }

        public void VisitLabel(Label node, SymbolTable context)
            => Debug.Fail("Labels are the compiler's internal feature and are not directly supported in the programming language.");

        public void VisitLiteral(LiteralExpression node, SymbolTable context)
            => map.Add(node, context);

        public void VisitMemberAccess(MemberAccessExpression node, SymbolTable context)
        {
            node.Member?.Accept(this, context);

            map.Add(node, context);
        }

        public void VisitMethod(MethodDeclaration node, SymbolTable context)
        {
            map.Add(node, context);

            context.AddId(node.Name, node);

            node.ReturnType.Accept(this, context);

            var child = context.CreateChild();
            child.AddId(MemberAccessExpression.This, node.Parent!);

            foreach (var parameter in node.Parameters)
                parameter.Accept(this, child);

            VisitBlockWithoutScope(node.Body, child);
        }

        public void VisitNamespace(Namespace node, SymbolTable context)
            => map.Add(node, context);

        public void VisitNewArray(NewArrayExpression node, SymbolTable context)
        {
            map.Add(node, context);

            node.Type.Accept(this, context);
            node.Size.Accept(this, context);
        }

        public void VisitNewObject(NewObjectExpression node, SymbolTable context)
        {
            map.Add(node, context);

            node.Type.Accept(this, context);

            foreach (var parameter in node.Parameters)
                parameter.Accept(this, context);
        }

        public void VisitNull(NullExpression node, SymbolTable context)
            => map.Add(node, context);

        public void VisitReturn(ReturnStatement node, SymbolTable context)
        {
            map.Add(node, context);

            node.Expression?.Accept(this, context);
        }

        public void VisitParameter(Parameter node, SymbolTable context)
        {
            map.Add(node, context);

            context.AddId(node.Name, node);

            node.Type.Accept(this, context);
        }

        public void VisitProperty(PropertyDeclaration node, SymbolTable context)
        {
            map.Add(node, context);

            context.AddId(node.Name, node);

            node.Type.Accept(this, context);
            node.Getter?.Accept(this, context);
            node.Setter?.Accept(this, context);
        }

        public void VisitGetter(PropertyGetter node, SymbolTable context)
        {
            map.Add(node, context);

            var child = context.CreateChild();
            child.AddId(MemberAccessExpression.Field, node.Parent!);

            if (node.Body is not null)
                VisitBlockWithoutScope(node.Body, child);
        }

        public void VisitSetter(PropertySetter node, SymbolTable context)
        {
            map.Add(node, context);

            var child = context.CreateChild();
            child.AddId(MemberAccessExpression.Field, node.Parent!);
            child.AddId(MemberAccessExpression.Value, node.Parent!);

            if (node.Body is not null)
                VisitBlockWithoutScope(node.Body, child);
        }

        public void VisitTree(SemanticTree node, SymbolTable context)
        {
            map.Add(node, context);

            node.Namespace.Accept(this, context);

            foreach (var use in node.UseNodes)
                use.Accept(this, context);

            foreach (var declaration in node.Declarations)
                declaration.Accept(this, context);
        }

        public void VisitTuple(TupleExpression node, SymbolTable context)
        {
            map.Add(node, context);

            foreach (var expression in node.Expressions)
                expression.Accept(this, context);
        }

        public void VisitTupleType(TupleType node, SymbolTable context)
        {
            map.Add(node, context);

            foreach (var type in node.Types)
                type.Accept(this, context);
        }

        public void VisitType(TypeDeclaration node, SymbolTable context)
        {
            var child = context.CreateChild();
            map.Add(node, child);

            foreach (var genericArgument in node.GenericArguments)
                genericArgument.Accept(this, child);

            foreach (var @interface in node.Interfaces)
                @interface.Accept(this, child);

            foreach (var property in node.Properties)
                property.Accept(this, child);

            foreach (var constructor in node.Constructors)
                constructor.Accept(this, child);

            foreach (var method in node.Methods)
                method.Accept(this, child);
        }

        public void VisitTypeNode(TypeRef node, SymbolTable context)
            => map.Add(node, context);

        public void VisitUnaryExpression(UnaryExpression node, SymbolTable context)
        {
            map.Add(node, context);

            node.Operand.Accept(this, context);
        }

        public void VisitUse(Use node, SymbolTable context)
            => map.Add(node, context);

        public void VisitVariable(VariableDeclaration node, SymbolTable context)
        {
            map.Add(node, context);

            context.AddId(node.Name, node);

            node.Type.Accept(this, context);
            node.Expression.Accept(this, context);
        }

        public void VisitWhile(While node, SymbolTable context)
        {
            map.Add(node, context);

            node.Condition.Accept(this, context);
            node.Body.Accept(this, context);
        }
    }
}
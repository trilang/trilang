using Trilang.Parsing.Ast;

namespace Trilang;

public static class Helpers
{
    public static T? Find<T>(this ISyntaxNode? node, Func<T, bool>? predicate = null)
        where T : class, ISyntaxNode
    {
        if (node is null)
            return null;

        if (node is T syntaxNode && (predicate is null || predicate(syntaxNode)))
            return syntaxNode;

        return node switch
        {
            ArrayTypeNode arrayTypeNode
                => Find(arrayTypeNode.ElementType, predicate),

            ArrayAccessExpressionNode arrayNode
                => Find(arrayNode.Member, predicate) ??
                   Find(arrayNode.Index, predicate),

            BinaryExpressionNode binaryExpressionNode
                => Find(binaryExpressionNode.Left, predicate) ??
                   Find(binaryExpressionNode.Right, predicate),

            BlockStatementNode blockStatementNode
                => blockStatementNode.Statements
                    .Select(x => Find(x, predicate))
                    .FirstOrDefault(x => x is not null),

            BreakNode
                => null,

            CallExpressionNode callExpressionNode
                => Find(callExpressionNode.Member, predicate) ??
                   callExpressionNode.Parameters
                       .Select(x => Find(x, predicate))
                       .FirstOrDefault(x => x is not null),

            ConstructorDeclarationNode constructorDeclarationNode
                => constructorDeclarationNode.Parameters
                       .Select(x => Find(x, predicate))
                       .FirstOrDefault(x => x is not null) ??
                   Find(constructorDeclarationNode.Body, predicate),

            ContinueNode
                => null,

            DiscriminatedUnionNode discriminatedUnionNode
                => discriminatedUnionNode.Types
                    .Select(x => Find(x, predicate))
                    .FirstOrDefault(x => x is not null),

            ExpressionStatementNode expressionStatementNode
                => Find(expressionStatementNode.Expression, predicate),

            FunctionDeclarationNode functionDeclarationNode
                => functionDeclarationNode.Parameters
                       .Select(x => Find(x, predicate))
                       .FirstOrDefault(x => x is not null) ??
                   Find(functionDeclarationNode.Body, predicate) ??
                   Find(functionDeclarationNode.ReturnType, predicate),

            FunctionTypeNode functionTypeDeclarationNode
                => functionTypeDeclarationNode.ParameterTypes
                       .Select(x => Find(x, predicate))
                       .FirstOrDefault(x => x is not null) ??
                   Find(functionTypeDeclarationNode.ReturnType, predicate),

            GenericTypeNode genericTypeNode
                => genericTypeNode.TypeArguments
                    .Select(x => Find(x, predicate))
                    .FirstOrDefault(x => x is not null),

            IfStatementNode ifStatementNode
                => Find(ifStatementNode.Condition, predicate) ??
                   Find(ifStatementNode.Then, predicate) ??
                   Find(ifStatementNode.Else, predicate),

            InterfaceNode interfaceNode
                => interfaceNode.Properties
                       .Select(x => Find(x, predicate))
                       .FirstOrDefault(x => x is not null) ??
                   interfaceNode.Methods
                       .Select(x => Find(x, predicate))
                       .FirstOrDefault(x => x is not null),

            InterfacePropertyNode interfacePropertyNode
                => Find(interfacePropertyNode.Type, predicate),

            InterfaceMethodNode interfaceMethodNode
                => interfaceMethodNode.ParameterTypes
                       .Select(x => Find(x, predicate))
                       .FirstOrDefault(x => x is not null) ??
                   Find(interfaceMethodNode.ReturnType, predicate),

            LiteralExpressionNode
                => null,

            MemberAccessExpressionNode memberAccessExpressionNode
                => Find(memberAccessExpressionNode.Member, predicate),

            MethodDeclarationNode methodDeclarationNode
                => methodDeclarationNode.Parameters
                       .Select(x => Find(x, predicate))
                       .FirstOrDefault(x => x is not null) ??
                   Find(methodDeclarationNode.ReturnType, predicate) ??
                   Find(methodDeclarationNode.Body, predicate),

            NewArrayExpressionNode newArrayExpressionNode
                => Find(newArrayExpressionNode.Type, predicate) ??
                   Find(newArrayExpressionNode.Size, predicate),

            NewObjectExpressionNode newExpressionNode
                => Find(newExpressionNode.Type, predicate) ??
                   newExpressionNode.Parameters
                       .Select(x => Find(x, predicate))
                       .FirstOrDefault(x => x is not null),

            NullExpressionNode
                => null,

            ParameterNode parameterNode
                => Find(parameterNode.Type, predicate),

            PropertyDeclarationNode propertyDeclarationNode
                => Find(propertyDeclarationNode.Type, predicate) ??
                   Find(propertyDeclarationNode.Getter, predicate) ??
                   Find(propertyDeclarationNode.Setter, predicate),

            PropertyGetterNode propertyGetterNode
                => Find(propertyGetterNode.Body, predicate),

            PropertySetterNode propertySetterNode
                => Find(propertySetterNode.Body, predicate),

            ReturnStatementNode returnStatementNode
                => Find(returnStatementNode.Expression, predicate),

            SyntaxTree syntaxTree
                => syntaxTree.Declarations
                    .Select(x => Find(x, predicate))
                    .FirstOrDefault(x => x is not null),

            TupleExpressionNode tupleExpressionNode
                => tupleExpressionNode.Expressions
                    .Select(x => Find(x, predicate))
                    .FirstOrDefault(x => x is not null),

            TupleTypeNode tupleTypeNode
                => tupleTypeNode.Types
                    .Select(x => Find(x, predicate))
                    .FirstOrDefault(x => x is not null),

            TypeAliasDeclarationNode typeAliasNode
                => Find(typeAliasNode.Type, predicate),

            TypeDeclarationNode typeDeclarationNode
                => typeDeclarationNode.GenericArguments
                       .Select(x => Find(x, predicate))
                       .FirstOrDefault(x => x is not null) ??
                   typeDeclarationNode.Properties
                       .Select(x => Find(x, predicate))
                       .FirstOrDefault(x => x is not null) ??
                   typeDeclarationNode.Methods
                       .Select(x => Find(x, predicate))
                       .FirstOrDefault(x => x is not null),

            TypeNode
                => null,

            UnaryExpressionNode unaryExpressionNode
                => Find(unaryExpressionNode.Operand, predicate),

            VariableDeclarationStatementNode variableDeclarationStatementNode
                => Find(variableDeclarationStatementNode.Expression, predicate),

            WhileNode whileNode
                => Find(whileNode.Condition, predicate) ??
                   Find(whileNode.Body, predicate),

            _ => throw new ArgumentOutOfRangeException(nameof(node)),
        };
    }

    public static T? FindInParent<T>(this ISyntaxNode node)
        where T : ISyntaxNode
    {
        var parent = node.Parent;
        while (parent is not null)
        {
            if (parent is T result)
                return result;

            parent = parent.Parent;
        }

        return default;
    }
}
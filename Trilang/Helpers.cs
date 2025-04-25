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

            BreakNode breakNode
                => null,

            CallExpressionNode callExpressionNode
                => callExpressionNode.Parameters
                    .Select(x => Find(x, predicate))
                    .FirstOrDefault(x => x is not null),

            ContinueNode
                => null,

            ExpressionStatementNode expressionStatementNode
                => Find(expressionStatementNode.Expression, predicate),

            FieldDeclarationNode fieldDeclarationNode
                => Find(fieldDeclarationNode.Type, predicate),

            FunctionDeclarationNode functionDeclarationNode
                => functionDeclarationNode.Parameters
                       .Select(x => Find(x, predicate))
                       .FirstOrDefault(x => x is not null) ??
                   Find(functionDeclarationNode.Body, predicate),

            FunctionTypeNode functionTypeDeclarationNode
                => functionTypeDeclarationNode.ParameterTypes
                       .Select(x => Find(x, predicate))
                       .FirstOrDefault(x => x is not null) ??
                   Find(functionTypeDeclarationNode.ReturnType, predicate),

            InterfaceNode interfaceNode
                => interfaceNode.Fields
                       .Select(x => Find(x, predicate))
                       .FirstOrDefault(x => x is not null) ??
                   interfaceNode.Methods
                       .Select(x => Find(x, predicate))
                       .FirstOrDefault(x => x is not null),

            InterfaceFieldNode interfaceFieldNode
                => Find(interfaceFieldNode.Type, predicate),

            InterfaceMethodNode interfaceMethodNode
                => interfaceMethodNode.Parameters
                       .Select(x => Find(x, predicate))
                       .FirstOrDefault(x => x is not null) ??
                   Find(interfaceMethodNode.ReturnType, predicate),

            IfStatementNode ifStatementNode
                => Find(ifStatementNode.Condition, predicate) ??
                   Find(ifStatementNode.Then, predicate) ??
                   Find(ifStatementNode.Else, predicate),

            LiteralExpressionNode
                => null,

            MethodDeclarationNode methodDeclarationNode
                => methodDeclarationNode.Parameters
                       .Select(x => Find(x, predicate))
                       .FirstOrDefault(x => x is not null) ??
                   Find(methodDeclarationNode.ReturnType, predicate) ??
                   Find(methodDeclarationNode.Body, predicate),

            MemberAccessExpressionNode memberAccessExpressionNode
                => Find(memberAccessExpressionNode.Member, predicate),

            ParameterNode parameterNode
                => Find(parameterNode.Type, predicate),

            ReturnStatementNode returnStatementNode
                => Find(returnStatementNode.Expression, predicate),

            SyntaxTree syntaxTree
                => syntaxTree.Declarations
                    .Select(x => Find(x, predicate))
                    .FirstOrDefault(x => x is not null),

            TypeAliasDeclarationNode typeAliasNode
                => Find(typeAliasNode.Type, predicate),

            TypeDeclarationNode typeDeclarationNode
                => typeDeclarationNode.Fields
                       .Select(x => Find(x, predicate))
                       .FirstOrDefault(x => x is not null) ??
                   typeDeclarationNode.Methods
                       .Select(x => Find(x, predicate))
                       .FirstOrDefault(x => x is not null),

            TypeNode
                => null,

            VariableDeclarationStatementNode variableDeclarationStatementNode
                => Find(variableDeclarationStatementNode.Expression, predicate),

            UnaryExpressionNode unaryExpressionNode
                => Find(unaryExpressionNode.Operand, predicate),

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
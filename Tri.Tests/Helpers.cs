using Trilang.Parsing.Ast;

namespace Tri.Tests;

internal static class Helpers
{
    public static T? Find<T>(this ISyntaxNode? node)
        where T : class, ISyntaxNode
    {
        if (node is null)
            return null;

        if (node is T syntaxNode)
            return syntaxNode;

        return node switch
        {
            ArrayAccessExpressionNode arrayNode
                => Find<T>(arrayNode.Member) ??
                   Find<T>(arrayNode.Index),

            BinaryExpressionNode binaryExpressionNode
                => Find<T>(binaryExpressionNode.Left) ??
                   Find<T>(binaryExpressionNode.Right),

            BlockStatementNode blockStatementNode
                => blockStatementNode.Statements
                    .Select(Find<T>)
                    .FirstOrDefault(x => x is not null),

            BreakNode breakNode
                => null,

            CallExpressionNode callExpressionNode
                => callExpressionNode.Parameters
                    .Select(Find<T>)
                    .FirstOrDefault(x => x is not null),

            ContinueNode continueNode
                => null,

            ExpressionStatementNode expressionStatementNode
                => Find<T>(expressionStatementNode.Expression),

            FieldDeclarationNode fieldDeclarationNode
                => Find<T>(fieldDeclarationNode.Type),

            FunctionDeclarationNode functionDeclarationNode
                => functionDeclarationNode.Parameters
                       .Select(Find<T>)
                       .FirstOrDefault(x => x is not null) ??
                   Find<T>(functionDeclarationNode.Body),

            FunctionTypeNode functionTypeDeclarationNode
                => functionTypeDeclarationNode.ParameterTypes
                       .Select(Find<T>)
                       .FirstOrDefault(x => x is not null) ??
                   Find<T>(functionTypeDeclarationNode.ReturnType),

            IfStatementNode ifStatementNode
                => Find<T>(ifStatementNode.Condition) ??
                   Find<T>(ifStatementNode.Then) ??
                   Find<T>(ifStatementNode.Else),

            LiteralExpressionNode
                => null,

            MethodDeclarationNode methodDeclarationNode
                => methodDeclarationNode.Parameters
                       .Select(Find<T>)
                       .FirstOrDefault(x => x is not null) ??
                   Find<T>(methodDeclarationNode.ReturnType) ??
                   Find<T>(methodDeclarationNode.Body),

            MemberAccessExpressionNode
                => null,

            ParameterNode
                => null,

            ReturnStatementNode returnStatementNode
                => Find<T>(returnStatementNode.Expression),

            SyntaxTree syntaxTree
                => syntaxTree.Declarations
                    .Select(Find<T>)
                    .FirstOrDefault(x => x is not null),

            TypeAliasDeclarationNode typeAliasNode
                => Find<T>(typeAliasNode.Type),

            TypeDeclarationNode typeDeclarationNode
                => typeDeclarationNode.Fields
                       .Select(Find<T>)
                       .FirstOrDefault(x => x is not null) ??
                   typeDeclarationNode.Methods
                       .Select(Find<T>)
                       .FirstOrDefault(x => x is not null),

            TypeNode typeNode
                => null,

            VariableDeclarationStatementNode variableDeclarationStatementNode
                => Find<T>(variableDeclarationStatementNode.Expression),

            UnaryExpressionNode unaryExpressionNode
                => Find<T>(unaryExpressionNode.Operand),

            WhileNode whileNode
                => Find<T>(whileNode.Condition) ??
                   Find<T>(whileNode.Body),

            _ => throw new ArgumentOutOfRangeException(nameof(node)),
        };
    }
}
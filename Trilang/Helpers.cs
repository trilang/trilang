using Trilang.Parsing.Ast;

namespace Trilang;

public static class Helpers
{
    public static T? Find<T>(this ISyntaxNode? node, Func<T, bool>? predicate = null)
        where T : class, ISyntaxNode
        => Where(node, predicate).FirstOrDefault();

    public static IEnumerable<T> Where<T>(this ISyntaxNode? node, Func<T, bool>? predicate = null)
        where T : class, ISyntaxNode
    {
        if (node is null)
            yield break;

        var q = new Queue<ISyntaxNode?>();
        q.Enqueue(node);

        while (q.TryDequeue(out node))
        {
            if (node is T syntaxNode && (predicate is null || predicate(syntaxNode)))
                yield return syntaxNode;

            switch (node)
            {
                case ArrayTypeNode arrayTypeNode:
                    q.Enqueue(arrayTypeNode.ElementType);
                    break;
                case ArrayAccessExpressionNode arrayNode:
                    q.Enqueue(arrayNode.Member);
                    q.Enqueue(arrayNode.Index);
                    break;
                case BinaryExpressionNode binaryExpressionNode:
                    q.Enqueue(binaryExpressionNode.Left);
                    q.Enqueue(binaryExpressionNode.Right);
                    break;
                case BlockStatementNode blockStatementNode:
                    foreach (var statement in blockStatementNode.Statements)
                        q.Enqueue(statement);

                    break;
                case BreakNode:
                    break;
                case CallExpressionNode callExpressionNode:
                    q.Enqueue(callExpressionNode.Member);
                    foreach (var parameter in callExpressionNode.Parameters)
                        q.Enqueue(parameter);

                    break;
                case CastExpressionNode castExpressionNode:
                    q.Enqueue(castExpressionNode.Type);
                    q.Enqueue(castExpressionNode.Expression);
                    break;
                case ConstructorDeclarationNode constructorDeclarationNode:
                    foreach (var parameter in constructorDeclarationNode.Parameters)
                        q.Enqueue(parameter);

                    q.Enqueue(constructorDeclarationNode.Body);
                    break;
                case ContinueNode:
                    break;
                case DiscriminatedUnionNode discriminatedUnionNode:
                    foreach (var type in discriminatedUnionNode.Types)
                        q.Enqueue(type);

                    break;
                case ExpressionBlockNode expressionBlockNode:
                    foreach (var statement in expressionBlockNode.Statements)
                        q.Enqueue(statement);

                    break;
                case ExpressionStatementNode expressionStatementNode:
                    q.Enqueue(expressionStatementNode.Expression);
                    break;
                case FunctionDeclarationNode functionDeclarationNode:
                    foreach (var parameter in functionDeclarationNode.Parameters)
                        q.Enqueue(parameter);

                    q.Enqueue(functionDeclarationNode.Body);
                    q.Enqueue(functionDeclarationNode.ReturnType);
                    break;
                case FunctionTypeNode functionTypeDeclarationNode:
                    foreach (var parameterType in functionTypeDeclarationNode.ParameterTypes)
                        q.Enqueue(parameterType);

                    q.Enqueue(functionTypeDeclarationNode.ReturnType);
                    break;
                case GenericTypeNode genericTypeNode:
                    foreach (var typeArgument in genericTypeNode.TypeArguments)
                        q.Enqueue(typeArgument);

                    break;
                case GoToNode:
                    break;
                case IfDirectiveNode ifDirectiveNode:
                    foreach (var item in ifDirectiveNode.Then)
                        q.Enqueue(item);

                    foreach (var item in ifDirectiveNode.Else)
                        q.Enqueue(item);

                    break;
                case IfStatementNode ifStatementNode:
                    q.Enqueue(ifStatementNode.Condition);
                    q.Enqueue(ifStatementNode.Then);
                    q.Enqueue(ifStatementNode.Else);
                    break;
                case InterfaceNode interfaceNode:
                    foreach (var property in interfaceNode.Properties)
                        q.Enqueue(property);

                    foreach (var method in interfaceNode.Methods)
                        q.Enqueue(method);

                    break;
                case InterfacePropertyNode interfacePropertyNode:
                    q.Enqueue(interfacePropertyNode.Type);
                    break;
                case IsExpressionNode isExpressionNode:
                    q.Enqueue(isExpressionNode.Expression);
                    q.Enqueue(isExpressionNode.Type);
                    break;
                case LabelNode:
                    break;
                case InterfaceMethodNode interfaceMethodNode:
                    foreach (var parameterTypes in interfaceMethodNode.ParameterTypes)
                        q.Enqueue(parameterTypes);

                    q.Enqueue(interfaceMethodNode.ReturnType);
                    break;
                case LiteralExpressionNode:
                    break;
                case MemberAccessExpressionNode memberAccessExpressionNode:
                    q.Enqueue(memberAccessExpressionNode.Member);
                    break;
                case MethodDeclarationNode methodDeclarationNode:
                    foreach (var parameter in methodDeclarationNode.Parameters)
                        q.Enqueue(parameter);

                    q.Enqueue(methodDeclarationNode.ReturnType);
                    q.Enqueue(methodDeclarationNode.Body);
                    break;
                case NewArrayExpressionNode newArrayExpressionNode:
                    q.Enqueue(newArrayExpressionNode.Type);
                    q.Enqueue(newArrayExpressionNode.Size);
                    break;
                case NewObjectExpressionNode newExpressionNode:
                    q.Enqueue(newExpressionNode.Type);
                    foreach (var parameter in newExpressionNode.Parameters)
                        q.Enqueue(parameter);

                    break;
                case NullExpressionNode:
                    break;
                case ParameterNode parameterNode:
                    q.Enqueue(parameterNode.Type);
                    break;
                case PropertyDeclarationNode propertyDeclarationNode:
                    q.Enqueue(propertyDeclarationNode.Type);
                    q.Enqueue(propertyDeclarationNode.Getter);
                    q.Enqueue(propertyDeclarationNode.Setter);
                    break;
                case PropertyGetterNode propertyGetterNode:
                    q.Enqueue(propertyGetterNode.Body);
                    break;
                case PropertySetterNode propertySetterNode:
                    q.Enqueue(propertySetterNode.Body);
                    break;
                case ReturnStatementNode returnStatementNode:
                    q.Enqueue(returnStatementNode.Expression);
                    break;
                case SyntaxTree syntaxTree:
                    foreach (var declaration in syntaxTree.Declarations)
                        q.Enqueue(declaration);

                    break;
                case TupleExpressionNode tupleExpressionNode:
                    foreach (var expression in tupleExpressionNode.Expressions)
                        q.Enqueue(expression);

                    break;
                case TupleTypeNode tupleTypeNode:
                    foreach (var type in tupleTypeNode.Types)
                        q.Enqueue(type);

                    break;
                case TypeAliasDeclarationNode typeAliasNode:
                    q.Enqueue(typeAliasNode.Type);
                    break;
                case TypeDeclarationNode typeDeclarationNode:
                    foreach (var genericArgument in typeDeclarationNode.GenericArguments)
                        q.Enqueue(genericArgument);

                    foreach (var property in typeDeclarationNode.Properties)
                        q.Enqueue(property);

                    foreach (var method in typeDeclarationNode.Methods)
                        q.Enqueue(method);

                    foreach (var constructor in typeDeclarationNode.Constructors)
                        q.Enqueue(constructor);

                    break;
                case TypeNode:
                    break;
                case UnaryExpressionNode unaryExpressionNode:
                    q.Enqueue(unaryExpressionNode.Operand);
                    break;
                case VariableDeclarationStatementNode variableDeclarationStatementNode:
                    q.Enqueue(variableDeclarationStatementNode.Expression);
                    break;
                case WhileNode whileNode:
                    q.Enqueue(whileNode.Condition);
                    q.Enqueue(whileNode.Body);
                    break;

                case null:
                    break;
                default:
                    throw new ArgumentOutOfRangeException(nameof(node));
            }
        }
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
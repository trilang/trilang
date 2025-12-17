using Trilang.Semantics.Model;

namespace Trilang;

public static class SemanticNodeExtensions
{
    public static T? Find<T>(this ISemanticNode? node, Func<T, bool>? predicate = null)
        where T : class, ISemanticNode
        => Where(node, predicate).FirstOrDefault();

    public static IEnumerable<T> Where<T>(this ISemanticNode? node, Func<T, bool>? predicate = null)
        where T : class, ISemanticNode
    {
        if (node is null)
            yield break;

        var q = new Queue<ISemanticNode?>();
        q.Enqueue(node);

        while (q.TryDequeue(out node))
        {
            if (node is T syntaxNode && (predicate is null || predicate(syntaxNode)))
                yield return syntaxNode;

            switch (node)
            {
                case ArrayType arrayTypeNode:
                    q.Enqueue(arrayTypeNode.ElementType);
                    break;
                case ArrayAccessExpression arrayNode:
                    q.Enqueue(arrayNode.Member);
                    q.Enqueue(arrayNode.Index);
                    break;
                case BinaryExpression binaryExpressionNode:
                    q.Enqueue(binaryExpressionNode.Left);
                    q.Enqueue(binaryExpressionNode.Right);
                    break;
                case BlockStatement blockStatementNode:
                    foreach (var statement in blockStatementNode.Statements)
                        q.Enqueue(statement);

                    break;
                case Break:
                    break;
                case CallExpression callExpressionNode:
                    q.Enqueue(callExpressionNode.Member);
                    foreach (var parameter in callExpressionNode.Parameters)
                        q.Enqueue(parameter);

                    break;
                case CastExpression castExpressionNode:
                    q.Enqueue(castExpressionNode.Type);
                    q.Enqueue(castExpressionNode.Expression);
                    break;
                case ConstructorDeclaration constructorDeclarationNode:
                    foreach (var parameter in constructorDeclarationNode.Parameters)
                        q.Enqueue(parameter);

                    q.Enqueue(constructorDeclarationNode.Body);
                    break;
                case Continue:
                    break;
                case DiscriminatedUnion discriminatedUnionNode:
                    foreach (var type in discriminatedUnionNode.Types)
                        q.Enqueue(type);

                    break;
                case ExpressionBlock expressionBlockNode:
                    foreach (var statement in expressionBlockNode.Statements)
                        q.Enqueue(statement);

                    break;
                case ExpressionStatement expressionStatementNode:
                    q.Enqueue(expressionStatementNode.Expression);
                    break;
                case FakeDeclaration:
                    break;
                case FakeExpression:
                    break;
                case FakeStatement:
                    break;
                case FakeType:
                    break;
                case FunctionDeclaration functionDeclarationNode:
                    foreach (var parameter in functionDeclarationNode.Parameters)
                        q.Enqueue(parameter);

                    q.Enqueue(functionDeclarationNode.Body);
                    q.Enqueue(functionDeclarationNode.ReturnType);
                    break;
                case FunctionType functionTypeDeclarationNode:
                    foreach (var parameterType in functionTypeDeclarationNode.ParameterTypes)
                        q.Enqueue(parameterType);

                    q.Enqueue(functionTypeDeclarationNode.ReturnType);
                    break;
                case GenericTypeRef genericTypeNode:
                    foreach (var typeArgument in genericTypeNode.TypeArguments)
                        q.Enqueue(typeArgument);

                    break;
                case GoTo:
                    break;
                case IfDirective ifDirectiveNode:
                    foreach (var item in ifDirectiveNode.Then)
                        q.Enqueue(item);

                    foreach (var item in ifDirectiveNode.Else)
                        q.Enqueue(item);

                    break;
                case IfStatement ifStatementNode:
                    q.Enqueue(ifStatementNode.Condition);
                    q.Enqueue(ifStatementNode.Then);
                    q.Enqueue(ifStatementNode.Else);
                    break;
                case Interface interfaceNode:
                    foreach (var property in interfaceNode.Properties)
                        q.Enqueue(property);

                    foreach (var method in interfaceNode.Methods)
                        q.Enqueue(method);

                    break;
                case InterfaceProperty interfacePropertyNode:
                    q.Enqueue(interfacePropertyNode.Type);
                    break;
                case IsExpression isExpressionNode:
                    q.Enqueue(isExpressionNode.Expression);
                    q.Enqueue(isExpressionNode.Type);
                    break;
                case Label:
                    break;
                case InterfaceMethod interfaceMethodNode:
                    foreach (var parameterTypes in interfaceMethodNode.ParameterTypes)
                        q.Enqueue(parameterTypes);

                    q.Enqueue(interfaceMethodNode.ReturnType);
                    break;
                case LiteralExpression:
                    break;
                case MemberAccessExpression memberAccessExpressionNode:
                    q.Enqueue(memberAccessExpressionNode.Member);
                    break;
                case MethodDeclaration methodDeclarationNode:
                    foreach (var parameter in methodDeclarationNode.Parameters)
                        q.Enqueue(parameter);

                    q.Enqueue(methodDeclarationNode.ReturnType);
                    q.Enqueue(methodDeclarationNode.Body);
                    break;
                case Namespace:
                    break;
                case NewArrayExpression newArrayExpressionNode:
                    q.Enqueue(newArrayExpressionNode.Type);
                    q.Enqueue(newArrayExpressionNode.Size);
                    break;
                case NewObjectExpression newExpressionNode:
                    q.Enqueue(newExpressionNode.Type);
                    foreach (var parameter in newExpressionNode.Parameters)
                        q.Enqueue(parameter);

                    break;
                case NullExpression:
                    break;
                case Parameter parameterNode:
                    q.Enqueue(parameterNode.Type);
                    break;
                case PropertyDeclaration propertyDeclarationNode:
                    q.Enqueue(propertyDeclarationNode.Type);
                    q.Enqueue(propertyDeclarationNode.Getter);
                    q.Enqueue(propertyDeclarationNode.Setter);
                    break;
                case PropertyGetter propertyGetterNode:
                    q.Enqueue(propertyGetterNode.Body);
                    break;
                case PropertySetter propertySetterNode:
                    q.Enqueue(propertySetterNode.Body);
                    break;
                case ReturnStatement returnStatementNode:
                    q.Enqueue(returnStatementNode.Expression);
                    break;
                case SemanticTree syntaxTree:
                    foreach (var declaration in syntaxTree.Declarations)
                        q.Enqueue(declaration);

                    break;
                case TupleExpression tupleExpressionNode:
                    foreach (var expression in tupleExpressionNode.Expressions)
                        q.Enqueue(expression);

                    break;
                case TupleType tupleTypeNode:
                    foreach (var type in tupleTypeNode.Types)
                        q.Enqueue(type);

                    break;
                case AliasDeclaration typeAliasNode:
                    q.Enqueue(typeAliasNode.Type);
                    break;
                case TypeDeclaration typeDeclarationNode:
                    foreach (var genericArgument in typeDeclarationNode.GenericArguments)
                        q.Enqueue(genericArgument);

                    foreach (var property in typeDeclarationNode.Properties)
                        q.Enqueue(property);

                    foreach (var method in typeDeclarationNode.Methods)
                        q.Enqueue(method);

                    foreach (var constructor in typeDeclarationNode.Constructors)
                        q.Enqueue(constructor);

                    break;
                case TypeRef:
                    break;
                case UnaryExpression unaryExpressionNode:
                    q.Enqueue(unaryExpressionNode.Operand);
                    break;
                case Use:
                    break;
                case VariableDeclaration variableDeclarationStatementNode:
                    q.Enqueue(variableDeclarationStatementNode.Expression);
                    break;
                case While whileNode:
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

    public static T? FindInParent<T>(this ISemanticNode? node)
        where T : ISemanticNode
    {
        while (node is not null)
        {
            if (node is T result)
                return result;

            node = node.Parent;
        }

        return default;
    }

    public static SemanticTree GetRoot(this ISemanticNode node)
        => node.FindInParent<SemanticTree>() ?? throw new Exception();

    public static SourceLocation GetLocation(this ISemanticNode node)
        => new SourceLocation(node.GetRoot().SourceFile, node.SourceSpan ?? default);
}
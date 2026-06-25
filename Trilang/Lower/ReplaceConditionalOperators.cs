using Trilang.Metadata;
using Trilang.Semantics;
using Trilang.Semantics.Model;

namespace Trilang.Lower;

internal class ReplaceConditionalOperators : Transformer
{
    private int conditionCounter;

    public ReplaceConditionalOperators(ISet<string> directives, BuiltInTypes builtInTypes)
        : base(directives, builtInTypes)
    {
        conditionCounter = 0;
    }

    public override ISemanticNode TransformAlias(AliasDeclaration node)
        => node;

    public override ISemanticNode TransformArrayType(ArrayType node)
        => node;

    public override ISemanticNode TransformBinaryExpression(BinaryExpression node)
    {
        var left = (IExpression)node.Left.Transform(this);
        var right = (IExpression)node.Right.Transform(this);

        if (node.Kind == BinaryExpressionKind.ConditionalAnd)
        {
            var variableName = $"cond_{conditionCounter++}";
            var variableMetadata = new VariableMetadata(null, variableName, builtInTypes.Bool);
            var result = new ExpressionBlock([
                new VariableDeclaration(
                    null,
                    variableName,
                    new TypeRef(null, null, [builtInTypes.Bool.Name])
                    {
                        Metadata = builtInTypes.Bool,
                    },
                    left
                )
                {
                    Metadata = variableMetadata,
                },
                new IfStatement(
                    null,
                    new MemberAccessExpression(null, variableName)
                    {
                        AccessKind = MemberAccessKind.Read,
                        Reference = variableMetadata,
                    },
                    new BlockStatement(null, [
                        new ExpressionStatement(
                            null,
                            new BinaryExpression(
                                null,
                                BinaryExpressionKind.Assignment,
                                new MemberAccessExpression(null, variableName)
                                {
                                    AccessKind = MemberAccessKind.Write,
                                    Reference = variableMetadata,
                                },
                                right
                            )
                        )
                    ])
                ),
                new ExpressionStatement(
                    null,
                    new MemberAccessExpression(null, variableName)
                    {
                        AccessKind = MemberAccessKind.Read,
                        Reference = variableMetadata,
                    }
                ),
            ]);

            return result;
        }

        if (node.Kind == BinaryExpressionKind.ConditionalOr)
        {
            var variableName = $"cond_{conditionCounter++}";
            var variableMetadata = new VariableMetadata(null, variableName, builtInTypes.Bool);
            var result = new ExpressionBlock([
                new VariableDeclaration(
                    null,
                    variableName,
                    new TypeRef(null, null, [builtInTypes.Bool.Name])
                    {
                        Metadata = builtInTypes.Bool,
                    },
                    left
                )
                {
                    Metadata = variableMetadata,
                },
                new IfStatement(
                    null,
                    new UnaryExpression(
                        null,
                        UnaryExpressionKind.LogicalNot,
                        new MemberAccessExpression(null, variableName)
                        {
                            AccessKind = MemberAccessKind.Read,
                            Reference = variableMetadata,
                        }
                    )
                    {
                        ReturnTypeMetadata = builtInTypes.Bool,
                    },
                    new BlockStatement(null, [
                        new ExpressionStatement(
                            null,
                            new BinaryExpression(
                                null,
                                BinaryExpressionKind.Assignment,
                                new MemberAccessExpression(null, variableName)
                                {
                                    AccessKind = MemberAccessKind.Write,
                                    Reference = variableMetadata,
                                },
                                right
                            )
                        )
                    ])
                ),
                new ExpressionStatement(
                    null,
                    new MemberAccessExpression(null, variableName)
                    {
                        AccessKind = MemberAccessKind.Read,
                        Reference = variableMetadata,
                    }
                ),
            ]);

            return result;
        }

        if (ReferenceEquals(left, node.Left) && ReferenceEquals(right, node.Right))
            return node;

        return new BinaryExpression(null, node.Kind, left, right)
        {
            ReturnTypeMetadata = node.ReturnTypeMetadata,
        };
    }

    public override ISemanticNode TransformDiscriminatedUnion(DiscriminatedUnion node)
        => node;

    public override ISemanticNode TransformFunctionType(FunctionType node)
        => node;

    public override ISemanticNode TransformGenericType(GenericApplication node)
        => node;

    public override ISemanticNode TransformGenericExpression(GenericExpression node)
        => node;

    public override ISemanticNode TransformInterface(Interface node)
        => node;

    public override ISemanticNode TransformInterfaceProperty(InterfaceProperty node)
        => node;

    public override ISemanticNode TransformInterfaceMethod(InterfaceMethod node)
        => node;

    public override ISemanticNode TransformParameter(Parameter node)
        => node;

    public override ISemanticNode TransformPointer(PointerType node)
        => node;

    public override ISemanticNode TransformTupleType(TupleType node)
        => node;

    public override ISemanticNode TransformTypeNode(TypeRef node)
        => node;
}
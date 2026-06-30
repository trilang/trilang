using Trilang.Metadata;
using Trilang.Semantics;
using Trilang.Semantics.Model;

namespace Trilang.Lower;

internal class ReplaceGettersAndSettersWithMethodCalls : Transformer
{
    private int tempVariableCounter;

    public ReplaceGettersAndSettersWithMethodCalls(ISet<string> directives, BuiltInTypes builtInTypes)
        : base(directives, builtInTypes)
    {
        tempVariableCounter = 0;
    }

    public override ISemanticNode TransformAlias(AliasDeclaration node)
        => node;

    public override ISemanticNode TransformArrayType(ArrayType node)
        => node;

    public override ISemanticNode TransformBinaryExpression(BinaryExpression node)
    {
        var left = (IExpression)node.Left.Transform(this);
        var right = (IExpression)node.Right.Transform(this);

        if (node.Kind == BinaryExpressionKind.Assignment &&
            left is MemberAccessExpression { Reference: PropertyMetadata propertyMetadata } memberAccess)
        {
            if (node.Parent is ExpressionStatement)
            {
                return new CallExpression(
                    null,
                    new MemberAccessExpression(null, memberAccess.Member, propertyMetadata.Setter!.Name)
                    {
                        Reference = propertyMetadata.Setter,
                        AccessKind = MemberAccessKind.Read,
                        SymbolTable = node.SymbolTable,
                        MetadataProvider = node.MetadataProvider,
                    },
                    [right]
                )
                {
                    SymbolTable = node.SymbolTable,
                    MetadataProvider = node.MetadataProvider,
                };
            }

            var name = $"<>_tmp_set{tempVariableCounter++}";
            var variableMetadata = new VariableMetadata(null, name, right.ReturnTypeMetadata!);

            return new ExpressionBlock([
                new VariableDeclaration(
                    null,
                    name,
                    new TypeRef(null, null, [variableMetadata.Type.ToString()!])
                    {
                        Metadata = variableMetadata.Type
                    },
                    right)
                {
                    Metadata = variableMetadata,
                    SymbolTable = node.SymbolTable,
                    MetadataProvider = node.MetadataProvider,
                },
                new ExpressionStatement(
                    null,
                    new CallExpression(
                        null,
                        new MemberAccessExpression(null, memberAccess.Member, propertyMetadata.Setter!.Name)
                        {
                            Reference = propertyMetadata.Setter,
                            AccessKind = MemberAccessKind.Read,
                            SymbolTable = node.SymbolTable,
                            MetadataProvider = node.MetadataProvider,
                        },
                        [
                            new MemberAccessExpression(null, name)
                            {
                                Reference = variableMetadata,
                                AccessKind = MemberAccessKind.Read,
                                SymbolTable = node.SymbolTable,
                                MetadataProvider = node.MetadataProvider,
                            }
                        ]
                    )
                    {
                        SymbolTable = node.SymbolTable,
                        MetadataProvider = node.MetadataProvider,
                    }
                )
                {
                    SymbolTable = node.SymbolTable,
                    MetadataProvider = node.MetadataProvider,
                },
                new ExpressionStatement(
                    null,
                    new MemberAccessExpression(null, name)
                    {
                        Reference = variableMetadata,
                        AccessKind = MemberAccessKind.Read,
                        SymbolTable = node.SymbolTable,
                        MetadataProvider = node.MetadataProvider,
                    }
                )
                {
                    SymbolTable = node.SymbolTable,
                    MetadataProvider = node.MetadataProvider,
                }
            ]);
        }

        if (ReferenceEquals(left, node.Left) && ReferenceEquals(right, node.Right))
            return node;

        return new BinaryExpression(null, node.Kind, left, right)
        {
            ReturnTypeMetadata = node.ReturnTypeMetadata,
            SymbolTable = node.SymbolTable,
            MetadataProvider = node.MetadataProvider,
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

    public override ISemanticNode TransformMemberAccess(MemberAccessExpression node)
    {
        // skip the first member because it can't be property access
        // even local properties are accessed through `this`
        if (node.IsFirstMember)
            return node;

        var member = (IExpression)node.Member.Transform(this);

        if (node is { Reference: PropertyMetadata propertyMetadata, AccessKind: MemberAccessKind.Read })
        {
            return new CallExpression(
                null,
                new MemberAccessExpression(null, member, propertyMetadata.Getter!.Name)
                {
                    Reference = propertyMetadata.Getter,
                    AccessKind = MemberAccessKind.Read,
                    SymbolTable = node.SymbolTable,
                    MetadataProvider = node.MetadataProvider,
                },
                []
            )
            {
                SymbolTable = node.SymbolTable,
                MetadataProvider = node.MetadataProvider,
            };
        }

        if (ReferenceEquals(member, node.Member))
            return node;

        return new MemberAccessExpression(null, member, node.Name)
        {
            Reference = node.Reference,
            AccessKind = node.AccessKind,
            SymbolTable = node.SymbolTable,
            MetadataProvider = node.MetadataProvider,
        };
    }

    public override ISemanticNode TransformParameter(Parameter node)
        => node;

    public override ISemanticNode TransformPointer(PointerType node)
        => node;

    public override ISemanticNode TransformTupleType(TupleType node)
        => node;

    public override ISemanticNode TransformTypeNode(TypeRef node)
        => node;
}
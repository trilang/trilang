using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;

namespace Trilang.IntermediateRepresentation;

internal class IrDiscoveryPhase : Visitor
{
    private readonly Dictionary<IFunctionMetadata, BlockStatementNode> functionsToGenerate;

    public IrDiscoveryPhase()
        => functionsToGenerate = [];

    public IReadOnlyDictionary<IFunctionMetadata, BlockStatementNode> Discover(
        IEnumerable<ITypeMetadata> types,
        IReadOnlyList<SyntaxTree> syntaxTrees)
    {
        foreach (var tree in syntaxTrees)
            VisitTree(tree);

        foreach (var metadata in types)
        {
            var properties = metadata switch
            {
                TupleMetadata tuple => tuple.Properties,
                TypeArrayMetadata array => array.Properties,
                TypeMetadata type => type.Properties,
                _ => [],
            };

            foreach (var property in properties)
            {
                var declaringType = property.DeclaringType;
                var fieldMetadata = declaringType.GetMember($"<>_{property.Name}") as FieldMetadata ??
                                    throw new Exception("Internal error: field metadata is null.");

                if (property.Getter is not null && !functionsToGenerate.ContainsKey(property.Getter))
                {
                    functionsToGenerate.Add(
                        property.Getter,
                        new BlockStatementNode([
                            new ReturnStatementNode(
                                new MemberAccessExpressionNode(
                                    new MemberAccessExpressionNode(MemberAccessExpressionNode.This)
                                    {
                                        Reference = new ParameterMetadata(MemberAccessExpressionNode.This, declaringType),
                                        AccessKind = PropertyAccessKind.Read,
                                    },
                                    fieldMetadata.Name
                                )
                                {
                                    Reference = fieldMetadata,
                                    AccessKind = PropertyAccessKind.Read,
                                }
                            ),
                        ])
                    );
                }

                if (property.Setter is not null && !functionsToGenerate.ContainsKey(property.Setter))
                {
                    var valueParameter = property.Setter.Parameters
                        .First(x => x.Name == MemberAccessExpressionNode.Value);
                    functionsToGenerate.Add(
                        property.Setter,
                        new BlockStatementNode([
                            new ExpressionStatementNode(
                                new BinaryExpressionNode(
                                    BinaryExpressionKind.Assignment,
                                    new MemberAccessExpressionNode(
                                        new MemberAccessExpressionNode(MemberAccessExpressionNode.This)
                                        {
                                            Reference = new ParameterMetadata(MemberAccessExpressionNode.This, declaringType),
                                            AccessKind = PropertyAccessKind.Read,
                                        },
                                        fieldMetadata.Name
                                    )
                                    {
                                        Reference = fieldMetadata,
                                        AccessKind = PropertyAccessKind.Write,
                                    },
                                    new MemberAccessExpressionNode(valueParameter.Name)
                                    {
                                        Reference = valueParameter,
                                        AccessKind = PropertyAccessKind.Read,
                                    }
                                )
                                {
                                    ReturnTypeMetadata = property.Type,
                                }
                            )
                        ])
                    );
                }
            }
        }

        return functionsToGenerate;
    }

    protected override void VisitFunctionEnter(FunctionDeclarationNode node)
        => functionsToGenerate.Add(node.Metadata!, node.Body);

    protected override void VisitGetterEnter(PropertyGetterNode node)
        => functionsToGenerate.Add(node.Metadata!, node.Body!);

    protected override void VisitSetterEnter(PropertySetterNode node)
        => functionsToGenerate.Add(node.Metadata!, node.Body!);

    protected override void VisitMethodEnter(MethodDeclarationNode node)
        => functionsToGenerate.Add(node.Metadata!, node.Body);

    protected override void VisitConstructorEnter(ConstructorDeclarationNode node)
        => functionsToGenerate.Add(node.Metadata!, node.Body);
}
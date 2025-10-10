using Trilang.Metadata;
using Trilang.Semantics;
using Trilang.Semantics.Model;

namespace Trilang.IntermediateRepresentation;

internal class IrDiscoveryPhase : Visitor
{
    private readonly Dictionary<IFunctionMetadata, BlockStatement> functionsToGenerate;

    public IrDiscoveryPhase()
        => functionsToGenerate = [];

    public IReadOnlyDictionary<IFunctionMetadata, BlockStatement> Discover(
        IEnumerable<ITypeMetadata> types,
        IEnumerable<SemanticTree> syntaxTrees)
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
                        new BlockStatement(null, [
                            new ReturnStatement(
                                null,
                                new MemberAccessExpression(
                                    null,
                                    new MemberAccessExpression(null, MemberAccessExpression.This)
                                    {
                                        Reference = new ParameterMetadata(null,MemberAccessExpression.This, declaringType),
                                        AccessKind = MemberAccessKind.Read,
                                    },
                                    fieldMetadata.Name
                                )
                                {
                                    Reference = fieldMetadata,
                                    AccessKind = MemberAccessKind.Read,
                                }
                            ),
                        ])
                    );
                }

                if (property.Setter is not null && !functionsToGenerate.ContainsKey(property.Setter))
                {
                    var valueParameter = property.Setter.Parameters
                        .First(x => x.Name == MemberAccessExpression.Value);
                    functionsToGenerate.Add(
                        property.Setter,
                        new BlockStatement(null, [
                            new ExpressionStatement(
                                null,
                                new BinaryExpression(
                                    null,
                                    BinaryExpressionKind.Assignment,
                                    new MemberAccessExpression(
                                        null,
                                        new MemberAccessExpression(null, MemberAccessExpression.This)
                                        {
                                            Reference = new ParameterMetadata(null, MemberAccessExpression.This, declaringType),
                                            AccessKind = MemberAccessKind.Read,
                                        },
                                        fieldMetadata.Name
                                    )
                                    {
                                        Reference = fieldMetadata,
                                        AccessKind = MemberAccessKind.Write,
                                    },
                                    new MemberAccessExpression(null, valueParameter.Name)
                                    {
                                        Reference = valueParameter,
                                        AccessKind = MemberAccessKind.Read,
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

    protected override void VisitFunctionEnter(FunctionDeclaration node)
        => functionsToGenerate.Add(node.Metadata!, node.Body);

    protected override void VisitGetterEnter(PropertyGetter node)
        => functionsToGenerate.Add(node.Metadata!, node.Body!);

    protected override void VisitSetterEnter(PropertySetter node)
        => functionsToGenerate.Add(node.Metadata!, node.Body!);

    protected override void VisitMethodEnter(MethodDeclaration node)
        => functionsToGenerate.Add(node.Metadata!, node.Body);

    protected override void VisitConstructorEnter(ConstructorDeclaration node)
        => functionsToGenerate.Add(node.Metadata!, node.Body);
}
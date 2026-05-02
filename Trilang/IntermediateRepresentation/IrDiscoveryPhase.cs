using Trilang.Metadata;
using Trilang.Semantics;
using Trilang.Semantics.Model;

namespace Trilang.IntermediateRepresentation;

internal class IrDiscoveryPhase : Visitor
{
    private readonly Dictionary<IMetadata, BlockStatement> functionsToGenerate;

    public IrDiscoveryPhase(ISet<string> directives) : base(directives)
        => functionsToGenerate = [];

    public IReadOnlyDictionary<IMetadata, BlockStatement> Discover(
        IEnumerable<SemanticTree> syntaxTrees,
        IEnumerable<ITypeMetadata> types)
    {
        foreach (var tree in syntaxTrees)
            VisitTree(tree);

        foreach (var metadata in types)
        {
            var properties = metadata switch
            {
                TupleMetadata tuple => tuple.Properties,
                ArrayMetadata array => array.Properties,
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
                                        Reference = new ParameterMetadata(
                                            null,
                                            MemberAccessExpression.This,
                                            declaringType),
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
                                            Reference = new ParameterMetadata(
                                                null,
                                                MemberAccessExpression.This,
                                                declaringType),
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

    public override void VisitFunction(FunctionDeclaration node)
    {
        functionsToGenerate.Add(node.Metadata!, node.Body);

        base.VisitFunction(node);
    }

    public override void VisitGetter(PropertyGetter node)
    {
        functionsToGenerate.Add(node.Metadata!, node.Body!);

        base.VisitGetter(node);
    }

    public override void VisitSetter(PropertySetter node)
    {
        functionsToGenerate.Add(node.Metadata!, node.Body!);

        base.VisitSetter(node);
    }

    public override void VisitMethod(MethodDeclaration node)
    {
        functionsToGenerate.Add(node.Metadata!, node.Body);

        base.VisitMethod(node);
    }

    public override void VisitConstructor(ConstructorDeclaration node)
    {
        functionsToGenerate.Add(node.Metadata!, node.Body);

        base.VisitConstructor(node);
    }
}
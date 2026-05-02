using Trilang.Metadata;
using Trilang.Semantics.Model;
using static Trilang.Semantics.Model.BinaryExpressionKind;
using static Tri.Tests.Factory;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Lowering;

public class ReplaceCompoundAssignmentsTests
{
    [TestCase("+=", Addition)]
    [TestCase("-=", Subtraction)]
    [TestCase("*=", Multiplication)]
    [TestCase("/=", Division)]
    [TestCase("%=", Modulus)]
    [TestCase("&=", BitwiseAnd)]
    [TestCase("|=", BitwiseOr)]
    [TestCase("^=", BitwiseXor)]
    public void ReplaceCompoundAssignmentsTest(string op, BinaryExpressionKind kind)
    {
        var file = CreateFile(
            $$"""
              namespace Test1;

              public test(x: i32): void {
                  x {{op}} 1;
              }
              """);
        var (tree, diagnostics, _) = Lower(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var parameterMetadata = new ParameterMetadata(null, "x", builtInTypes.I32);
        var expected = new SemanticTree(
            file,
            null,
            new Namespace(null, ["Test1"]),
            [],
            [
                new FunctionDeclaration(
                    null,
                    AccessModifier.Public,
                    "test",
                    [
                        new Parameter(null, "x", new TypeRef(null, null, ["i32"]) { Metadata = builtInTypes.I32 })
                        {
                            Metadata = parameterMetadata,
                        }
                    ],
                    new TypeRef(null, null, ["void"]) { Metadata = builtInTypes.Void },
                    new BlockStatement(null, [
                        new ExpressionStatement(
                            null,
                            new BinaryExpression(
                                null,
                                Assignment,
                                new MemberAccessExpression(null, "x")
                                {
                                    Reference = parameterMetadata,
                                    AccessKind = MemberAccessKind.Write,
                                },
                                new BinaryExpression(
                                    null,
                                    kind,
                                    new MemberAccessExpression(null, "x")
                                    {
                                        Reference = parameterMetadata,
                                        AccessKind = MemberAccessKind.Read,
                                    },
                                    new LiteralExpression(null, LiteralExpressionKind.Integer, 1)
                                    {
                                        ReturnTypeMetadata = builtInTypes.I32,
                                    }
                                )
                                {
                                    ReturnTypeMetadata = builtInTypes.I32,
                                }
                            )
                            {
                                ReturnTypeMetadata = builtInTypes.I32,
                            }
                        ),
                        new ReturnStatement(null)
                    ])
                )
                {
                    Metadata = new FunctionMetadata(
                        null,
                        AccessModifierMetadata.Public,
                        "test",
                        [parameterMetadata],
                        CreateFunctionType([builtInTypes.I32], builtInTypes.Void, rootNamespace)
                    )
                    {
                        Namespace = rootNamespace,
                    }
                }
            ]);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }
}
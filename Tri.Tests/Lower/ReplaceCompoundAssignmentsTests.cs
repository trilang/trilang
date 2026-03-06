using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using static Trilang.Semantics.Model.BinaryExpressionKind;
using static Tri.Tests.Factory;

namespace Tri.Tests.Lower;

public class ReplaceCompoundAssignmentsTests
{
    private static readonly SourceFile file = new SourceFile("test.tri");

    private static SemanticTree Parse(string code)
    {
        var diagnostics = new DiagnosticCollection();

        var lexer = new Lexer();
        var lexerOptions = new LexerOptions(new LexerDiagnosticReporter(diagnostics, file));
        var tokens = lexer.Tokenize(code, lexerOptions);

        var parser = new Parser();
        var parserOptions = new ParserOptions(file, new ParserDiagnosticReporter(diagnostics, file));
        var tree = parser.Parse(tokens, parserOptions);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        Assert.That(diagnostics.Diagnostics, Is.Empty);

        var semanticTree = semanticTrees.Single();

        var lowering = new Lowering(builtInTypes);
        lowering.Lower(semanticTree, LoweringOptions.Default);

        return semanticTree;
    }

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
        var tree = Parse(
            $$"""
              public test(x: i32): void {
                  x {{op}} 1;
              }
              """);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var parameterMetadata = new ParameterMetadata(null, "x", builtInTypes.I32);
        var expected = new SemanticTree(file, null, null, [], [
            new FunctionDeclaration(
                null,
                AccessModifier.Public,
                "test",
                [
                    new Parameter(null, "x", new TypeRef(null, "i32") { Metadata = builtInTypes.I32 })
                    {
                        Metadata = parameterMetadata,
                    }
                ],
                new TypeRef(null, "void") { Metadata = builtInTypes.Void },
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
                    )
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

        Assert.That(tree, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }
}
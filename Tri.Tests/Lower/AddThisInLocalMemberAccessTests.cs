using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Lower;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using static Tri.Tests.Factory;

namespace Tri.Tests.Lower;

public class AddThisInLocalMemberAccessTests
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

    [Test]
    public void AddThisBeforePropertyTest()
    {
        const string code =
            """
            public type Test {
                count: i32;

                public getCount(): i32 {
                    return count;
                }
            }
            """;
        var tree = Parse(code);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var typeMetadata = new TypeMetadata(null, "Test")
        {
            Namespace = rootNamespace,
        };
        var propertyMetadata = CreatePropertyMetadata(typeMetadata,
            "count",
            builtInTypes.I32);

        var expected = new CallExpression(
            null,
            new MemberAccessExpression(
                null,
                new MemberAccessExpression(null, MemberAccessExpression.This)
                {
                    Reference = new ParameterMetadata(null, MemberAccessExpression.This, typeMetadata),
                    AccessKind = MemberAccessKind.Read,
                },
                "<>_get_count")
            {
                Reference = propertyMetadata.Getter,
                AccessKind = MemberAccessKind.Read,
            },
            []
        );

        var method = tree.Find<MethodDeclaration>();
        var returnStatement = method?.Body.Find<ReturnStatement>();
        Assert.That(returnStatement, Is.Not.Null);
        Assert.That(returnStatement.Expression, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void AddThisBeforeMethodTest()
    {
        const string code =
            """
            public type Test {
                public print(): void { }

                public test(): void {
                    print();
                }
            }
            """;
        var tree = Parse(code);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = NamespaceMetadata.CreateRoot(builtInTypes);
        var typeMetadata = new TypeMetadata(null, "Test")
        {
            Namespace = rootNamespace,
        };
        var methodMetadata = new MethodMetadata(
            null,
            typeMetadata,
            AccessModifierMetadata.Public,
            false,
            "print",
            [],
            CreateFunctionType([], builtInTypes.Void, rootNamespace));

        var expected = new MemberAccessExpression(
            null,
            new MemberAccessExpression(null, MemberAccessExpression.This)
            {
                Reference = new ParameterMetadata(null, MemberAccessExpression.This, typeMetadata),
                AccessKind = MemberAccessKind.Read,
            },
            "print")
        {
            Reference = methodMetadata,
            AccessKind = MemberAccessKind.Read,
        };

        var returnStatement = tree.Find<CallExpression>();
        Assert.That(returnStatement, Is.Not.Null);
        Assert.That(returnStatement.Member, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }
}
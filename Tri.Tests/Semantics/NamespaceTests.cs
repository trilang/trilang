using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using static Tri.Tests.Factory;

namespace Tri.Tests.Semantics;

public class NamespaceTests
{
    private static SyntaxTree Parse(DiagnosticCollection diagnostics, string filePath, string code)
    {
        var file = new SourceFile(filePath);
        var lexer = new Lexer();
        var lexerOptions = new LexerOptions(new LexerDiagnosticReporter(diagnostics, file));
        var tokens = lexer.Tokenize(code, lexerOptions);

        var parser = new Parser();
        var parserOptions = new ParserOptions(file, new ParserDiagnosticReporter(diagnostics, file));
        var tree = parser.Parse(tokens, parserOptions);

        return tree;
    }

    [Test]
    public void DefineTypeInNamespaceTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;

            public type MyType = i32;
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootProvider, _) = semantic.Analyze(
            [file1],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var ns1Provider = rootProvider.FindNamespace(["NS1"])!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(
            rootProvider.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType" }));
        Assert.That(
            ns1Provider.Types,
            Has.One.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType" }));
    }

    [Test]
    public void ImportTypeFromNamespaceTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;

            public type MyType1 = i32;
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace NS2;

            use NS1;

            public type MyType2 = MyType1;
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootProvider, _) = semantic.Analyze(
            [file1, file2],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var ns1Provider = rootProvider.FindNamespace(["NS1"])!;
        var ns2Provider = rootProvider.FindNamespace(["NS2"])!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(
            rootProvider.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType1" }));
        Assert.That(
            rootProvider.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType2" }));
        Assert.That(
            ns1Provider.Types,
            Has.One.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType1" }));
        Assert.That(
            ns2Provider.Types,
            Has.One.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType2" }));
    }

    [Test]
    public void UseUnknownNamespaceTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;

            public type MyType1 = i32;
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace NS2;

            use NS1.Something;

            public main(): void {}
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [file1, file2],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0028UnknownNamespace,
            DiagnosticSeverity.Error,
            new SourceLocation(
                new SourceFile("file2.tri"),
                new SourceSpan(new SourcePosition(16, 3, 1), new SourcePosition(34, 3, 19))),
            "Unknown namespace: NS1.Something.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void CrossUseTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;
            use NS2;

            public type MyType1 = i32;
            public type Test1 = MyType2;
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace NS2;
            use NS1;

            public type MyType2 = i32;
            public type Test2 = MyType1;
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootProvider, _) = semantic.Analyze(
            [file1, file2],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var ns1Provider = rootProvider.FindNamespace(["NS1"])!;
        var ns2Provider = rootProvider.FindNamespace(["NS2"])!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(
            rootProvider.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType1" }));
        Assert.That(
            rootProvider.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "Test1" }));
        Assert.That(
            rootProvider.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType2" }));
        Assert.That(
            rootProvider.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "Test2" }));
        Assert.That(
            ns1Provider.Types,
            Has.One.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType1" }));
        Assert.That(
            ns1Provider.Types,
            Has.One.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "Test1" }));
        Assert.That(
            ns2Provider.Types,
            Has.One.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType2" }));
        Assert.That(
            ns2Provider.Types,
            Has.One.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "Test2" }));
    }

    [Test]
    public void NestedNamespacesTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1.NS2.NS3;

            public type MyType = i32;
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace Test;

            use NS1.NS2.NS3;

            public type MyType2 = MyType;
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootProvider, _) = semantic.Analyze(
            [file1, file2],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var ns1Ns2Ns3Provider = rootProvider.FindNamespace(["NS1", "NS2", "NS3"])!;
        var testProvider = rootProvider.FindNamespace(["Test"])!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(
            rootProvider.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType" }));
        Assert.That(
            rootProvider.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType2" }));
        Assert.That(
            ns1Ns2Ns3Provider.Types,
            Has.One.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType" }));
        Assert.That(
            testProvider.Types,
            Has.One.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType2" }));
    }

    [Test]
    public void IntermediateNamespaceTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1.NS2.NS3;

            public type MyType = i32;
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace Test;

            use NS1.NS2.NS3;

            public type MyType2 = MyType;
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootProvider, _) = semantic.Analyze(
            [file1, file2],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var ns1Provider = rootProvider.FindNamespace(["NS1"])!;
        var ns1Ns2Provider = rootProvider.FindNamespace(["NS1", "NS2"])!;
        var ns1Ns2Ns3Provider = rootProvider.FindNamespace(["NS1", "NS2", "NS3"])!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(ns1Provider, Is.Not.Null);
        Assert.That(ns1Ns2Provider, Is.Not.Null);
        Assert.That(ns1Ns2Ns3Provider, Is.Not.Null);
    }

    [Test]
    public void UseTypeFromParentNamespaceWithoutImportTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;

            public type MyType = i32;
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace NS1.NS2.NS3;

            use NS1;

            public type MyType2 = MyType;
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootProvider, _) = semantic.Analyze(
            [file1, file2],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var ns1Provider = rootProvider.FindNamespace(["NS1"])!;
        var ns1Ns2Ns3Provider = rootProvider.FindNamespace(["NS1", "NS2", "NS3"])!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(
            rootProvider.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType" }));
        Assert.That(
            rootProvider.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType2" }));
        Assert.That(
            ns1Provider.Types,
            Has.One.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType" }));
        Assert.That(
            ns1Provider.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType2" }));
        Assert.That(
            ns1Ns2Ns3Provider.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType" }));
        Assert.That(
            ns1Ns2Ns3Provider.Types,
            Has.One.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType2" }));
    }

    [Test]
    public void DuplicateTypeInDifferentNamespaceTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;

            public type MyType = i32;
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace NS2;

            public type MyType = i32;
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootProvider, _) = semantic.Analyze(
            [file1, file2],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var ns1Provider = rootProvider.FindNamespace(["NS1"])!;
        var ns2Provider = rootProvider.FindNamespace(["NS2"])!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(
            rootProvider.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType" }));
        Assert.That(
            ns1Provider.Types,
            Has.One.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType" }));
        Assert.That(
            ns2Provider.Types,
            Has.One.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType" }));
    }

    [Test]
    public void UseDuplicateTypeFromDifferentNamespaceTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;

            public type MyType1 = i32;
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace NS2;

            public type MyType1 = i32;
            """);
        var file3 = Parse(
            diagnostics,
            "file3.tri",
            """
            namespace Test;

            use NS1;
            use NS2;

            public type MyType2 = MyType1;
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [file1, file2, file3],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var testNs = rootNamespace.FindNamespace(["Test"])!;
        var myType2 = (AliasMetadata)testNs.FindType("MyType2")!;

        var diagnostic = new Diagnostic(
            DiagnosticId.S0027MultipleMembersFound,
            DiagnosticSeverity.Error,
            new SourceLocation(
                new SourceFile("file3.tri"),
                new SourceSpan(new SourcePosition(58, 6, 23), new SourcePosition(65, 6, 30))),
            """
            Multiple members found:
            MyType1: SourceLocation { File = file1.tri, Span = (16:3:1) - (42:3:27) }
            MyType1: SourceLocation { File = file2.tri, Span = (16:3:1) - (42:3:27) }.
            """);

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
        Assert.That(
            myType2.Type,
            Is.EqualTo(TypeMetadata.Invalid("MyType1")).Using(new MetadataComparer()));
    }

    [Test]
    public void DefineDuplicateNamespaceTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;

            public type MyType1 = i32;
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace NS1;

            public type MyType2 = i32;
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootProvider, _) = semantic.Analyze(
            [file1, file2],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var ns1Provider = rootProvider.FindNamespace(["NS1"])!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(
            rootProvider.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType1" }));
        Assert.That(
            rootProvider.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType2" }));
        Assert.That(
            ns1Provider.Types,
            Has.One.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType1" }));
        Assert.That(
            ns1Provider.Types,
            Has.One.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType2" }));
    }

    [Test]
    public void AnonymousTypesHoistingTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;

            public type MyType1 = i32 | bool;
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootProvider, _) = semantic.Analyze(
            [file1],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var ns1Provider = rootProvider.FindNamespace(["NS1"])!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(
            rootProvider.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType1" }));
        Assert.That(
            rootProvider.Types,
            Has.One.Matches<ITypeMetadata>(x => x is DiscriminatedUnionMetadata));
        Assert.That(
            ns1Provider.Types,
            Has.One.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType1" }));
        Assert.That(
            ns1Provider.Types,
            Has.None.Matches<ITypeMetadata>(x => x is DiscriminatedUnionMetadata));
    }

    [Test]
    public void GenericProviderTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;

            public type List<T> {
                public getItem(index: i32): T | null {
                    return (T | null)null;
                }
            }
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace NS2;

            use NS1;

            public main(): void {
                var list: List<i32> = new List<i32>();
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootProvider, _) = semantic.Analyze(
            [file1, file2],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var t = new TypeArgumentMetadata(null, "T");
        var ns1Provider = rootProvider.FindNamespace(["NS1"])!;
        var ns2Provider = rootProvider.FindNamespace(["NS2"])!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(rootProvider.Types, Does.Not.Contain(t));
        Assert.That(ns1Provider.Types, Does.Not.Contain(t));
        Assert.That(ns2Provider.Types, Does.Not.Contain(t));
    }

    [Test]
    public void FunctionOverloadTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;

            public type MyType = i32;
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace NS2;

            public type MyType = i32;
            """);
        var file3 = Parse(
            diagnostics,
            "file3.tri",
            """
            namespace Test1;

            use NS1;

            public test(t: MyType): void { }
            """);
        var file4 = Parse(
            diagnostics,
            "file4.tri",
            """
            namespace Test2;

            use NS2;

            public test(t: MyType): void { }
            """);
        var file5 = Parse(
            diagnostics,
            "file5.tri",
            """
            namespace Test3;

            use NS1;
            use Test1;

            public main(t: MyType): void {
                test(t);
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, _, _) = semantic.Analyze(
            [file1, file2, file3, file4, file5],
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                new SemanticDiagnosticReporter(diagnostics),
                new BuiltInTypes()));

        var tree = semanticTrees[^1];
        var call = tree.Find<CallExpression>()!;

        var builtInTypes = new BuiltInTypes();
        var rootNs = NamespaceMetadata.CreateRoot(builtInTypes);
        var ns1Ns = rootNs.CreateChild(["NS1"]);
        var myType = new AliasMetadata(null, "MyType", [], builtInTypes.I32)
        {
            Namespace = ns1Ns,
        };
        var expected = CreateFunctionType([myType], builtInTypes.Void, rootNs);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(call.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void FunctionMultipleOverloadTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;

            public type MyType { }
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace NS2;

            public type MyType { }
            """);
        var file3 = Parse(
            diagnostics,
            "file3.tri",
            """
            namespace Test1;

            use NS1;

            public test(t: MyType): void { }
            """);
        var file4 = Parse(
            diagnostics,
            "file4.tri",
            """
            namespace Test2;

            use NS2;

            public test(t: MyType): void { }
            """);
        var file5 = Parse(
            diagnostics,
            "file5.tri",
            """
            namespace Test3;

            use NS1;
            use Test1;
            use Test2;

            public main(t: MyType): void {
                test(t);
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [file1, file2, file3, file4, file5],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void FunctionOverloadIncorrectParameterTypeTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;

            public type MyType { }
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace NS2;

            public type MyType { }
            """);
        var file3 = Parse(
            diagnostics,
            "file3.tri",
            """
            namespace Test1;

            use NS1;

            public test(t: MyType): void { }
            """);
        var file4 = Parse(
            diagnostics,
            "file4.tri",
            """
            namespace Test2;

            use NS2;

            public test(t: MyType): void { }
            """);
        var file5 = Parse(
            diagnostics,
            "file5.tri",
            """
            namespace Test3;

            use NS2;
            use Test1;

            public main(t: MyType): void {
                test(t);
            }
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [file1, file2, file3, file4, file5],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0005TypeMismatch,
            DiagnosticSeverity.Error,
            new SourceLocation(
                new SourceFile("file5.tri"),
                new SourceSpan(new SourcePosition(79, 7, 10), new SourcePosition(80, 7, 11))),
            "Type mismatch: expected 'MyType', got 'MyType'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void FunctionGroupInDifferentNamespacesTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;

            public type MyType { }
            public test(t: MyType): void { }
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace NS2;

            public type MyType { }
            public test(t: MyType): void { }
            """);
        var file3 = Parse(
            diagnostics,
            "file3.tri",
            """
            namespace Test3;

            use NS1;

            public main(): void {
                var callback: (MyType) => void = test;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, rootNamespace, _) = semantic.Analyze(
            [file1, file2, file3],
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                new SemanticDiagnosticReporter(diagnostics),
                new BuiltInTypes()));

        var tree = semanticTrees[^1];
        var memberAccessExpression = tree.Find<MemberAccessExpression>()!;

        var ns1Provider = rootNamespace.FindNamespace(["NS1"])!;
        var testFunction = ns1Provider.FindFunction("test").First();

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(
            memberAccessExpression.Reference,
            Is.EqualTo(testFunction).Using(new MetadataComparer()));
    }

    [Test]
    public void ResolveFullyQualifiedNameTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;

            public type MyType = i32;
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace Test1;

            public test(obj: NS1.MyType): void { }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [file1, file2],
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                new SemanticDiagnosticReporter(diagnostics),
                new BuiltInTypes()));

        var test1Provider = rootNamespace.FindNamespace(["Test1"])!;
        var testFunction = test1Provider.FindFunction("test").First();

        var builtInTypes = new BuiltInTypes();
        var rootNs = NamespaceMetadata.CreateRoot(builtInTypes);
        var ns1Ns = rootNs.CreateChild(["NS1"]);
        var myType = new AliasMetadata(null, "MyType", [], builtInTypes.I32)
        {
            Namespace = ns1Ns,
        };

        var test1Ns = rootNs.CreateChild(["Test1"]);
        var expected = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [new ParameterMetadata(null, "obj", myType)],
            CreateFunctionType([myType], builtInTypes.Void, rootNs))
        {
            Namespace = test1Ns,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(testFunction, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void ResolveFullyQualifiedNameForStaticTypeTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;

            public type MyType {
                public static test(): void { }
            }
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace Test1;

            public test(): void {
                NS1.MyType.test();
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, _, rootNamespace, _) = semantic.Analyze(
            [file1, file2],
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                new SemanticDiagnosticReporter(diagnostics),
                new BuiltInTypes()));

        var tree = semanticTrees[^1];
        var member = tree.Find<MemberAccessExpression>(x => x.Name == "test")!;

        var ns1 = rootNamespace.FindNamespace(["NS1"])!;
        var myType = (TypeMetadata)ns1.FindType("MyType")!;
        var method = myType.Methods[0];

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(member.Reference, Is.EqualTo(method).Using(new MetadataComparer()));
    }

    [Test]
    public void ResolveFullyQualifiedNameForStaticTypeMissingNamespaceTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace Test1;

            public test(): void {
                NS1.MyType.test();
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [file1],
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                new SemanticDiagnosticReporter(diagnostics),
                new BuiltInTypes()));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0008UnknownMember,
            DiagnosticSeverity.Error,
            new SourceLocation(
                new SourceFile("file1.tri"),
                new SourceSpan(new SourcePosition(44, 4, 5), new SourcePosition(47, 4, 8))),
            "Unknown symbol: 'NS1'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ResolveFullyQualifiedNameForStaticMissingTypeTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;

            public type MyType2 = i32;
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace Test1;

            public test(): void {
                NS1.MyType.test();
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [file1, file2],
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                new SemanticDiagnosticReporter(diagnostics),
                new BuiltInTypes()));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0008UnknownMember,
            DiagnosticSeverity.Error,
            new SourceLocation(
                new SourceFile("file2.tri"),
                new SourceSpan(new SourcePosition(44, 4, 5), new SourcePosition(54, 4, 15))),
            "Unknown symbol: 'MyType'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ArrayWithSameTypesFromDifferentNamespacesTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;

            public type MyType { }
            public type A = MyType[];
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace NS2;

            public type MyType { }
            public type A = MyType[];
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [file1, file2],
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                new SemanticDiagnosticReporter(diagnostics),
                new BuiltInTypes()));

        var ns1 = rootNamespace.FindNamespace(["NS1"])!;
        var ns2 = rootNamespace.FindNamespace(["NS2"])!;
        var alias1 = (AliasMetadata)ns1.FindType("A")!;
        var alias2 = (AliasMetadata)ns2.FindType("A")!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(alias1.Type, Is.Not.EqualTo(alias2.Type));
        Assert.That(
            rootNamespace.Types,
            Has.Exactly(2).Matches<ITypeMetadata>(x => x is ArrayMetadata));
    }

    [Test]
    public void DuWithSameTypesFromDifferentNamespacesTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;

            public type MyType { }
            public type DU = MyType | null;
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace NS2;

            public type MyType { }
            public type DU = MyType | null;
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [file1, file2],
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                new SemanticDiagnosticReporter(diagnostics),
                new BuiltInTypes()));

        var ns1 = rootNamespace.FindNamespace(["NS1"])!;
        var ns2 = rootNamespace.FindNamespace(["NS2"])!;
        var alias1 = (AliasMetadata)ns1.FindType("DU")!;
        var alias2 = (AliasMetadata)ns2.FindType("DU")!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(alias1.Type, Is.Not.EqualTo(alias2.Type));
        Assert.That(
            rootNamespace.Types,
            Has.Exactly(3).Matches<ITypeMetadata>(x => x is DiscriminatedUnionMetadata));
    }

    [Test]
    public void FunctionTypeWithSameTypesFromDifferentNamespacesTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;

            public type MyType { }
            public type FT = () => MyType;
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace NS2;

            public type MyType { }
            public type FT = () => MyType;
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [file1, file2],
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                new SemanticDiagnosticReporter(diagnostics),
                new BuiltInTypes()));

        var ns1 = rootNamespace.FindNamespace(["NS1"])!;
        var ns2 = rootNamespace.FindNamespace(["NS2"])!;
        var alias1 = (AliasMetadata)ns1.FindType("FT")!;
        var alias2 = (AliasMetadata)ns2.FindType("FT")!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(alias1.Type, Is.Not.EqualTo(alias2.Type));
        Assert.That(
            rootNamespace.Types,
            Has.Exactly(3).Matches<ITypeMetadata>(x => x is FunctionTypeMetadata));
    }

    [Test]
    public void GenericWithSameTypesFromDifferentNamespacesTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS0;

            public type Generic<T> = T;
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace NS1;

            use NS0;

            public type MyType { }
            public type Closed = Generic<MyType>;
            """);
        var file3 = Parse(
            diagnostics,
            "file3.tri",
            """
            namespace NS2;

            use NS0;

            public type MyType { }
            public type Closed = Generic<MyType>;
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [file1, file2, file3],
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                new SemanticDiagnosticReporter(diagnostics),
                new BuiltInTypes()));

        var ns1 = rootNamespace.FindNamespace(["NS1"])!;
        var ns2 = rootNamespace.FindNamespace(["NS2"])!;
        var alias1 = (AliasMetadata)ns1.FindType("Closed")!;
        var alias2 = (AliasMetadata)ns2.FindType("Closed")!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(alias1.Type, Is.Not.EqualTo(alias2.Type));
        Assert.That(
            rootNamespace.Types,
            Has.Exactly(2).Matches<ITypeMetadata>(x => x is GenericApplicationMetadata));
    }

    [Test]
    public void InterfaceWithSameTypesFromDifferentNamespacesTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;

            public type MyType { }
            public type Interface1 = {
                obj: MyType;
            }
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace NS2;

            public type MyType { }
            public type Interface1 = {
                obj: MyType;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [file1, file2],
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                new SemanticDiagnosticReporter(diagnostics),
                new BuiltInTypes()));

        var ns1 = rootNamespace.FindNamespace(["NS1"])!;
        var ns2 = rootNamespace.FindNamespace(["NS2"])!;
        var alias1 = (AliasMetadata)ns1.FindType("Interface1")!;
        var alias2 = (AliasMetadata)ns2.FindType("Interface1")!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(alias1.Type, Is.Not.EqualTo(alias2.Type));
        Assert.That(
            rootNamespace.Types,
            Has.Exactly(3).Matches<ITypeMetadata>(x => x is InterfaceMetadata));
    }

    [Test]
    public void TupleWithSameTypesFromDifferentNamespacesTest()
    {
        var diagnostics = new DiagnosticCollection();
        var file1 = Parse(
            diagnostics,
            "file1.tri",
            """
            namespace NS1;

            public type MyType { }
            public type T = (MyType, i32);
            """);
        var file2 = Parse(
            diagnostics,
            "file2.tri",
            """
            namespace NS2;

            public type MyType { }
            public type T = (MyType, i32);
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, rootNamespace, _) = semantic.Analyze(
            [file1, file2],
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                new SemanticDiagnosticReporter(diagnostics),
                new BuiltInTypes()));

        var ns1 = rootNamespace.FindNamespace(["NS1"])!;
        var ns2 = rootNamespace.FindNamespace(["NS2"])!;
        var alias1 = (AliasMetadata)ns1.FindType("T")!;
        var alias2 = (AliasMetadata)ns2.FindType("T")!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(alias1.Type, Is.Not.EqualTo(alias2.Type));
        Assert.That(
            rootNamespace.Types,
            Has.Exactly(2).Matches<ITypeMetadata>(x => x is TupleMetadata));
    }
}
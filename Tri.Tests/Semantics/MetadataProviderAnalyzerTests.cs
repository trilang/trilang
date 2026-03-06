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

public class MetadataProviderAnalyzerTests
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
        var expected = new AliasMetadata(null, "MyType", [], builtInTypes.I32)
        {
            Namespace = ns1Provider,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(rootProvider.Types, Does.Not.Contain(expected));
        Assert.That(ns1Provider.Types, Does.Contain(expected));
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
        var myType1 = new AliasMetadata(null, "MyType1", [], builtInTypes.I32)
        {
            Namespace = ns1Provider,
        };
        var myType2 = new AliasMetadata(null, "MyType2", [], myType1)
        {
            Namespace = ns2Provider,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(rootProvider.Types, Does.Not.Contain(myType1));
        Assert.That(rootProvider.Types, Does.Not.Contain(myType2));
        Assert.That(ns1Provider.Types, Does.Contain(myType1));
        Assert.That(ns2Provider.Types, Does.Contain(myType2));
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
        var myType1 = new AliasMetadata(null, "MyType1", [], builtInTypes.I32)
        {
            Namespace = ns1Provider,
        };
        var myType2 = new AliasMetadata(null, "MyType2", [], builtInTypes.I32)
        {
            Namespace = ns2Provider,
        };
        var test1 = new AliasMetadata(null, "Test1", [], myType2)
        {
            Namespace = ns1Provider,
        };
        var test2 = new AliasMetadata(null, "Test2", [], myType1)
        {
            Namespace = ns2Provider,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(rootProvider.Types, Does.Not.Contain(myType1));
        Assert.That(rootProvider.Types, Does.Not.Contain(test1));
        Assert.That(rootProvider.Types, Does.Not.Contain(myType2));
        Assert.That(rootProvider.Types, Does.Not.Contain(test2));
        Assert.That(ns1Provider.Types, Does.Contain(myType1));
        Assert.That(ns1Provider.Types, Does.Contain(test1));
        Assert.That(ns2Provider.Types, Does.Contain(myType2));
        Assert.That(ns2Provider.Types, Does.Contain(test2));
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
        var myType = new AliasMetadata(null, "MyType", [], builtInTypes.I32)
        {
            Namespace = ns1Ns2Ns3Provider,
        };
        var myType2 = new AliasMetadata(null, "MyType2", [], myType)
        {
            Namespace = testProvider,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(rootProvider.Types, Does.Not.Contain(myType));
        Assert.That(rootProvider.Types, Does.Not.Contain(myType2));
        Assert.That(ns1Ns2Ns3Provider.Types, Does.Contain(myType));
        Assert.That(testProvider.Types, Does.Contain(myType2));
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

            public type MyType2 = MyType;
            """);

        var builtInTypes = new BuiltInTypes();
        var semantic = new SemanticAnalysis();
        var (_, _, rootProvider, _) = semantic.Analyze(
            [file1, file2],
            new SemanticAnalysisOptions(new HashSet<string>(), new SemanticDiagnosticReporter(diagnostics), builtInTypes));

        var ns1Provider = rootProvider.FindNamespace(["NS1"])!;
        var ns1Ns2Ns3Provider = rootProvider.FindNamespace(["NS1", "NS2", "NS3"])!;
        var myType = new AliasMetadata(null, "MyType", [], builtInTypes.I32)
        {
            Namespace = ns1Provider,
        };
        var myType2 = new AliasMetadata(null, "MyType2", [], myType)
        {
            Namespace = ns1Ns2Ns3Provider,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(rootProvider.Types, Does.Not.Contain(myType));
        Assert.That(rootProvider.Types, Does.Not.Contain(myType2));
        Assert.That(ns1Provider.Types, Does.Contain(myType));
        Assert.That(ns1Provider.Types, Does.Not.Contain(myType2));
        Assert.That(ns1Ns2Ns3Provider.Types, Does.Not.Contain(myType));
        Assert.That(ns1Ns2Ns3Provider.Types, Does.Contain(myType2));
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
        var myType1 = new AliasMetadata(null, "MyType", [], builtInTypes.I32)
        {
            Namespace = ns1Provider,
        };
        var myType2 = new AliasMetadata(null, "MyType", [], builtInTypes.I32)
        {
            Namespace = ns2Provider,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(rootProvider.Types, Does.Not.Contain(myType1));
        Assert.That(rootProvider.Types, Does.Not.Contain(myType2));
        Assert.That(ns1Provider.Types, Does.Contain(myType1));
        Assert.That(ns2Provider.Types, Does.Contain(myType2));
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
        var myType1 = new AliasMetadata(null, "MyType1", [], builtInTypes.I32)
        {
            Namespace = ns1Provider,
        };
        var myType2 = new AliasMetadata(null, "MyType2", [], builtInTypes.I32)
        {
            Namespace = ns1Provider,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(rootProvider.Types, Does.Not.Contain(myType1));
        Assert.That(rootProvider.Types, Does.Not.Contain(myType2));
        Assert.That(ns1Provider.Types, Does.Contain(myType1));
        Assert.That(ns1Provider.Types, Does.Contain(myType2));
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
        var myType1 = new AliasMetadata(null, "MyType1", [], builtInTypes.I32)
        {
            Namespace = ns1Provider,
        };
        var anonType = new DiscriminatedUnionMetadata(null, [builtInTypes.I32, builtInTypes.Bool])
        {
            Namespace = rootProvider,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(rootProvider.Types, Does.Not.Contain(myType1));
        Assert.That(rootProvider.Types, Does.Contain(anonType));
        Assert.That(ns1Provider.Types, Does.Contain(myType1));
        Assert.That(ns1Provider.Types, Does.Not.Contain(anonType));
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

        var diagnostic = new Diagnostic(
            DiagnosticId.S0024MultipleOverloads,
            DiagnosticSeverity.Error,
            new SourceLocation(
                new SourceFile("file5.tri"),
                new SourceSpan(new SourcePosition(85, 8, 5), new SourcePosition(89, 8, 9))),
            "Multiple overloads found for 'test'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
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
        var testFunction = ns1Provider.Functions[0];

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(
            memberAccessExpression.Reference,
            Is.EqualTo(testFunction).Using(new MetadataComparer()));
    }
}
using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using static Tri.Tests.Factory;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Semantics;

public class NamespaceTests
{
    [Test]
    public void DefineTypeInNamespaceTest()
    {
        var (project, diagnostics) = Parse([
            new SourceFile(
                "file1.tri",
                """
                namespace NS1;

                public type MyType = i32;
                """)
        ]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var ns1Provider = compilationContext.FindNamespace("test", ["NS1"]).Namespace!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(
            compilationContext.RootNamespace.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType" }));
        Assert.That(
            ns1Provider.Types,
            Has.One.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType" }));
    }

    [Test]
    public void ImportTypeFromNamespaceTest()
    {
        var (project, diagnostics) = Parse([
            new SourceFile(
                "file1.tri",
                """
                namespace NS1;

                public type MyType1 = i32;
                """),
            new SourceFile(
                "file2.tri",
                """
                namespace NS2;

                use NS1;

                public type MyType2 = MyType1;
                """)
        ]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var ns1Provider = compilationContext.FindNamespace("test", ["NS1"]).Namespace!;
        var ns2Provider = compilationContext.FindNamespace("test", ["NS2"]).Namespace!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(
            compilationContext.RootNamespace.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType1" }));
        Assert.That(
            compilationContext.RootNamespace.Types,
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
        var file1 = new SourceFile(
            "file1.tri",
            """
            namespace NS1;

            public type MyType1 = i32;
            """);
        var file2 = new SourceFile(
            "file2.tri",
            """
            namespace NS2;

            use NS1.Something;

            public main(): void {}
            """);
        var (project, diagnostics) = Parse([file1, file2]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0028UnknownNamespace,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file2,
                new SourceSpan(new SourcePosition(16, 3, 1), new SourcePosition(34, 3, 19))),
            "Unknown namespace: NS1.Something.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void CrossUseTest()
    {
        var (project, diagnostics) = Parse([
            new SourceFile(
                "file1.tri",
                """
                namespace NS1;
                use NS2;

                public type MyType1 = i32;
                public type Test1 = MyType2;
                """),
            new SourceFile(
                "file2.tri",
                """
                namespace NS2;
                use NS1;

                public type MyType2 = i32;
                public type Test2 = MyType1;
                """)
        ]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var ns1Provider = compilationContext.FindNamespace("test", ["NS1"]).Namespace!;
        var ns2Provider = compilationContext.FindNamespace("test", ["NS2"]).Namespace!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(
            compilationContext.RootNamespace.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType1" }));
        Assert.That(
            compilationContext.RootNamespace.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "Test1" }));
        Assert.That(
            compilationContext.RootNamespace.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType2" }));
        Assert.That(
            compilationContext.RootNamespace.Types,
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
        var (project, diagnostics) = Parse([
            new SourceFile(
                "file1.tri",
                """
                namespace NS1.NS2.NS3;

                public type MyType = i32;
                """),
            new SourceFile(
                "file2.tri",
                """
                namespace Test;

                use NS1.NS2.NS3;

                public type MyType2 = MyType;
                """)
        ]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var ns1Ns2Ns3Provider = compilationContext.FindNamespace("test", ["NS1", "NS2", "NS3"]).Namespace!;
        var testProvider = compilationContext.FindNamespace("test", ["Test"]).Namespace!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(
            compilationContext.RootNamespace.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType" }));
        Assert.That(
            compilationContext.RootNamespace.Types,
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
        var (project, diagnostics) = Parse([
            new SourceFile(
                "file1.tri",
                """
                namespace NS1.NS2.NS3;

                public type MyType = i32;
                """),
            new SourceFile(
                "file2.tri",
                """
                namespace Test;

                use NS1.NS2.NS3;

                public type MyType2 = MyType;
                """)
        ]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var ns1Provider = compilationContext.FindNamespace("test", ["NS1"]).Namespace!;
        var ns1Ns2Provider = compilationContext.FindNamespace("test", ["NS1", "NS2"]).Namespace!;
        var ns1Ns2Ns3Provider = compilationContext.FindNamespace("test", ["NS1", "NS2", "NS3"]).Namespace!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(ns1Provider, Is.Not.Null);
        Assert.That(ns1Ns2Provider, Is.Not.Null);
        Assert.That(ns1Ns2Ns3Provider, Is.Not.Null);
    }

    [Test]
    public void UseTypeFromParentNamespaceWithoutImportTest()
    {
        var (project, diagnostics) = Parse([
            new SourceFile(
                "file1.tri",
                """
                namespace NS1;

                public type MyType = i32;
                """),
            new SourceFile(
                "file2.tri",
                """
                namespace NS1.NS2.NS3;

                use NS1;

                public type MyType2 = MyType;
                """)
        ]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var ns1Provider = compilationContext.FindNamespace("test", ["NS1"]).Namespace!;
        var ns1Ns2Ns3Provider = compilationContext.FindNamespace("test", ["NS1", "NS2", "NS3"]).Namespace!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(
            compilationContext.RootNamespace.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType" }));
        Assert.That(
            compilationContext.RootNamespace.Types,
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
        var (project, diagnostics) = Parse([
            new SourceFile(
                "file1.tri",
                """
                namespace NS1;

                public type MyType = i32;
                """),
            new SourceFile(
                "file2.tri",
                """
                namespace NS2;

                public type MyType = i32;
                """)
        ]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var ns1Provider = compilationContext.FindNamespace("test", ["NS1"]).Namespace!;
        var ns2Provider = compilationContext.FindNamespace("test", ["NS2"]).Namespace!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(
            compilationContext.RootNamespace.Types,
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
        var file1 = new SourceFile(
            "file1.tri",
            """
            namespace NS1;

            public type MyType1 = i32;
            """);
        var file2 = new SourceFile(
            "file2.tri",
            """
            namespace NS2;

            public type MyType1 = i32;
            """);
        var file3 = new SourceFile(
            "file3.tri",
            """
            namespace Test;

            use NS1;
            use NS2;

            public type MyType2 = MyType1;
            """);
        var (project, diagnostics) = Parse([file1, file2, file3]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var testNs = compilationContext.FindNamespace("test", ["Test"]).Namespace!;
        var myType2 = (AliasMetadata)testNs.FindType("MyType2")!;

        var diagnostic = new Diagnostic(
            DiagnosticId.S0027MultipleMembersFound,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file3,
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
        var (project, diagnostics) = Parse([
            new SourceFile(
                "file1.tri",
                """
                namespace NS1;

                public type MyType1 = i32;
                """),
            new SourceFile(
                "file2.tri",
                """
                namespace NS1;

                public type MyType2 = i32;
                """)
        ]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var ns1Provider = compilationContext.FindNamespace("test", ["NS1"]).Namespace!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(
            compilationContext.RootNamespace.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType1" }));
        Assert.That(
            compilationContext.RootNamespace.Types,
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
        var (project, diagnostics) = Parse([
            new SourceFile(
                "file1.tri",
                """
                namespace NS1;

                public type MyType1 = i32 | bool;
                """)
        ]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var ns1Provider = compilationContext.FindNamespace("test", ["NS1"]).Namespace!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(
            compilationContext.RootNamespace.Types,
            Has.None.Matches<ITypeMetadata>(x => x is AliasMetadata { Name: "MyType1" }));
        Assert.That(
            compilationContext.RootNamespace.Types,
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
        var (project, diagnostics) = Parse([
            new SourceFile(
                "file1.tri",
                """
                namespace NS1;

                public type List<T> {
                    public getItem(index: i32): T | null {
                        return (T | null)null;
                    }
                }
                """),
            new SourceFile(
                "file2.tri",
                """
                namespace NS2;

                use NS1;

                public main(): void {
                    var list: List<i32> = new List<i32>();
                }
                """)
        ]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var t = new TypeArgumentMetadata(null, "T");
        var ns1Provider = compilationContext.FindNamespace("test", ["NS1"]).Namespace!;
        var ns2Provider = compilationContext.FindNamespace("test", ["NS2"]).Namespace!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(compilationContext.RootNamespace.Types, Does.Not.Contain(t));
        Assert.That(ns1Provider.Types, Does.Not.Contain(t));
        Assert.That(ns2Provider.Types, Does.Not.Contain(t));
    }

    [Test]
    public void FunctionOverloadTest()
    {
        var (project, diagnostics) = Parse([
            new SourceFile(
                "file1.tri",
                """
                namespace NS1;

                public type MyType = i32;
                """),
            new SourceFile(
                "file2.tri",
                """
                namespace NS2;

                public type MyType = i32;
                """),
            new SourceFile(
                "file3.tri",
                """
                namespace Test1;

                use NS1;

                public test(t: MyType): void { }
                """),
            new SourceFile(
                "file4.tri",
                """
                namespace Test2;

                use NS2;

                public test(t: MyType): void { }
                """),
            new SourceFile(
                "file5.tri",
                """
                namespace Test3;

                use NS1;
                use Test1;

                public main(t: MyType): void {
                    test(t);
                }
                """)
        ]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var tree = project.SourceFiles.Last().SemanticTree!;
        var call = tree.Find<CallExpression>()!;

        var expectedBuiltInTypes = new BuiltInTypes();
        var rootNs = RootNamespaceMetadata.Create(expectedBuiltInTypes);
        var packageNs = NamespaceMetadata.CreateForPackage();
        var ns1Ns = packageNs.CreateChild(["NS1"]);
        var myType = new AliasMetadata(null, "MyType", [], expectedBuiltInTypes.I32, false)
        {
            Namespace = ns1Ns,
        };
        var expected = CreateFunctionType([myType], expectedBuiltInTypes.Void, rootNs);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(call.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void FunctionMultipleOverloadTest()
    {
        var (project, diagnostics) = Parse([
            new SourceFile(
                "file1.tri",
                """
                namespace NS1;

                public type MyType { }
                """),
            new SourceFile(
                "file2.tri",
                """
                namespace NS2;

                public type MyType { }
                """),
            new SourceFile(
                "file3.tri",
                """
                namespace Test1;

                use NS1;

                public test(t: MyType): void { }
                """),
            new SourceFile(
                "file4.tri",
                """
                namespace Test2;

                use NS2;

                public test(t: MyType): void { }
                """),
            new SourceFile(
                "file5.tri",
                """
                namespace Test3;

                use NS1;
                use Test1;
                use Test2;

                public main(t: MyType): void {
                    test(t);
                }
                """)
        ]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void FunctionOverloadIncorrectParameterTypeTest()
    {
        var file1 = new SourceFile(
            "file1.tri",
            """
            namespace NS1;

            public type MyType { }
            """);
        var file2 = new SourceFile(
            "file2.tri",
            """
            namespace NS2;

            public type MyType { }
            """);
        var file3 = new SourceFile(
            "file3.tri",
            """
            namespace Test1;

            use NS1;

            public test(t: MyType): void { }
            """);
        var file4 = new SourceFile(
            "file4.tri",
            """
            namespace Test2;

            use NS2;

            public test(t: MyType): void { }
            """);
        var file5 = new SourceFile(
            "file5.tri",
            """
            namespace Test3;

            use NS2;
            use Test1;

            public main(t: MyType): void {
                test(t);
            }
            """);
        var (project, diagnostics) = Parse([file1, file2, file3, file4, file5]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0005TypeMismatch,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file5,
                new SourceSpan(new SourcePosition(79, 7, 10), new SourcePosition(80, 7, 11))),
            "Type mismatch: expected 'MyType', got 'MyType'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void FunctionGroupInDifferentNamespacesTest()
    {
        var (project, diagnostics) = Parse([
            new SourceFile(
                "file1.tri",
                """
                namespace NS1;

                public type MyType { }
                public test(t: MyType): void { }
                """),
            new SourceFile(
                "file2.tri",
                """
                namespace NS2;

                public type MyType { }
                public test(t: MyType): void { }
                """),
            new SourceFile(
                "file3.tri",
                """
                namespace Test3;

                use NS1;

                public main(): void {
                    var callback: (MyType) => void = test;
                }
                """)
        ]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var tree = project.SourceFiles.Last().SemanticTree!;
        var memberAccessExpression = tree.Find<MemberAccessExpression>()!;

        var ns1Provider = compilationContext.FindNamespace("test", ["NS1"]).Namespace!;
        var testFunction = ns1Provider.FindFunction("test").First();

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(
            memberAccessExpression.Reference,
            Is.EqualTo(testFunction).Using(new MetadataComparer()));
    }

    [Test]
    public void ResolveFullyQualifiedNameTest()
    {
        var (project, diagnostics) = Parse([
            new SourceFile(
                "file1.tri",
                """
                namespace NS1;

                public type MyType = i32;
                """),
            new SourceFile(
                "file2.tri",
                """
                namespace Test1;

                public test(obj: NS1.MyType): void { }
                """)
        ]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var test1Provider = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var testFunction = test1Provider.FindFunction("test").First();

        var expectedBuiltInTypes = new BuiltInTypes();
        var rootNs = RootNamespaceMetadata.Create(expectedBuiltInTypes);
        var packageNs = NamespaceMetadata.CreateForPackage();
        var ns1Ns = packageNs.CreateChild(["NS1"]);
        var myType = new AliasMetadata(null, "MyType", [], expectedBuiltInTypes.I32, false)
        {
            Namespace = ns1Ns,
        };

        var test1Ns = packageNs.CreateChild(["Test1"]);
        var expected = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [new ParameterMetadata(null, "obj", myType)],
            CreateFunctionType([myType], expectedBuiltInTypes.Void, rootNs))
        {
            Namespace = test1Ns,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(testFunction, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void ResolveFullyQualifiedNameForStaticTypeTest()
    {
        var (project, diagnostics) = Parse([
            new SourceFile(
                "file1.tri",
                """
                namespace NS1;

                public type MyType {
                    public static test(): void { }
                }
                """),
            new SourceFile(
                "file2.tri",
                """
                namespace Test1;

                public test(): void {
                    NS1.MyType.test();
                }
                """)
        ]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var tree = project.SourceFiles.Last().SemanticTree!;
        var member = tree.Find<MemberAccessExpression>(x => x.Name == "test")!;

        var ns1 = compilationContext.FindNamespace("test", ["NS1"]).Namespace!;
        var myType = (TypeMetadata)ns1.FindType("MyType")!;
        var method = myType.Methods[0];

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(member.Reference, Is.EqualTo(method).Using(new MetadataComparer()));
    }

    [Test]
    public void ResolveFullyQualifiedNameForStaticTypeMissingNamespaceTest()
    {
        var file = new SourceFile(
            "file1.tri",
            """
            namespace Test1;

            public test(): void {
                NS1.MyType.test();
            }
            """);
        var (project, diagnostics) = Parse([file]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0008UnknownMember,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(44, 4, 5), new SourcePosition(47, 4, 8))),
            "Unknown symbol: 'NS1'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ResolveFullyQualifiedNameForStaticMissingTypeTest()
    {
        var file1 = new SourceFile(
            "file1.tri",
            """
            namespace NS1;

            public type MyType2 = i32;
            """);
        var file2 = new SourceFile(
            "file2.tri",
            """
            namespace Test1;

            public test(): void {
                NS1.MyType.test();
            }
            """);
        var (project, diagnostics) = Parse([file1, file2]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0008UnknownMember,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file2,
                new SourceSpan(new SourcePosition(44, 4, 5), new SourcePosition(54, 4, 15))),
            "Unknown symbol: 'MyType'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ArrayWithSameTypesFromDifferentNamespacesTest()
    {
        var (project, diagnostics) = Parse([
            new SourceFile(
                "file1.tri",
                """
                namespace NS1;

                public type MyType { }
                public type A = MyType[];
                """),
            new SourceFile(
                "file2.tri",
                """
                namespace NS2;

                public type MyType { }
                public type A = MyType[];
                """)
        ]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var ns1 = compilationContext.FindNamespace("test", ["NS1"]).Namespace!;
        var ns2 = compilationContext.FindNamespace("test", ["NS2"]).Namespace!;
        var alias1 = (AliasMetadata)ns1.FindType("A")!;
        var alias2 = (AliasMetadata)ns2.FindType("A")!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(alias1.Type, Is.Not.EqualTo(alias2.Type));
        Assert.That(
            compilationContext.RootNamespace.Types,
            Has.Exactly(2).Matches<ITypeMetadata>(x => x is ArrayMetadata));
    }

    [Test]
    public void DuWithSameTypesFromDifferentNamespacesTest()
    {
        var (project, diagnostics) = Parse([
            new SourceFile(
                "file1.tri",
                """
                namespace NS1;

                public type MyType { }
                public type DU = MyType | null;
                """),
            new SourceFile(
                "file2.tri",
                """
                namespace NS2;

                public type MyType { }
                public type DU = MyType | null;
                """)
        ]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var ns1 = compilationContext.FindNamespace("test", ["NS1"]).Namespace!;
        var ns2 = compilationContext.FindNamespace("test", ["NS2"]).Namespace!;
        var alias1 = (AliasMetadata)ns1.FindType("DU")!;
        var alias2 = (AliasMetadata)ns2.FindType("DU")!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(alias1.Type, Is.Not.EqualTo(alias2.Type));
        Assert.That(
            compilationContext.RootNamespace.Types,
            Has.Exactly(3).Matches<ITypeMetadata>(x => x is DiscriminatedUnionMetadata));
    }

    [Test]
    public void FunctionTypeWithSameTypesFromDifferentNamespacesTest()
    {
        var (project, diagnostics) = Parse([
            new SourceFile(
                "file1.tri",
                """
                namespace NS1;

                public type MyType { }
                public type FT = () => MyType;
                """),
            new SourceFile(
                "file2.tri",
                """
                namespace NS2;

                public type MyType { }
                public type FT = () => MyType;
                """)
        ]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var ns1 = compilationContext.FindNamespace("test", ["NS1"]).Namespace!;
        var ns2 = compilationContext.FindNamespace("test", ["NS2"]).Namespace!;
        var alias1 = (AliasMetadata)ns1.FindType("FT")!;
        var alias2 = (AliasMetadata)ns2.FindType("FT")!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(alias1.Type, Is.Not.EqualTo(alias2.Type));
        Assert.That(
            compilationContext.RootNamespace.Types,
            Has.Exactly(3).Matches<ITypeMetadata>(x => x is FunctionTypeMetadata));
    }

    [Test]
    public void GenericWithSameTypesFromDifferentNamespacesTest()
    {
        var (project, diagnostics) = Parse([
            new SourceFile(
                "file1.tri",
                """
                namespace NS0;

                public type Generic<T> = T;
                """),
            new SourceFile(
                "file2.tri",
                """
                namespace NS1;

                use NS0;

                public type MyType { }
                public type Closed = Generic<MyType>;
                """),
            new SourceFile(
                "file3.tri",
                """
                namespace NS2;

                use NS0;

                public type MyType { }
                public type Closed = Generic<MyType>;
                """)
        ]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var ns1 = compilationContext.FindNamespace("test", ["NS1"]).Namespace!;
        var ns2 = compilationContext.FindNamespace("test", ["NS2"]).Namespace!;
        var alias1 = (AliasMetadata)ns1.FindType("Closed")!;
        var alias2 = (AliasMetadata)ns2.FindType("Closed")!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(alias1.Type, Is.Not.EqualTo(alias2.Type));
        Assert.That(
            compilationContext.RootNamespace.Types,
            Has.Exactly(2).Matches<ITypeMetadata>(x => x is GenericApplicationMetadata));
    }

    [Test]
    public void InterfaceWithSameTypesFromDifferentNamespacesTest()
    {
        var (project, diagnostics) = Parse([
            new SourceFile(
                "file1.tri",
                """
                namespace NS1;

                public type MyType { }
                public type Interface1 = {
                    obj: MyType;
                }
                """),
            new SourceFile(
                "file2.tri",
                """
                namespace NS2;

                public type MyType { }
                public type Interface1 = {
                    obj: MyType;
                }
                """)
        ]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var ns1 = compilationContext.FindNamespace("test", ["NS1"]).Namespace!;
        var ns2 = compilationContext.FindNamespace("test", ["NS2"]).Namespace!;
        var alias1 = (AliasMetadata)ns1.FindType("Interface1")!;
        var alias2 = (AliasMetadata)ns2.FindType("Interface1")!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(alias1.Type, Is.Not.EqualTo(alias2.Type));
        Assert.That(
            compilationContext.RootNamespace.Types,
            Has.Exactly(3).Matches<ITypeMetadata>(x => x is InterfaceMetadata));
    }

    [Test]
    public void TupleWithSameTypesFromDifferentNamespacesTest()
    {
        var (project, diagnostics) = Parse([
            new SourceFile(
                "file1.tri",
                """
                namespace NS1;

                public type MyType { }
                public type T = (MyType, i32);
                """),
            new SourceFile(
                "file2.tri",
                """
                namespace NS2;

                public type MyType { }
                public type T = (MyType, i32);
                """)
        ]);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var ns1 = compilationContext.FindNamespace("test", ["NS1"]).Namespace!;
        var ns2 = compilationContext.FindNamespace("test", ["NS2"]).Namespace!;
        var alias1 = (AliasMetadata)ns1.FindType("T")!;
        var alias2 = (AliasMetadata)ns2.FindType("T")!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(alias1.Type, Is.Not.EqualTo(alias2.Type));
        Assert.That(
            compilationContext.RootNamespace.Types,
            Has.Exactly(2).Matches<ITypeMetadata>(x => x is TupleMetadata));
    }

    [Test]
    public void DuplicateGenericApplicationHoistingTest()
    {
        var (project, diagnostics) = Parse(
            new SourceFile(
                "file1.tri",
                """
                namespace NS1;

                public type List<T> { }
                public type Closed = List<i32>;
                """),
            new SourceFile(
                "file2.tri",
                """
                namespace NS2;

                public type List<T> { }
                public type Closed = List<i32>;
                """));

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(
                new HashSet<string>(),
                diagnostics,
                compilationContext));

        var ns1 = compilationContext.FindNamespace("test", ["NS1"]).Namespace!;
        var ns2 = compilationContext.FindNamespace("test", ["NS2"]).Namespace!;
        var alias1 = (AliasMetadata)ns1.FindType("Closed")!;
        var alias2 = (AliasMetadata)ns2.FindType("Closed")!;
        var genericApplication1 = (GenericApplicationMetadata)alias1.Type!;
        var genericApplication2 = (GenericApplicationMetadata)alias2.Type!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(genericApplication1, Is.Not.EqualTo(genericApplication2));
        Assert.That(genericApplication1.OpenGeneric, Is.Not.EqualTo(genericApplication2.OpenGeneric));
        Assert.That(genericApplication1.ClosedGeneric, Is.Not.EqualTo(genericApplication2.ClosedGeneric));
        Assert.That(
            compilationContext.RootNamespace.Types,
            Has.Exactly(2).Matches<ITypeMetadata>(x => x is GenericApplicationMetadata));
    }
}
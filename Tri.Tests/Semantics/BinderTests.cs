using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using static Tri.Tests.Factory;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Semantics;

public class BinderTests
{
    [Test]
    public void SetMetadataForFunctionReturnTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): void {
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var expected = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "main",
            [],
            CreateFunctionType([], compilationContext.BuiltInTypes.Void, compilationContext.RootNamespace))
        {
            Namespace = compilationContext.RootNamespace,
        };

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var function = semanticTree.Find<FunctionDeclaration>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(function, Is.Not.Null);
        Assert.That(function.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForFunctionParameterTypesTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(a: i32, b: bool): void { }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var expected = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "main",
            [
                new ParameterMetadata(null, "a", compilationContext.BuiltInTypes.I32),
                new ParameterMetadata(null, "b", compilationContext.BuiltInTypes.Bool)
            ],
            CreateFunctionType(
                [compilationContext.BuiltInTypes.I32, compilationContext.BuiltInTypes.Bool],
                compilationContext.BuiltInTypes.Void,
                compilationContext.RootNamespace));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var function = semanticTree.Find<FunctionDeclaration>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(function, Is.Not.Null);
        Assert.That(function.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));
        Assert.That(
            function.Parameters[0].Type.Metadata,
            Is.EqualTo(compilationContext.BuiltInTypes.I32).Using(new MetadataComparer()));
        Assert.That(
            function.Parameters[1].Type.Metadata,
            Is.EqualTo(compilationContext.BuiltInTypes.Bool).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForVariableTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): void {
                var a: i32 = 1;
            }
            """);
        var (project, diagnostics) = Parse(file);

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

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var variable = semanticTree.Find<VariableDeclaration>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(variable, Is.Not.Null);
        Assert.That(variable.Type.Metadata, Is.EqualTo(compilationContext.BuiltInTypes.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForIncorrectVariableTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): void {
                var a: xxx = 1;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0003UnknownType,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(51, 4, 12), new SourcePosition(54, 4, 15))),
            "Unknown type: 'xxx'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void SetMetadataForTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                x: i32;
                y: i32;

                public toString(): void {
                }

                public distance(other: i32): i32 {
                    return 0;
                }
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var expected = new TypeMetadata(null, "Point")
        {
            Namespace = test1Ns,
        };
        expected.AddConstructor(
            new ConstructorMetadata(
                null,
                expected,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], expected, compilationContext.RootNamespace)));

        var xProperty = CreatePropertyMetadata(
            compilationContext.RootNamespace,
            expected,
            "x",
            compilationContext.BuiltInTypes.I32);
        expected.AddProperty(xProperty);
        expected.AddMethod(xProperty.Getter!);
        expected.AddMethod(xProperty.Setter!);

        var yProperty = CreatePropertyMetadata(
            compilationContext.RootNamespace,
            expected,
            "y",
            compilationContext.BuiltInTypes.I32);
        expected.AddProperty(yProperty);
        expected.AddMethod(yProperty.Getter!);
        expected.AddMethod(yProperty.Setter!);

        expected.AddMethod(new MethodMetadata(
            null,
            expected,
            AccessModifierMetadata.Public,
            false,
            "toString",
            [],
            CreateFunctionType([], compilationContext.BuiltInTypes.Void, compilationContext.RootNamespace)));
        expected.AddMethod(new MethodMetadata(
            null,
            expected,
            AccessModifierMetadata.Public,
            false,
            "distance",
            [new ParameterMetadata(null, "other", compilationContext.BuiltInTypes.I32)],
            CreateFunctionType([compilationContext.BuiltInTypes.I32], compilationContext.BuiltInTypes.I32, compilationContext.RootNamespace)));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var type = semanticTree.Find<TypeDeclaration>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForAliasType()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type MyInt = i32;
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var expected = new AliasMetadata(null, "MyInt", [], compilationContext.BuiltInTypes.I32, false)
        {
            Namespace = test1Ns,
        };
        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var node = semanticTree.Find<AliasDeclaration>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(node, Is.Not.Null);
        Assert.That(node.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForFunctionTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type MyF = (i32, bool) => f64;
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var expected = CreateFunctionType([compilationContext.BuiltInTypes.I32, compilationContext.BuiltInTypes.Bool],
            compilationContext.BuiltInTypes.F64,
            compilationContext.RootNamespace);
        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var type = semanticTree.Find<FunctionType>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type, Is.Not.Null);
        Assert.That(type.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForFunctionTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public add(a: i32, b: i32): i32 {
                return 0;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var expected = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "add",
            [
                new ParameterMetadata(null, "a", compilationContext.BuiltInTypes.I32),
                new ParameterMetadata(null, "b", compilationContext.BuiltInTypes.I32)
            ],
            CreateFunctionType([compilationContext.BuiltInTypes.I32, compilationContext.BuiltInTypes.I32], compilationContext.BuiltInTypes.I32, compilationContext.RootNamespace))
        {
            Namespace = compilationContext.RootNamespace,
        };

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var node = semanticTree.Find<FunctionDeclaration>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(node, Is.Not.Null);
        Assert.That(node.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForForwardDefinedFunctionTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test1(): void {
                test2();
            }

            public test2(): void { }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var expected = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test2",
            [],
            CreateFunctionType([], compilationContext.BuiltInTypes.Void, compilationContext.RootNamespace))
        {
            Namespace = compilationContext.RootNamespace,
        };

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var node = semanticTree.Find<FunctionDeclaration>(x => x.Name == "test2")!;
        var memberAccess = semanticTree.Find<MemberAccessExpression>()!;
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(node.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));
        Assert.That(memberAccess.Reference, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void LiteralNumberTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): i32 {
                return 1;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(compilationContext.BuiltInTypes.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void LiteralByteTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(): i8 {
                return 1i8;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var returnNode = semanticTree.Find<ReturnStatement>()!;
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(
            returnNode.Expression!.ReturnTypeMetadata,
            Is.EqualTo(compilationContext.BuiltInTypes.I8).Using(new MetadataComparer()));
    }

    [Test]
    public void LiteralFloatTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): f64 {
                return 3.14;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(compilationContext.BuiltInTypes.F64).Using(new MetadataComparer()));
    }

    [Test]
    public void LiteralBoolTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): bool {
                return true;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(compilationContext.BuiltInTypes.Bool).Using(new MetadataComparer()));
    }

    [Test]
    public void LiteralCharTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): char {
                return 'x';
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(compilationContext.BuiltInTypes.Char).Using(new MetadataComparer()));
    }

    [Test]
    public void LiteralStringTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): string {
                return "xxx";
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(compilationContext.BuiltInTypes.String).Using(new MetadataComparer()));
    }

    [Test]
    public void ReturnStatementTypeDoesntMatchTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): bool {
                return 1;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0004ReturnTypeMismatch,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(44, 4, 5), new SourcePosition(53, 4, 14))),
            "Return type mismatch: expected 'bool', got 'i32'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void UnaryPlusTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): i32 {
                return -1;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(compilationContext.BuiltInTypes.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void UnaryMinusTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): i32 {
                return -1;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(compilationContext.BuiltInTypes.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void LogicalNotTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): bool {
                return !true;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(compilationContext.BuiltInTypes.Bool).Using(new MetadataComparer()));
    }

    [Test]
    public void BinaryExpressionTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): i32 {
                return 1 + 2;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var binaryNode = semanticTree.Find<BinaryExpression>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(binaryNode, Is.Not.Null);
        Assert.That(
            binaryNode.ReturnTypeMetadata,
            Is.EqualTo(compilationContext.BuiltInTypes.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void LogicalNotIncorrectOperandTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): i32 {
                return !1;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0010IncompatibleUnaryOperator,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(50, 4, 12), new SourcePosition(52, 4, 14))),
            "Incompatible operand type 'i32' for operator '!'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void AddressOfTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(p: i32): i32* {
                return &p;
            }
            """);
        var (project, diagnostics) = Parse(file);

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
    public void DereferenceTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(p: i32*): i32 {
                return *p;
            }
            """);
        var (project, diagnostics) = Parse(file);

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
    public void DereferenceIncorrectOperantTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(p: i32): i32 {
                return *p;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0010IncompatibleUnaryOperator,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(56, 4, 12), new SourcePosition(58, 4, 14))),
            "Incompatible operand type 'i32' for operator '*'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void VariableExpressionTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(a: i32): i32 {
                return a;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(compilationContext.BuiltInTypes.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void VariableDeclarationIncorrectTypesTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): void {
                var a: i32 = true;
            }
            """);
        var (project, diagnostics) = Parse(file);

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
                file,
                new SourceSpan(new SourcePosition(57, 4, 18), new SourcePosition(61, 4, 22))),
            "Type mismatch: expected 'i32', got 'bool'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void IfIncorrectConditionTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): void {
                if (1) {
                }
            }
            """);
        var (project, diagnostics) = Parse(file);

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
                file,
                new SourceSpan(new SourcePosition(48, 4, 9), new SourcePosition(49, 4, 10))),
            "Type mismatch: expected 'bool', got 'i32'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void FunctionCallIncorrectParameterTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public add(a: i32): i32 {
                return 0;
            }

            public main(): i32 {
                return add(true);
            }
            """);
        var (project, diagnostics) = Parse(file);

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
                file,
                new SourceSpan(new SourcePosition(97, 8, 16), new SourcePosition(101, 8, 20))),
            "Type mismatch: expected 'i32', got 'bool'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void WhileNonBoolConditionTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): void {
                while (1) {
                }
            }
            """);
        var (project, diagnostics) = Parse(file);

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
                file,
                new SourceSpan(new SourcePosition(51, 4, 12), new SourcePosition(52, 4, 13))),
            "Type mismatch: expected 'bool', got 'i32'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ReturnInConstructorTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                public constructor() {
                    return;
                }
            }
            """);
        var (project, diagnostics) = Parse(file);

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
    public void ReturnWithExpressionInConstructorTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                public constructor() {
                    return 0;
                }
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0004ReturnTypeMismatch,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(73, 5, 9), new SourcePosition(82, 5, 18))),
            "Return type mismatch: expected 'void', got 'i32'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void SetMetadataForInterfaceTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point = {
                x: i32;
                y: i32;
                distance(Point): f64;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var interfaceType = new InterfaceMetadata(null)
        {
            Namespace = compilationContext.RootNamespace,
        };
        var expected = new AliasMetadata(null, "Point", [], interfaceType, false)
        {
            Namespace = test1Ns,
        };

        interfaceType.AddProperty(
            new InterfacePropertyMetadata(
                null,
                interfaceType,
                "x",
                compilationContext.BuiltInTypes.I32,
                AccessModifierMetadata.Public,
                null));
        interfaceType.AddProperty(
            new InterfacePropertyMetadata(
                null,
                interfaceType,
                "y",
                compilationContext.BuiltInTypes.I32,
                AccessModifierMetadata.Public,
                null));
        interfaceType.AddMethod(
            new InterfaceMethodMetadata(
                null,
                interfaceType,
                "distance",
                CreateFunctionType(
                    [expected],
                    compilationContext.BuiltInTypes.F64,
                    compilationContext.RootNamespace)));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var type = semanticTree.Find<AliasDeclaration>()!;
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(type.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void SetReturnTypeForVariableWithFunctionTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public add(a: i32, b: i32): i32 {
                return 1;
            }

            public main(): void {
                var x: (i32, i32) => i32 = add;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var expected = CreateFunctionType(
            [compilationContext.BuiltInTypes.I32, compilationContext.BuiltInTypes.I32],
            compilationContext.BuiltInTypes.I32,
            compilationContext.RootNamespace);
        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var memberAccess = semanticTree.Find<MemberAccessExpression>()!;
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(memberAccess.ReturnTypeMetadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void CallNonFunctionTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(a: i32): void {
                a();
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0007ExpectedFunction,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(50, 4, 5), new SourcePosition(51, 4, 6))),
            "Expected a function, got 'i32'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ThisReturnTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                public toString(): void {
                    this;
                }
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var thisNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "this")!;
        var pointType = rootNamespace.FindType("Point*")!;
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(thisNode.ReturnTypeMetadata, Is.EqualTo(pointType).Using(new MetadataComparer()));
    }

    [Test]
    public void ThisWithMultipleMembersReturnTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                a: i32;

                public toString(): void {
                    this.a;
                }
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var thisNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "a");
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(thisNode, Is.Not.Null);
        Assert.That(
            thisNode.ReturnTypeMetadata,
            Is.EqualTo(compilationContext.BuiltInTypes.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void ThisWithIncorrectPropertyNameTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                a: i32;

                public toString(): void {
                    this.x;
                }
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0008UnknownMember,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(89, 7, 9), new SourcePosition(95, 7, 15))),
            "The member was not found.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void ThisIsPointerTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                public getInstance(): Point* {
                    return this;
                }
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var pointPointer = rootNamespace.FindType("Point*");
        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var returnNode = semanticTree.Find<ReturnStatement>()!;
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(
            returnNode.Expression!.ReturnTypeMetadata,
            Is.EqualTo(pointPointer).Using(new MetadataComparer()));
    }

    [Test]
    public void InterfaceMemberAccessReturnTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point = {
                x: i32;
            }

            public test(a: Point): i32 {
                return a.x;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var aNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "a")!;
        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var pointType = test1Ns.FindType("Point");
        var xNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "x")!;
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(aNode.ReturnTypeMetadata, Is.EqualTo(pointType).Using(new MetadataComparer()));
        Assert.That(
            xNode.ReturnTypeMetadata,
            Is.EqualTo(compilationContext.BuiltInTypes.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void InterfaceMemberAccessIncorrectPropertyTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point = {
                x: i32;
            }

            public test(a: Point): i32 {
                return a.c;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0008UnknownMember,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(95, 8, 12), new SourcePosition(98, 8, 15))),
            "The member was not found.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void AliasFunctionTypeMemberAccessReturnTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type F = () => void;

            public type Test {
                f: F;
            }

            public test(a: Test): F {
                return a.f;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var aNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "a")!;
        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var pointType = test1Ns.FindType("Test");
        var xNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "f")!;
        var functionType = test1Ns.FindType("F");
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(aNode.ReturnTypeMetadata, Is.EqualTo(pointType).Using(new MetadataComparer()));
        Assert.That(xNode.ReturnTypeMetadata, Is.EqualTo(functionType).Using(new MetadataComparer()));
    }

    [Test]
    public void NewOperatorSetCtorTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                public constructor(x: i32, y: i32) {
                }
            }

            public test(): void {
                var a: Point* = new Point(1, 2);
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var type = (TypeMetadata)test1Ns.FindType("Point")!;
        var ctor = (ConstructorMetadata)type.GetConstructors()[0];
        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var newOp = semanticTree.Find<NewObjectExpression>()!;
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(newOp.Metadata, Is.EqualTo(ctor));
    }

    [Test]
    public void NewOperatorForInterfaceTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point = {
                x: i32;
                y: i32;
            }

            public test(): void {
                var a: Point* = new Point(1, 2);
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0023NoSuitableOverload,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(113, 9, 25), new SourcePosition(118, 9, 30))),
            "No suitable overload found.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void NewOperatorMissingConstructorTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                public constructor(x: i32, y: i32) {
                }
            }

            public test(): void {
                var a: Point* = new Point(2);
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0026MissingArgument,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(134, 9, 25), new SourcePosition(142, 9, 33))),
            "Missing argument: 'i32'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void NewOperatorWithAliasTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point { }
            public type Test = Point;

            public test(): void {
                var a: Point* = new Test();
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var test1Ns = compilationContext.FindNamespace(["Test1"]).Namespace!;
        var point = test1Ns.Types.OfType<TypeMetadata>().First(x => x.Name == "Point");
        var ctor = (ConstructorMetadata)point.GetConstructors()[0];

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var newOp = semanticTree.Find<NewObjectExpression>()!;

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(newOp.Metadata, Is.EqualTo(ctor).Using(new MetadataComparer()));
    }

    [Test]
    public void NewOperatorWithExpressionTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                x: i32;
            }

            public test(p: Point): void {
                var a: Point* = new p.x;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0009ExpectedCtorOrArray,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(103, 8, 21), new SourcePosition(110, 8, 28))),
            "Expected a constructor call or array access.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void NewOperatorWithIncorrectGenericTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                x: i32;
            }

            public test(p: Point): void {
                var a: Point* = new p.x<i32>();
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0030NonGenericMember,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(107, 8, 25), new SourcePosition(115, 8, 33))),
            "'x' is not generic.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void CreateObjectOnStackTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                public constructor(x: i32, y: i32) { }
            }

            public test(): void {
                var a: Point = Point(1, 2);
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var call = semanticTree.Find<CallExpression>()!;
        var expectedBuiltInTypes = new BuiltInTypes();
        var expectedRoot = RootNamespaceMetadata.Create(expectedBuiltInTypes);
        var expectedPackage = NamespaceMetadata.CreateForPackage();
        var expectedTest1Ns = expectedPackage.CreateChild(["Test1"]);
        var expectedType = new TypeMetadata(null, "Point", [], [], [], [], [], [], false, false)
        {
            Namespace = expectedTest1Ns,
        };
        var expectedCtor = new ConstructorMetadata(
            null,
            expectedType,
            AccessModifierMetadata.Public,
            [
                new ParameterMetadata(null, "x", expectedBuiltInTypes.I32),
                new ParameterMetadata(null, "y", expectedBuiltInTypes.I32),
            ],
            CreateFunctionType([expectedBuiltInTypes.I32, expectedBuiltInTypes.I32], expectedType, expectedRoot));
        expectedType.AddConstructor(expectedCtor);

        var memberAccessExpression = (MemberAccessExpression)call.Member;
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(memberAccessExpression.Reference, Is.EqualTo(expectedCtor).Using(new MetadataComparer()));
    }

    [Test]
    public void CreateObjectOnStackWithTheSameFunctionTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type point {
                public constructor(x: i32, y: i32) { }
            }

            public point(x: i32, y: i32): void { }

            public test(): void {
                var a: point = point(1, 2);
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0024MultipleCandidates,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(165, 10, 20), new SourcePosition(170, 10, 25))),
            """
            Multiple candidates found:
            - point: (i32, i32) => void
            - <>_ctor: (i32, i32) => point.
            """);

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void CreateObjectOnStackCtorWithCallbackTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Point {
                public constructor(callback: (i32) => void) { }
            }

            public test(p: i32): void { }
            public test(p: bool): void { }

            public main(): void {
                var a: Point = Point(test);
            }
            """);
        var (project, diagnostics) = Parse(file);

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
    public void SetMetadataForDiscriminatedUnionTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type DU = {} | i32 | () => void;
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var du = new DiscriminatedUnionMetadata(null, [
            new InterfaceMetadata(null)
            {
                Namespace = compilationContext.RootNamespace,
            },
            compilationContext.BuiltInTypes.I32,
            CreateFunctionType([], compilationContext.BuiltInTypes.Void, compilationContext.RootNamespace)
        ])
        {
            Namespace = compilationContext.RootNamespace,
        };
        var alias = new AliasMetadata(null, "DU", [], du, false)
        {
            Namespace = test1Ns,
        };

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var aliasNode = semanticTree.Find<AliasDeclaration>()!;
        var duNode = semanticTree.Find<DiscriminatedUnion>()!;
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(aliasNode.Metadata, Is.EqualTo(alias).Using(new MetadataComparer()));
        Assert.That(duNode.Metadata, Is.EqualTo(du).Using(new MetadataComparer()));
    }

    [Test]
    public void AccessArrayElementTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(a: i32[]): i32 {
                return a[1];
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var arrayAccess = semanticTree.Find<ArrayAccessExpression>()!;
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(
            arrayAccess.ReturnTypeMetadata,
            Is.EqualTo(compilationContext.BuiltInTypes.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void UseArrayAccessorOnNotArrayTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(a: i32): void {
                return a[1];
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0006ExpectedArray,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(57, 4, 12), new SourcePosition(58, 4, 13))),
            "Expected an array, got 'i32'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void UseArrayAccessorWithNonNumberTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(a: i32[]): i32 {
                return a["xxx"];
            }
            """);
        var (project, diagnostics) = Parse(file);

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
                file,
                new SourceSpan(new SourcePosition(60, 4, 14), new SourcePosition(65, 4, 19))),
            "Type mismatch: expected 'i32', got 'string'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void GenerateMetadataForExpressionTupleTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(): (i32, i32) {
                return (1, 2);
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var expected = CreateTupleMetadata(
            [compilationContext.BuiltInTypes.I32, compilationContext.BuiltInTypes.I32],
            compilationContext.RootNamespace);

        var actual = compilationContext.RootNamespace.FindType("(i32, i32)");
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void NewArrayReturnTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): i32[]* {
                return new i32[10];
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var expected = new PointerMetadata(
            null,
            CreateArrayMetadata(compilationContext.BuiltInTypes.I32, compilationContext.RootNamespace));
        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var newObj = semanticTree.Find<NewObjectExpression>()!;
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(newObj.ReturnTypeMetadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenericPropertyTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test<T> {
                x: T;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var property = semanticTree.Find<PropertyDeclaration>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(property, Is.Not.Null);
        Assert.That(property.Metadata, Is.Not.Null);
        Assert.That(
            property.Metadata.Type,
            Is.EqualTo(new TypeArgumentMetadata(null, "T")).Using(new MetadataComparer()));
    }

    [Test]
    public void GenericArrayPropertyTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test<T> {
                x: T[];
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var property = semanticTree.Find<PropertyDeclaration>();
        Assert.That(property, Is.Not.Null);
        Assert.That(property.Metadata, Is.Not.Null);

        var typeArrayMetadata = CreateArrayMetadata(new TypeArgumentMetadata(null, "T"),
            compilationContext.RootNamespace);
        Assert.That(property.Metadata.Type, Is.EqualTo(typeArrayMetadata).Using(new MetadataComparer()));
        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }

    [Test]
    public void SetMetadataForClosedGenericTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type List<T> {}
            public type Test = List<i32>;
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var closedType = compilationContext.RootNamespace.FindType("List<i32>")!;
        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var genericTypeNode = semanticTree.Find<GenericApplication>()!;
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(genericTypeNode.Metadata, Is.EqualTo(closedType).Using(new MetadataComparer()));
    }

    [Test]
    public void FindCtorInGenericTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test<T> {
                a: T;
            }

            public main(): void {
                var x: Test<i32>* = new Test<i32>();
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var closedGeneric = compilationContext.RootNamespace.FindType("Test<i32>") as GenericApplicationMetadata;
        var closedType = closedGeneric!.ClosedGeneric as TypeMetadata;
        var ctor = (ConstructorMetadata)closedType!.GetConstructors()[0];

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var newObj = semanticTree.Find<NewObjectExpression>()!;
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(newObj.Metadata, Is.EqualTo(ctor));
    }

    [Test]
    public void SetMetadataForClosedGenericTypeFieldTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test<T> {
                a: T;
            }

            public main(): i32 {
                var x: Test<i32>* = new Test<i32>();
                return x.a;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var returnStmt = semanticTree.Find<ReturnStatement>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(returnStmt, Is.Not.Null);
        Assert.That(returnStmt.Expression, Is.Not.Null);
        Assert.That(
            returnStmt.Expression.ReturnTypeMetadata,
            Is.EqualTo(compilationContext.BuiltInTypes.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void MultipleGenericsTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test<T1> { }
            public type Test<T1, T2> { }
            public type Test<T1, T2, T3> { }

            public main(): void {
                var x: Test<i32>* = new Test<i32>();
            }
            """);
        var (project, diagnostics) = Parse(file);

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
    public void MemberAccessNestedCallTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test1 {
                public b(): Test2* {
                    return new Test2();
                }
            }

            public type Test2 {
                c: i32;
            }

            public test(a: Test1): i32 {
                return a.b().c;
            }
            """);
        var (project, diagnostics) = Parse(file);

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
    public void SetMetadataForStaticClassTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test {
                public static test(): void {
                }
            }

            public main(): void {
                Test.test();
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var type = test1Ns.FindType("Test")!;
        var functionType = compilationContext.RootNamespace.FindType("() => void")!;

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var staticTypeMember = semanticTree.Find<MemberAccessExpression>(x => x.Name == "Test")!;
        var member = semanticTree.Find<MemberAccessExpression>(x => x.Name == "test")!;
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(staticTypeMember.ReturnTypeMetadata, Is.EqualTo(type));
        Assert.That(member.ReturnTypeMetadata, Is.EqualTo(functionType));
    }

    [Test]
    public void TupleMemberAccessTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(a: (i32, string)): string {
                return a.1;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var tupleMember = semanticTree.Find<MemberAccessExpression>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(tupleMember, Is.Not.Null);
        Assert.That(
            tupleMember.ReturnTypeMetadata,
            Is.EqualTo(compilationContext.BuiltInTypes.String).Using(new MetadataComparer()));
    }

    [Test]
    public void TupleMemberAccessIndexOutsideTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(a: (i32, string)): string {
                return a.2;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0008UnknownMember,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(69, 4, 12), new SourcePosition(72, 4, 15))),
            "The member was not found.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void CastExpressionTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(a: i32): i8 {
                return (i8)a;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var castExp = semanticTree.Find<CastExpression>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(castExp, Is.Not.Null);
        Assert.That(
            castExp.ReturnTypeMetadata,
            Is.EqualTo(compilationContext.BuiltInTypes.I8).Using(new MetadataComparer()));
    }

    [Test]
    public void UseMethodBeforeDeclarationTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test {
                public method1(): void {
                    method2();
                }

                public method2(): void {}
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var type = (TypeMetadata)test1Ns.FindType("Test")!;
        var method = (MethodMetadata)type.GetMethods("method2")[0];

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var memberAccess = semanticTree.Find<MemberAccessExpression>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(memberAccess, Is.Not.Null);
        Assert.That(memberAccess.Reference, Is.EqualTo(method).Using(new MetadataComparer()));
    }

    [Test]
    public void UseMethodBeforeDeclarationInCtorTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test {
                public constructor() {
                    method2();
                }

                public method2(): void {}
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var type = (TypeMetadata)test1Ns.FindType("Test")!;
        var method = (MethodMetadata)type.GetMethods("method2")[0];

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var memberAccess = semanticTree.Find<MemberAccessExpression>()!;
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(memberAccess.Reference, Is.EqualTo(method).Using(new MetadataComparer()));
    }

    [Test]
    public void UsePropertyBeforeDeclarationTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test {
                public method1(): i32 {
                    return prop;
                }

                prop: i32;
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var type = test1Ns.FindType("Test")!;
        var method = type.GetMembers("prop")[0];

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var memberAccess = semanticTree.Find<MemberAccessExpression>();
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(memberAccess, Is.Not.Null);
        Assert.That(memberAccess.Reference, Is.EqualTo(method).Using(new MetadataComparer()));
    }

    [Test]
    public void CallFunctionOverloadTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public test(x: i32): void { }

            public test(x: bool): void { }

            public main(): void {
                test(1);
                test(true);
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var members = semanticTree.Where<MemberAccessExpression>().ToArray();
        var calls = semanticTree.Where<CallExpression>().ToArray();
        var function1 = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [new ParameterMetadata(null, "x", compilationContext.BuiltInTypes.I32)],
            CreateFunctionType([compilationContext.BuiltInTypes.I32], compilationContext.BuiltInTypes.Void, compilationContext.RootNamespace))
        {
            Namespace = compilationContext.RootNamespace,
        };
        var function2 = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "test",
            [new ParameterMetadata(null, "x", compilationContext.BuiltInTypes.Bool)],
            CreateFunctionType([compilationContext.BuiltInTypes.Bool], compilationContext.BuiltInTypes.Void, compilationContext.RootNamespace))
        {
            Namespace = compilationContext.RootNamespace,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(members[0].Reference, Is.EqualTo(function1).Using(new MetadataComparer()));
        Assert.That(members[1].Reference, Is.EqualTo(function2).Using(new MetadataComparer()));
        Assert.That(calls[0].Metadata, Is.EqualTo(function1.Type).Using(new MetadataComparer()));
        Assert.That(calls[1].Metadata, Is.EqualTo(function2.Type).Using(new MetadataComparer()));
    }

    [Test]
    public void CallMethodOverloadTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test {
                public method(x: i32): void { }

                public method(x: bool): void { }

                public test(): void {
                    method(1);
                    method(true);
                }
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var members = semanticTree.Where<MemberAccessExpression>().ToArray();
        var calls = semanticTree.Where<CallExpression>().ToArray();
        var type = new TypeMetadata(null, "Test");
        var method1 = new MethodMetadata(
            null,
            type,
            AccessModifierMetadata.Public,
            false,
            "method",
            [new ParameterMetadata(null, "x", compilationContext.BuiltInTypes.I32)],
            CreateFunctionType([compilationContext.BuiltInTypes.I32], compilationContext.BuiltInTypes.Void, compilationContext.RootNamespace));
        var method2 = new MethodMetadata(
            null,
            type,
            AccessModifierMetadata.Public,
            false,
            "method",
            [new ParameterMetadata(null, "x", compilationContext.BuiltInTypes.Bool)],
            CreateFunctionType([compilationContext.BuiltInTypes.Bool], compilationContext.BuiltInTypes.Void, compilationContext.RootNamespace));
        type.AddMethod(method1);
        type.AddMethod(method2);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(members[0].Reference, Is.EqualTo(method1).Using(new MetadataComparer()));
        Assert.That(members[1].Reference, Is.EqualTo(method2).Using(new MetadataComparer()));
        Assert.That(calls[0].Metadata, Is.EqualTo(method1.Type).Using(new MetadataComparer()));
        Assert.That(calls[1].Metadata, Is.EqualTo(method2.Type).Using(new MetadataComparer()));
    }

    [Test]
    public void CallMethodOverloadOutsideTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test {
                public method(x: i32): void { }

                public method(x: bool): void { }
            }

            public test(o: Test): void {
                o.method(1);
                o.method(true);
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var members = semanticTree.Where<MemberAccessExpression>().ToArray();
        var calls = semanticTree.Where<CallExpression>().ToArray();
        var type = new TypeMetadata(null, "Test")
        {
            Namespace = compilationContext.RootNamespace,
        };
        var method1 = new MethodMetadata(
            null,
            type,
            AccessModifierMetadata.Public,
            false,
            "method",
            [new ParameterMetadata(null, "x", compilationContext.BuiltInTypes.I32)],
            CreateFunctionType([compilationContext.BuiltInTypes.I32], compilationContext.BuiltInTypes.Void, compilationContext.RootNamespace));
        var method2 = new MethodMetadata(
            null,
            type,
            AccessModifierMetadata.Public,
            false,
            "method",
            [new ParameterMetadata(null, "x", compilationContext.BuiltInTypes.Bool)],
            CreateFunctionType([compilationContext.BuiltInTypes.Bool], compilationContext.BuiltInTypes.Void, compilationContext.RootNamespace));
        type.AddMethod(method1);
        type.AddMethod(method2);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(members[0].Reference, Is.EqualTo(method1).Using(new MetadataComparer()));
        Assert.That(members[1].Reference, Is.EqualTo(method2).Using(new MetadataComparer()));
        Assert.That(calls[0].Metadata, Is.EqualTo(method1.Type).Using(new MetadataComparer()));
        Assert.That(calls[1].Metadata, Is.EqualTo(method2.Type).Using(new MetadataComparer()));
    }

    [Test]
    public void CallInterfaceMethodOverloadOutsideTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test = {
                method(i32): void;
                method(bool): void;
            }

            public test(o: Test): void {
                o.method(1);
                o.method(true);
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var members = semanticTree.Where<MemberAccessExpression>().ToArray();
        var calls = semanticTree.Where<CallExpression>().ToArray();
        var type = new InterfaceMetadata(null)
        {
            Namespace = compilationContext.RootNamespace,
        };
        var method1 = new InterfaceMethodMetadata(
            null,
            type,
            "method",
            CreateFunctionType([compilationContext.BuiltInTypes.I32], compilationContext.BuiltInTypes.Void, compilationContext.RootNamespace));
        var method2 = new InterfaceMethodMetadata(
            null,
            type,
            "method",
            CreateFunctionType([compilationContext.BuiltInTypes.Bool], compilationContext.BuiltInTypes.Void, compilationContext.RootNamespace));
        type.AddMethod(method1);
        type.AddMethod(method2);

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(members[0].Reference, Is.EqualTo(method1).Using(new MetadataComparer()));
        Assert.That(members[1].Reference, Is.EqualTo(method2).Using(new MetadataComparer()));
        Assert.That(calls[0].Metadata, Is.EqualTo(method1.Type).Using(new MetadataComparer()));
        Assert.That(calls[1].Metadata, Is.EqualTo(method2.Type).Using(new MetadataComparer()));
    }

    [Test]
    public void PassFunctionGroupAsParameterTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public f(x: i32): void { }
            public f(x: bool): void { }
            public test(func: (bool) => void): void { }

            public main(): void {
                test(f);
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var call = semanticTree.Find<CallExpression>()!;
        var parameter = (MemberAccessExpression)call.Parameters[0];

        var function = new FunctionMetadata(
            null,
            AccessModifierMetadata.Public,
            "f",
            [new ParameterMetadata(null, "x", compilationContext.BuiltInTypes.Bool)],
            CreateFunctionType([compilationContext.BuiltInTypes.Bool], compilationContext.BuiltInTypes.Void, compilationContext.RootNamespace))
        {
            Namespace = compilationContext.RootNamespace,
        };

        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(parameter.Reference, Is.EqualTo(function).Using(new MetadataComparer()));
    }

    [Test]
    public void PassFunctionGroupAsParameterFailTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public f(x: i32): void { }
            public f(x: f64): void { }
            public test(func: (bool) => void): void { }

            public main(): void {
                test(f);
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0023NoSuitableOverload,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(148, 8, 10), new SourcePosition(149, 8, 11))),
            "No suitable overload found.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void FullyQualifiedMemberAccessTest()
    {
        var file = CreateFile(
            """
            namespace Test1.Test2.Test3;

            public type Point { }

            public main(): void {
                var p: Point* = new Test1.Test2.Test3.Point();
            }
            """);
        var (project, diagnostics) = Parse(file);

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
    public void NoGenericCtorOverloadTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type List<T1, T2> { }

            public main(): void {
                var p: List<i32, i32>* = new List<i32>();
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.S0026MissingArgument,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    file,
                    new SourceSpan(new SourcePosition(103, 6, 34), new SourcePosition(112, 6, 43))),
                "Missing argument: 'T2'."),
        };

        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void MultipleGenericCtorOverloadTest()
    {
        var file1 = new SourceFile(
            "test1.tri",
            """
            namespace Test1;

            public type List<T1> { }
            """);
        var file2 = new SourceFile(
            "test2.tri",
            """
            namespace Test2;

            public type List<T1> { }
            """);
        var file3 = new SourceFile(
            "test3.tri",
            """
            namespace Test3;

            use Test1;
            use Test2;

            public main(): void {
                var p: List<i32>* = new List<i32>();
            }
            """);
        var (project, diagnostics) = Parse(file1, file2, file3);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.S0027MultipleMembersFound,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    file3,
                    new SourceSpan(new SourcePosition(74, 7, 12), new SourcePosition(83, 7, 21))),
                """
                Multiple members found:
                - List<>
                - List<>.
                """),
            new Diagnostic(
                DiagnosticId.S0024MultipleCandidates,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    file3,
                    new SourceSpan(new SourcePosition(91, 7, 29), new SourcePosition(95, 7, 33))),
                """
                Multiple candidates found:
                - List<>
                - List<>.
                """),
        };

        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void MultipleTargetsMultipleCandidatesTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type MyType {
                public method1(p: bool): void { }
                public method1(p: i32): void { }
            }

            public test(callback: (bool) => void): void { }
            public test(callback: (i32) => void): void { }

            public main(): void {
                var t: MyType* = new MyType();
                test(t.method1);
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new[]
        {
            new Diagnostic(
                DiagnosticId.S0027MultipleMembersFound,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    file,
                    new SourceSpan(new SourcePosition(279, 13, 10), new SourcePosition(288, 13, 19))),
                """
                Multiple members found:
                - method1: (bool) => void
                - method1: (i32) => void.
                """),
            new Diagnostic(
                DiagnosticId.S0023NoSuitableOverload,
                DiagnosticSeverity.Error,
                new SourceLocation(
                    file,
                    new SourceSpan(new SourcePosition(274, 13, 5), new SourcePosition(278, 13, 9))),
                "No suitable overload found."),
        };

        Assert.That(diagnostics.Diagnostics, Is.EqualTo(diagnostic));
    }

    [Test]
    public void CtorAndMethodWithTheSameNameTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type MyType { }

            public MyType(): void { }

            public main(): void {
                var t: MyType* = new MyType();
            }
            """);
        var (project, diagnostics) = Parse(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0024MultipleCandidates,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(116, 8, 26), new SourcePosition(122, 8, 32))),
            """
            Multiple candidates found:
            - MyType: () => void
            - <>_ctor: () => MyType.
            """);

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void CastResolveFunctionTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public f(x: bool): void { }
            public f(x: i32): void { }

            public test(func: (bool) => void): void { }
            public test(func: (i32) => void): void { }

            public main(): void {
                test(((i32) => void)f);
            }
            """);
        var (project, diagnostics) = Parse(file);

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
    public void CallbackTest()
    {
        var (project, diagnostics) = Parse(
            new SourceFile(
                "file.tri",
                """
                namespace Test1;

                public test(callback: () => void): void {
                    callback();
                }
                """));

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
    public void FunctionPointerTest()
    {
        var (project, diagnostics) = Parse(
            new SourceFile(
                "file.tri",
                """
                namespace Test1;

                public f(): void { }

                public main(): void {
                    var func: () => void = f;
                    func();
                }
                """));

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var compilationContext = new CompilationContext(builtInTypes, rootNamespace);
        var semantic = new SemanticAnalyzer();
        semantic.Analyze(
            project,
            new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext));

        Assert.That(diagnostics.Diagnostics, Is.Empty);
    }
}
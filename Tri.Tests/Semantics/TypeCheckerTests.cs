using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Metadata;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using static Tri.Tests.Factory;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Semantics;

public class TypeCheckerTests
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
                CreateFunctionType([], compilationContext.BuiltInTypes.Void, compilationContext.RootNamespace)));

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
        var node = semanticTree.Find<FunctionDeclaration>(x => x.Name == "test2");
        Assert.That(node, Is.Not.Null);
        Assert.That(node.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));

        var memberAccess = semanticTree.Find<MemberAccessExpression>();
        Assert.That(memberAccess, Is.Not.Null);
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
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(compilationContext.BuiltInTypes.I32).Using(new MetadataComparer()));
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
            "Incompatible operand type 'i32' for operator 'LogicalNot'.");

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

        Assert.That(
            () => semantic.Analyze(
                project,
                new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext)),
            Throws.Nothing);
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
        var type = semanticTree.Find<AliasDeclaration>();
        Assert.That(type, Is.Not.Null);
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
        var memberAccess = semanticTree.Find<MemberAccessExpression>();
        Assert.That(memberAccess, Is.Not.Null);
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
        var thisNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "this");
        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var pointType = test1Ns.FindType("Point");
        Assert.That(thisNode, Is.Not.Null);
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
            "The 'Point' type doesn't have 'x'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
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
        var aNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "a");
        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var pointType = test1Ns.FindType("Point");
        Assert.That(aNode, Is.Not.Null);
        Assert.That(aNode.ReturnTypeMetadata, Is.EqualTo(pointType).Using(new MetadataComparer()));

        var xNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "x");
        Assert.That(xNode, Is.Not.Null);
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
            "The 'Point' type doesn't have 'c'.");

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
        var aNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "a");
        var test1Ns = compilationContext.FindNamespace("test", ["Test1"]).Namespace!;
        var pointType = test1Ns.FindType("Test");
        Assert.That(aNode, Is.Not.Null);
        Assert.That(aNode.ReturnTypeMetadata, Is.EqualTo(pointType).Using(new MetadataComparer()));

        var xNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "f");
        var functionType = test1Ns.FindType("F");
        Assert.That(xNode, Is.Not.Null);
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
                var a: Point = new Point(1, 2);
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
        var type = test1Ns.FindType("Point") as TypeMetadata;
        Assert.That(type, Is.Not.Null);

        var ctor = type.GetConstructor([compilationContext.BuiltInTypes.I32, compilationContext.BuiltInTypes.I32]);
        Assert.That(ctor, Is.Not.Null);

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var newOp = semanticTree.Find<NewObjectExpression>();
        Assert.That(newOp, Is.Not.Null);
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
                var a: Point = new Point(1, 2);
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
            DiagnosticId.S0009CantCreateObject,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(108, 9, 20), new SourcePosition(123, 9, 35))),
            "Cannot create an instance of type 'Point'");

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
                var a: Point = new Point(2);
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
                new SourceSpan(new SourcePosition(129, 9, 20), new SourcePosition(141, 9, 32))),
            "The 'Point' type doesn't have 'i32' constructor.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
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
        var aliasNode = semanticTree.Find<AliasDeclaration>();
        Assert.That(aliasNode, Is.Not.Null);
        Assert.That(aliasNode.Metadata, Is.EqualTo(alias).Using(new MetadataComparer()));

        var duNode = semanticTree.Find<DiscriminatedUnion>();
        Assert.That(duNode, Is.Not.Null);
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
        var arrayAccess = semanticTree.Find<ArrayAccessExpression>();
        Assert.That(arrayAccess, Is.Not.Null);
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
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void NewArrayReturnTypeTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public main(): i32[] {
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

        var expected = CreateArrayMetadata(compilationContext.BuiltInTypes.I32, compilationContext.RootNamespace);
        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var newArray = semanticTree.Find<NewArrayExpression>();
        Assert.That(newArray, Is.Not.Null);
        Assert.That(newArray.ReturnTypeMetadata, Is.EqualTo(expected).Using(new MetadataComparer()));
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

        var closedType = compilationContext.RootNamespace.FindType("List<i32>");
        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var genericTypeNode = semanticTree.Find<GenericApplication>();
        Assert.That(closedType, Is.Not.Null);
        Assert.That(genericTypeNode, Is.Not.Null);
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
                var x: Test<i32> = new Test<i32>();
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
        var ctor = closedType!.GetConstructor([]);

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var newObj = semanticTree.Find<NewObjectExpression>();
        Assert.That(newObj, Is.Not.Null);
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
                var x: Test<i32> = new Test<i32>();
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
        Assert.That(returnStmt, Is.Not.Null);
        Assert.That(returnStmt.Expression, Is.Not.Null);
        Assert.That(
            returnStmt.Expression.ReturnTypeMetadata,
            Is.EqualTo(compilationContext.BuiltInTypes.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void MemberAccessNestedCallTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Test1 {
                public b(): Test2 {
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

        Assert.That(
            () => semantic.Analyze(
                project,
                new SemanticAnalysisOptions(new HashSet<string>(), diagnostics, compilationContext)),
            Throws.Nothing);
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
        var type = test1Ns.FindType("Test");
        Assert.That(type, Is.Not.Null);

        var functionType = compilationContext.RootNamespace.FindType("() => void");
        Assert.That(functionType, Is.Not.Null);

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var staticTypeMember = semanticTree.Find<MemberAccessExpression>(x => x.Name == "Test");
        Assert.That(staticTypeMember, Is.Not.Null);
        Assert.That(staticTypeMember.ReturnTypeMetadata, Is.EqualTo(type));

        var member = semanticTree.Find<MemberAccessExpression>(x => x.Name == "test");
        Assert.That(member, Is.Not.Null);
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
            "The '(i32, string)' type doesn't have '2'.");

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
        var method = type.GetMember("prop")!;

        var semanticTree = project.SourceFiles.Single().SemanticTree!;
        var memberAccess = semanticTree.Find<MemberAccessExpression>();
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
            "No suitable overload found for 'f'.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }
}
using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;
using Trilang.Semantics.Model;
using Trilang.Symbols;

namespace Tri.Tests.Semantics;

public class SymbolFinderTests
{
    private static readonly SourceFile file = new SourceFile("test.tri");

    private static (SyntaxTree, DiagnosticCollection) Parse(string code)
    {
        var diagnostics = new DiagnosticCollection();

        var lexer = new Lexer();
        var lexerOptions = new LexerOptions(new LexerDiagnosticReporter(diagnostics, file));
        var tokens = lexer.Tokenize(code, lexerOptions);

        var parser = new Parser();
        var parserOptions = new ParserOptions(file, new ParserDiagnosticReporter(diagnostics, file));
        var tree = parser.Parse(tokens, parserOptions);

        return (tree, diagnostics);
    }

    [Test]
    public void FunctionInRootScopeTest()
    {
        var (tree, diagnostics) = Parse("public main(): void { }");

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var treeSymbolTable = map.Get(semanticTree);
        var function = semanticTree.Find<FunctionDeclaration>()!;
        Assert.That(treeSymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(treeSymbolTable.Ids, Contains.Key(function.Name).WithValue(new IdSymbol(function)));
    }

    [Test]
    public void TwoFunctionsInRootScopeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void { }
            public add(): void { }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var treeSymbolTable = map.Get(semanticTree);
        var function1 = semanticTree.Find<FunctionDeclaration>(x => x.Name == "main")!;
        var function2 = semanticTree.Find<FunctionDeclaration>(x => x.Name == "add")!;
        Assert.That(treeSymbolTable.Ids, Has.Count.EqualTo(2));
        Assert.That(
            treeSymbolTable.Ids,
            Contains.Key(function1.Name).WithValue(new IdSymbol(function1)));
        Assert.That(
            treeSymbolTable.Ids,
            Contains.Key(function2.Name).WithValue(new IdSymbol(function2)));
    }

    [Test]
    public void SameFunctionInRootScopeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void { }
            public main(): void { }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0002AlreadyDefined,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(24, 2, 1), new SourcePosition(47, 2, 24))),
            "The 'main' function is already defined.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void FunctionWithParametersInRootScopeTest()
    {
        var (tree, diagnostics) = Parse("public add(a: i32, b: i32): void { }");

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var function = semanticTree.Find<FunctionDeclaration>()!;
        var a = function.Parameters[0];
        var b = function.Parameters[1];

        var treeSymbolTable = map.Get(semanticTree);
        Assert.That(treeSymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(treeSymbolTable.Ids, Contains.Key("add").WithValue(new IdSymbol(function)));

        var functionBodySymbolTable = map.Get(function.Body);
        Assert.That(functionBodySymbolTable.Ids, Has.Count.EqualTo(2));
        Assert.That(functionBodySymbolTable.Ids, Contains.Key(a.Name).WithValue(new IdSymbol(a)));
        Assert.That(functionBodySymbolTable.Ids, Contains.Key(b.Name).WithValue(new IdSymbol(b)));
    }

    [Test]
    public void FunctionWithSameParametersInRootScopeTest()
    {
        var (tree, diagnostics) = Parse("public add(a: i32, a: i32): void { }");

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0002AlreadyDefined,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(19, 1, 20), new SourcePosition(25, 1, 26))),
            "The 'a' parameter is already defined.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void FunctionWithVariablesInRootScopeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var a: i32 = 1;
                var b: i32 = 2;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var function = semanticTree.Find<FunctionDeclaration>()!;
        var variables = semanticTree.Where<VariableDeclaration>().ToArray();
        var a = variables[0];
        var b = variables[1];

        var treeSymbolTable = map.Get(semanticTree);
        Assert.That(treeSymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(treeSymbolTable.Ids, Contains.Key(function.Name).WithValue(new IdSymbol(function)));

        var functionBodySymbolTable = map.Get(function.Body);
        Assert.That(functionBodySymbolTable.Ids, Has.Count.EqualTo(2));
        Assert.That(functionBodySymbolTable.Ids, Contains.Key(a.Name).WithValue(new IdSymbol(a)));
        Assert.That(functionBodySymbolTable.Ids, Contains.Key(b.Name).WithValue(new IdSymbol(b)));
    }

    [Test]
    public void FunctionWithSameVariablesInRootScopeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var a: i32 = 1;
                var a: i32 = 2;
            }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0002AlreadyDefined,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(46, 3, 5), new SourcePosition(61, 3, 20))),
            "The 'a' variable is already defined.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void IfScopeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                if (true) {
                    var a: i32 = 1;
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var function = semanticTree.Find<FunctionDeclaration>()!;
        var ifStatement = semanticTree.Find<IfStatement>()!;
        var a = semanticTree.Find<VariableDeclaration>()!;

        var treeSymbolTable = map.Get(semanticTree);
        Assert.That(treeSymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(treeSymbolTable.Ids, Contains.Key(function.Name).WithValue(new IdSymbol(function)));

        var thenSymbolTable = map.Get(ifStatement.Then);
        Assert.That(thenSymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(thenSymbolTable.Ids, Contains.Key(a.Name).WithValue(new IdSymbol(a)));
    }

    [Test]
    public void IfElseScopeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                if (true) {
                    var a: i32 = 1;
                } else {
                    var b: i32 = 1;
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var function = semanticTree.Find<FunctionDeclaration>()!;
        var ifStatement = semanticTree.Find<IfStatement>()!;
        var variables = semanticTree.Where<VariableDeclaration>().ToArray();
        var a = variables[0];
        var b = variables[1];

        var treeSymbolTable = map.Get(semanticTree);
        Assert.That(treeSymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(treeSymbolTable.Ids, Contains.Key(function.Name).WithValue(new IdSymbol(function)));

        var thenSymbolTable = map.Get(ifStatement.Then);
        Assert.That(thenSymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(thenSymbolTable.Ids, Contains.Key(a.Name).WithValue(new IdSymbol(a)));

        var elseSymbolTable = map.Get(ifStatement.Else!);
        Assert.That(elseSymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(elseSymbolTable.Ids, Contains.Key(b.Name).WithValue(new IdSymbol(b)));
    }

    [Test]
    public void SameVariableInMultipleScopesTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public main(): void {
                var a: i32 = 1;
                if (true) {
                    var a: i32 = 1;
                } else {
                    var a: i32 = 1;
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var function = semanticTree.Find<FunctionDeclaration>()!;
        var ifStatement = semanticTree.Find<IfStatement>()!;
        var variables = semanticTree.Where<VariableDeclaration>().ToArray();
        var a1 = variables[0];
        var a2 = variables[1];
        var a3 = variables[2];

        var treeSymbolTable = map.Get(semanticTree);
        Assert.That(treeSymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(treeSymbolTable.Ids, Contains.Key(function.Name).WithValue(new IdSymbol(function)));

        var functionBodySymbolTable = map.Get(function.Body);
        Assert.That(functionBodySymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(functionBodySymbolTable.Ids, Contains.Key(a1.Name).WithValue(new IdSymbol(a1)));

        var thenSymbolTable = map.Get(ifStatement.Then);
        Assert.That(thenSymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(thenSymbolTable.Ids, Contains.Key(a2.Name).WithValue(new IdSymbol(a2)));

        var elseSymbolTable = map.Get(ifStatement.Else!);
        Assert.That(elseSymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(elseSymbolTable.Ids, Contains.Key(a3.Name).WithValue(new IdSymbol(a3)));
    }

    [Test]
    public void ArrayTypeTest()
    {
        var (tree, diagnostics) = Parse("public main(): i32[] { return new i32[0]; }");

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var arrayTypeNode = semanticTree.Find<ArrayType>();
        Assert.That(arrayTypeNode, Is.Not.Null);

        var treeSymbolTable = map.Get(semanticTree);
        var symbol = TypeSymbol.Array(arrayTypeNode);
        Assert.That(treeSymbolTable.Types, Has.Count.EqualTo(1));
        Assert.That(treeSymbolTable.Types, Contains.Key("i32[]").WithValue(symbol));
    }

    [Test]
    public void TypeDeclarationTest()
    {
        var (tree, diagnostics) = Parse("public type Point { }");

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var type = semanticTree.Find<TypeDeclaration>()!;

        var treeSymbolTable = map.Get(semanticTree);
        Assert.That(treeSymbolTable.Types, Has.Count.EqualTo(1));
        Assert.That(treeSymbolTable.Types, Contains.Key("Point").WithValue(TypeSymbol.Type(type)));
    }

    [Test]
    public void TypeDeclarationDuplicateTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point { }
            public type Point { }
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0002AlreadyDefined,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(22, 2, 1), new SourcePosition(43, 2, 22))),
            "The 'Point' type is already defined.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void CtorDeclarationVariableTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public constructor(a: i32) { }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var type = semanticTree.Find<TypeDeclaration>()!;
        var ctor = semanticTree.Find<ConstructorDeclaration>()!;
        var parameter = ctor.Parameters[0];

        var ctorSymbolTable = map.Get(ctor.Body);
        Assert.That(ctorSymbolTable.Ids, Has.Count.EqualTo(2));
        Assert.That(ctorSymbolTable.Ids, Contains.Key("this").WithValue(new IdSymbol("this", type)));
        Assert.That(ctorSymbolTable.Ids, Contains.Key(parameter.Name).WithValue(new IdSymbol(parameter)));
    }

    [Test]
    public void MethodDeclarationVariableTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                public test(a: i32): void { }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var type = semanticTree.Find<TypeDeclaration>()!;
        var method = semanticTree.Find<MethodDeclaration>()!;
        var parameter = method.Parameters[0];

        var methodSymbolTable = map.Get(method.Body);
        Assert.That(methodSymbolTable.Ids, Has.Count.EqualTo(2));
        Assert.That(methodSymbolTable.Ids, Contains.Key("this").WithValue(new IdSymbol("this", type)));
        Assert.That(methodSymbolTable.Ids, Contains.Key(parameter.Name).WithValue(new IdSymbol(parameter)));
    }

    [Test]
    public void TypeAliasTest()
    {
        var (tree, diagnostics) = Parse("public type MyInt = i32;");

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var type = semanticTree.Find<TypeAliasDeclaration>()!;

        var treeSymbolTable = map.Get(semanticTree);
        Assert.That(treeSymbolTable.Types, Has.Count.EqualTo(1));
        Assert.That(treeSymbolTable.Types, Contains.Key(type.Name).WithValue(TypeSymbol.Alias(type)));
    }

    [Test]
    public void TypeAliasDuplicateTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type MyInt = i32;
            public type MyInt = i32;
            """);

        var semantic = new SemanticAnalysis();
        semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var diagnostic = new Diagnostic(
            DiagnosticId.S0002AlreadyDefined,
            DiagnosticSeverity.Error,
            new SourceLocation(
                file,
                new SourceSpan(new SourcePosition(25, 2, 1), new SourcePosition(49, 2, 25))),
            "The 'MyInt' type is already defined.");

        Assert.That(diagnostics.Diagnostics, Is.EqualTo([diagnostic]));
    }

    [Test]
    public void FunctionTypeAliasTest()
    {
        var (tree, diagnostics) = Parse("public type F = (i32, i32) => i32;");

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var aliasType = semanticTree.Find<TypeAliasDeclaration>()!;
        var type = semanticTree.Find<FunctionType>()!;

        var treeSymbolTable = map.Get(semanticTree);
        Assert.That(treeSymbolTable.Types, Has.Count.EqualTo(2));
        Assert.That(
            treeSymbolTable.Types,
            Contains.Key(type.Name).WithValue(TypeSymbol.FunctionType(type)));
        Assert.That(
            treeSymbolTable.Types,
            Contains.Key(aliasType.Name).WithValue(TypeSymbol.Alias(aliasType)));
    }

    [Test]
    public void TypeIdsInSymbolTableTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point {
                x: i32;
                y: i32;

                public toString(): string {
                    return "pew";
                }

                public distance(other: Point): i32 {
                    return 0;
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var type = semanticTree.Find<TypeDeclaration>()!;

        var treeSymbolTable = map.Get(semanticTree);
        Assert.That(treeSymbolTable.Types, Has.Count.EqualTo(1));
        Assert.That(treeSymbolTable.Types, Contains.Key(type.Name).WithValue(TypeSymbol.Type(type)));

        var typeSymbolTable = map.Get(type);
        Assert.That(typeSymbolTable.Ids, Has.Count.EqualTo(4));
        Assert.That(typeSymbolTable.Ids, Contains.Key("x").WithValue(new IdSymbol(type.Properties[0])));
        Assert.That(typeSymbolTable.Ids, Contains.Key("y").WithValue(new IdSymbol(type.Properties[1])));
        Assert.That(typeSymbolTable.Ids, Contains.Key("toString").WithValue(new IdSymbol(type.Methods[0])));
        Assert.That(typeSymbolTable.Ids, Contains.Key("distance").WithValue(new IdSymbol(type.Methods[1])));
    }

    [Test]
    public void InterfaceIdsInSymbolTableTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point = {
                x: i32;
                y: i32;
                toString(): string;
                distance(Point): f32;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var alias = semanticTree.Find<TypeAliasDeclaration>()!;
        var @interface = semanticTree.Find<Interface>()!;

        var treeSymbolTable = map.Get(semanticTree);
        Assert.That(treeSymbolTable.Types, Has.Count.EqualTo(2));
        Assert.That(treeSymbolTable.Types, Contains.Key(@interface.Name).WithValue(TypeSymbol.Interface(@interface)));
        Assert.That(treeSymbolTable.Types, Contains.Key(alias.Name).WithValue(TypeSymbol.Alias(alias)));

        var interfaceSymbolTable = map.Get(@interface);
        Assert.That(interfaceSymbolTable, Is.Not.Null);
        Assert.That(interfaceSymbolTable.Ids, Has.Count.EqualTo(4));
        Assert.That(interfaceSymbolTable.Ids, Contains.Key("x").WithValue(new IdSymbol(@interface.Properties[0])));
        Assert.That(interfaceSymbolTable.Ids, Contains.Key("y").WithValue(new IdSymbol(@interface.Properties[1])));
        Assert.That(interfaceSymbolTable.Ids, Contains.Key("toString").WithValue(new IdSymbol(@interface.Methods[0])));
        Assert.That(interfaceSymbolTable.Ids, Contains.Key("distance").WithValue(new IdSymbol(@interface.Methods[1])));
    }

    [Test]
    public void InlineFunctionTypeInInterfaceTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Point = {
                x: () => void;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var alias = semanticTree.Find<TypeAliasDeclaration>()!;
        var @interface = semanticTree.Find<Interface>()!;

        var treeSymbolTable = map.Get(semanticTree);
        Assert.That(treeSymbolTable.Types, Has.Count.EqualTo(3));
        Assert.That(treeSymbolTable.Types, Contains.Key(@interface.Name).WithValue(TypeSymbol.Interface(@interface)));
        Assert.That(treeSymbolTable.Types, Contains.Key(alias.Name).WithValue(TypeSymbol.Alias(alias)));
    }

    [Test]
    public void DiscriminatedUnionTest()
    {
        var (tree, diagnostics) = Parse("public type T = {} | i32 | () => void;");

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var alias = semanticTree.Find<TypeAliasDeclaration>()!;
        var discriminatedUnionNode = semanticTree.Find<DiscriminatedUnion>()!;

        var treeSymbolTable = map.Get(semanticTree);
        Assert.That(treeSymbolTable.Types, Has.Count.EqualTo(4));
        Assert.That(
            treeSymbolTable.Types,
            Contains.Key(discriminatedUnionNode.Name)
                .WithValue(TypeSymbol.DiscriminatedUnion(discriminatedUnionNode)));
        Assert.That(
            treeSymbolTable.Types,
            Contains.Key(alias.Name).WithValue(TypeSymbol.Alias(alias)));
    }

    [Test]
    public void TupleTypeTest()
    {
        var (tree, diagnostics) = Parse("public type T = (i32, bool);");

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var aliasNode = semanticTree.Find<TypeAliasDeclaration>();
        var tupleNode = semanticTree.Find<TupleType>();
        Assert.That(aliasNode, Is.Not.Null);
        Assert.That(tupleNode, Is.Not.Null);

        var treeSymbolTable = map.Get(semanticTree);
        Assert.That(treeSymbolTable.Types, Has.Count.EqualTo(2));
        Assert.That(
            treeSymbolTable.Types,
            Contains.Key("(i32, bool)").WithValue(TypeSymbol.Tuple(tupleNode)));
        Assert.That(
            treeSymbolTable.Types,
            Contains.Key("T").WithValue(TypeSymbol.Alias(aliasNode)));
    }

    [Test]
    public void NestedTupleTypeTest()
    {
        var (tree, diagnostics) = Parse("public type T = ((i32, i32), bool);");

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var aliasNode = semanticTree.Find<TypeAliasDeclaration>();
        var tupleNode = semanticTree.Find<TupleType>(x => x.Name == "((i32, i32), bool)");
        var nestedTupleNode = semanticTree.Find<TupleType>(x => x.Name == "(i32, i32)");

        Assert.That(aliasNode, Is.Not.Null);
        Assert.That(tupleNode, Is.Not.Null);
        Assert.That(nestedTupleNode, Is.Not.Null);

        var treeSymbolTable = map.Get(semanticTree);
        Assert.That(treeSymbolTable.Types, Has.Count.EqualTo(3));
        Assert.That(
            treeSymbolTable.Types,
            Contains.Key("(i32, i32)").WithValue(TypeSymbol.Tuple(nestedTupleNode)));
        Assert.That(
            treeSymbolTable.Types,
            Contains.Key("((i32, i32), bool)").WithValue(TypeSymbol.Tuple(tupleNode)));
        Assert.That(
            treeSymbolTable.Types,
            Contains.Key("T").WithValue(TypeSymbol.Alias(aliasNode)));
    }

    [Test]
    public void FieldInGetterTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type Test {
                x: i32 {
                    private get {
                        return field;
                    }

                    private set {
                        field = value;
                    }
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var property = semanticTree.Find<PropertyDeclaration>();
        var getter = semanticTree.Find<PropertyGetter>();
        Assert.That(getter, Is.Not.Null);

        var getterSymbolTable = map.Get(getter.Body!);
        Assert.That(getterSymbolTable.Ids, Has.Count.EqualTo(1));
        Assert.That(getterSymbolTable.Ids, Contains.Key("field").WithValue(new IdSymbol("field", property)));

        var setter = semanticTree.Find<PropertySetter>();
        Assert.That(setter, Is.Not.Null);

        var setterSymbolTable = map.Get(setter.Body!);
        Assert.That(setterSymbolTable.Ids, Has.Count.EqualTo(2));
        Assert.That(setterSymbolTable.Ids, Contains.Key("field").WithValue(new IdSymbol("field", property)));
        Assert.That(setterSymbolTable.Ids, Contains.Key("value").WithValue(new IdSymbol("value", property)));
    }

    [Test]
    public void GenericTypeTest()
    {
        var (tree, diagnostics) = Parse("public type List<T> { }");

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var type = semanticTree.Find<TypeDeclaration>();
        Assert.That(type, Is.Not.Null);

        var typeSymbolTable = map.Get(type);
        Assert.That(typeSymbolTable.Types, Has.Count.EqualTo(1));
        Assert.That(typeSymbolTable.Types, Contains.Key("List<>").WithValue(TypeSymbol.OpenGenericType(type)));
    }

    [Test]
    public void GenericTypeWithMultipleTypeArgumentsTest()
    {
        var (tree, diagnostics) = Parse("public type Test<T1, T2> { }");

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var type = semanticTree.Find<TypeDeclaration>();
        Assert.That(type, Is.Not.Null);

        var typeSymbolTable = map.Get(type);
        Assert.That(typeSymbolTable, Is.Not.Null);
        Assert.That(typeSymbolTable.Types, Has.Count.EqualTo(1));
        Assert.That(typeSymbolTable.Types, Contains.Key("Test<,>").WithValue(TypeSymbol.OpenGenericType(type)));
    }

    [Test]
    public void SymbolForClosedGenericTypeTest()
    {
        var (tree, diagnostics) = Parse(
            """
            public type List<T> { }

            public type Test = List<i32>;
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTrees, map, _, _) = semantic.Analyze(
            [tree],
            new SemanticAnalysisOptions([], new SemanticDiagnosticReporter(diagnostics)));

        var semanticTree = semanticTrees.Single();
        var alias = semanticTree.Find<TypeAliasDeclaration>();
        Assert.That(alias, Is.Not.Null);

        var closedGeneric = semanticTree.Find<GenericType>();
        Assert.That(closedGeneric, Is.Not.Null);

        var treeSymbolTable = map.Get(semanticTree);
        Assert.That(treeSymbolTable.Types, Has.Count.EqualTo(3));
        Assert.That(
            treeSymbolTable.Types,
            Contains.Key("Test").WithValue(TypeSymbol.Alias(alias)));
        Assert.That(
            treeSymbolTable.Types,
            Contains.Key("List<i32>").WithValue(TypeSymbol.GenericType(closedGeneric)));
    }
}
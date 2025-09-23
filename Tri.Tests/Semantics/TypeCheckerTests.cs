using Trilang;
using Trilang.Compilation.Diagnostics;
using Trilang.Lexing;
using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using Trilang.Semantics;
using Trilang.Semantics.Model;

namespace Tri.Tests.Semantics;

public class TypeCheckerTests
{
    private static SyntaxTree Parse(string code)
    {
        var diagnostics = new DiagnosticCollection();

        var lexer = new Lexer();
        var tokens = lexer.Tokenize(code, new LexerOptions(diagnostics.Lexer));

        var parser = new Parser();
        var tree = parser.Parse(tokens, new ParserOptions(diagnostics.Parser));

        return tree;
    }

    [Test]
    public void SetMetadataForFunctionReturnTypeTest()
    {
        var tree = Parse(
            """
            public main(): void {
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var expected = new FunctionMetadata(
            AccessModifierMetadata.Public,
            "main",
            [],
            new FunctionTypeMetadata([], TypeMetadata.Void));

        var function = semanticTree.Find<FunctionDeclaration>();
        Assert.That(function, Is.Not.Null);
        Assert.That(function.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForFunctionParameterTypesTest()
    {
        var tree = Parse("public main(a: i32, b: bool): void { }");

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var expected = new FunctionMetadata(
            AccessModifierMetadata.Public,
            "main",
            [
                new ParameterMetadata("a", TypeMetadata.I32),
                new ParameterMetadata("b", TypeMetadata.Bool)
            ],
            new FunctionTypeMetadata([TypeMetadata.I32, TypeMetadata.Bool], TypeMetadata.Void));

        var function = semanticTree.Find<FunctionDeclaration>();
        Assert.That(function, Is.Not.Null);
        Assert.That(function.Metadata, Is.EqualTo(expected));
        Assert.That(
            function.Parameters[0].Type.Metadata,
            Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
        Assert.That(
            function.Parameters[1].Type.Metadata,
            Is.EqualTo(TypeMetadata.Bool).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForVariableTypeTest()
    {
        var tree = Parse(
            """
            public main(): void {
                var a: i32 = 1;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var variable = semanticTree.Find<VariableDeclaration>();
        Assert.That(variable, Is.Not.Null);
        Assert.That(variable.Type.Metadata, Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForIncorrectVariableTypeTest()
    {
        var tree = Parse(
            """
            public main(): void {
                var a: xxx = 1;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Referenced unknown type 'xxx'"));
    }

    [Test]
    public void SetMetadataForTypeTest()
    {
        var tree = Parse(
            """
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

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var expected = new TypeMetadata("Point");
        expected.AddConstructor(
            new ConstructorMetadata(
                expected,
                AccessModifierMetadata.Public,
                [],
                new FunctionTypeMetadata([], expected)));

        var xProperty = new PropertyMetadata(
            expected,
            "x",
            TypeMetadata.I32);
        expected.AddProperty(xProperty);
        expected.AddMethod(xProperty.Getter!);
        expected.AddMethod(xProperty.Setter!);

        var yProperty = new PropertyMetadata(
            expected,
            "y",
            TypeMetadata.I32);
        expected.AddProperty(yProperty);
        expected.AddMethod(yProperty.Getter!);
        expected.AddMethod(yProperty.Setter!);

        expected.AddMethod(new MethodMetadata(
            expected,
            AccessModifierMetadata.Public,
            false,
            "toString",
            [],
            new FunctionTypeMetadata([], TypeMetadata.Void)));
        expected.AddMethod(new MethodMetadata(
            expected,
            AccessModifierMetadata.Public,
            false,
            "distance",
            [new ParameterMetadata("other", TypeMetadata.I32)],
            new FunctionTypeMetadata([TypeMetadata.I32], TypeMetadata.I32)));

        var type = semanticTree.Find<TypeDeclaration>();
        Assert.That(type, Is.Not.Null);
        Assert.That(type.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForAliasType()
    {
        var tree = Parse("public type MyInt = i32;");

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var expected = new TypeAliasMetadata("MyInt", [], TypeMetadata.I32);
        var node = semanticTree.Find<TypeAliasDeclaration>();
        Assert.That(node, Is.Not.Null);
        Assert.That(node.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForFunctionTypeTest()
    {
        var tree = Parse("public type MyF = (i32, bool) => f64;");

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var expected = new FunctionTypeMetadata([TypeMetadata.I32, TypeMetadata.Bool], TypeMetadata.F64);
        var type = semanticTree.Find<FunctionType>();
        Assert.That(type, Is.Not.Null);
        Assert.That(type.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForFunctionTest()
    {
        var tree = Parse(
            """
            public add(a: i32, b: i32): i32 {
                return 0;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var expected = new FunctionMetadata(
            AccessModifierMetadata.Public,
            "add",
            [
                new ParameterMetadata("a", TypeMetadata.I32),
                new ParameterMetadata("b", TypeMetadata.I32)
            ],
            new FunctionTypeMetadata([TypeMetadata.I32, TypeMetadata.I32], TypeMetadata.I32));

        var node = semanticTree.Find<FunctionDeclaration>();
        Assert.That(node, Is.Not.Null);
        Assert.That(node.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForForwardDefinedFunctionTest()
    {
        var tree = Parse(
            """
            public test1(): void {
                test2();
            }

            public test2(): void { }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var expected = new FunctionMetadata(
            AccessModifierMetadata.Public,
            "test2",
            [],
            new FunctionTypeMetadata([], TypeMetadata.Void));

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
        var tree = Parse(
            """
            public main(): i32 {
                return 1;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void LiteralFloatTest()
    {
        var tree = Parse(
            """
            public main(): f64 {
                return 3.14;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(TypeMetadata.F64).Using(new MetadataComparer()));
    }

    [Test]
    public void LiteralBoolTest()
    {
        var tree = Parse(
            """
            public main(): bool {
                return true;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(TypeMetadata.Bool).Using(new MetadataComparer()));
    }

    [Test]
    public void LiteralCharTest()
    {
        var tree = Parse(
            """
            public main(): char {
                return 'x';
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(TypeMetadata.Char).Using(new MetadataComparer()));
    }

    [Test]
    public void LiteralStringTest()
    {
        var tree = Parse(
            """
            public main(): string {
                return "xxx";
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(TypeMetadata.String).Using(new MetadataComparer()));
    }

    [Test]
    public void ReturnStatementDoesntTest()
    {
        var tree = Parse(
            """
            public main(): bool {
                return 1;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Function return type mismatch: expected 'bool', got 'i32'"));
    }

    [Test]
    public void UnaryPlusTest()
    {
        var tree = Parse(
            """
            public main(): i32 {
                return -1;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void UnaryMinusTest()
    {
        var tree = Parse(
            """
            public main(): i32 {
                return -1;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void LogicalNotTest()
    {
        var tree = Parse(
            """
            public main(): bool {
                return !true;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(TypeMetadata.Bool).Using(new MetadataComparer()));
    }

    [Test]
    public void BinaryExpressionTest()
    {
        var tree = Parse(
            """
            public main(): i32 {
                return 1 + 2;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var binaryNode = semanticTree.Find<BinaryExpression>();
        Assert.That(binaryNode, Is.Not.Null);
        Assert.That(
            binaryNode.ReturnTypeMetadata,
            Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void LogicalNotIncorrectOperandTest()
    {
        var tree = Parse(
            """
            public main(): i32 {
                return !1;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Invalid unary expression: incompatible operand type 'i32' for operator 'LogicalNot'"));
    }

    [Test]
    public void VariableExpressionTest()
    {
        var tree = Parse(
            """
            public main(a: i32): i32 {
                return a;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var returnNode = semanticTree.Find<ReturnStatement>();
        Assert.That(returnNode, Is.Not.Null);
        Assert.That(returnNode.Expression, Is.Not.Null);
        Assert.That(
            returnNode.Expression.ReturnTypeMetadata,
            Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void VariableDeclarationIncorrectTypesTest()
    {
        var tree = Parse(
            """
            public main(): void {
                var a: i32 = true;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Type mismatch in variable declaration 'a': expected 'i32', got 'bool'"));
    }

    [Test]
    public void IfIncorrectConditionTest()
    {
        var tree = Parse(
            """
            public main(): void {
                if (1) {
                }
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The condition returns non-boolean type."));
    }

    [Test]
    public void FunctionCallIncorrectParameterTest()
    {
        var tree = Parse(
            """
            public add(a: i32): i32 {
            }

            public main(): i32 {
                return add(true);
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Expected 'i32' but got 'bool'"));
    }

    [Test]
    public void WhileNonBoolConditionTest()
    {
        var tree = Parse(
            """
            public main(): void {
                while (1) {
                }
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Condition must be a boolean"));
    }

    [Test]
    public void ReturnInConstructorTest()
    {
        var tree = Parse(
            """
            public type Point {
                public constructor() {
                    return;
                }
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(() => semantic.Analyze(tree, SemanticAnalysisOptions.Default), Throws.Nothing);
    }

    [Test]
    public void ReturnWithExpressionInConstructorTest()
    {
        var tree = Parse(
            """
            public type Point {
                public constructor() {
                    return 0;
                }
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Constructor return type mismatch: expected 'void', got 'i32'"));
    }

    [Test]
    public void SetMetadataForInterfaceTypeTest()
    {
        var tree = Parse(
            """
            public type Point = {
                x: i32;
                y: i32;
                distance(Point): f64;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var interfaceType = new InterfaceMetadata();
        var expected = new TypeAliasMetadata("Point", [], interfaceType);

        interfaceType.AddProperty(
            new InterfacePropertyMetadata(
                interfaceType,
                "x",
                TypeMetadata.I32,
                AccessModifierMetadata.Public,
                AccessModifierMetadata.Private));
        interfaceType.AddProperty(
            new InterfacePropertyMetadata(
                interfaceType,
                "y",
                TypeMetadata.I32,
                AccessModifierMetadata.Public,
                AccessModifierMetadata.Private));
        interfaceType.AddMethod(
            new InterfaceMethodMetadata(
                interfaceType,
                "distance",
                new FunctionTypeMetadata([expected], TypeMetadata.F64)));

        var type = semanticTree.Find<TypeAliasDeclaration>();
        Assert.That(type, Is.Not.Null);
        Assert.That(type.Metadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void SetReturnTypeForVariableWithFunctionTest()
    {
        var tree = Parse(
            """
            public add(a: i32, b: i32): i32 {
                return 1;
            }

            public main(): void {
                var x: (i32, i32) => i32 = add;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var expected = new FunctionTypeMetadata([TypeMetadata.I32, TypeMetadata.I32], TypeMetadata.I32);
        var memberAccess = semanticTree.Find<MemberAccessExpression>();
        Assert.That(memberAccess, Is.Not.Null);
        Assert.That(memberAccess.ReturnTypeMetadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void CallNonFunctionTest()
    {
        var tree = Parse(
            """
            public test(a: i32): void {
                a();
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Cannot call a non-function member"));
    }

    [Test]
    public void ThisReturnTypeTest()
    {
        var tree = Parse(
            """
            public type Point {
                public toString(): void {
                    this;
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var thisNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "this");
        var pointType = typeProvider.GetType("Point");
        Assert.That(thisNode, Is.Not.Null);
        Assert.That(thisNode.ReturnTypeMetadata, Is.EqualTo(pointType).Using(new MetadataComparer()));
    }

    [Test]
    public void ThisWithMultipleMembersReturnTypeTest()
    {
        var tree = Parse(
            """
            public type Point {
                a: i32;

                public toString(): void {
                    this.a;
                }
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var thisNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "a");
        Assert.That(thisNode, Is.Not.Null);
        Assert.That(thisNode.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void ThisWithIncorrectPropertyNameTest()
    {
        var tree = Parse(
            """
            public type Point {
                a: i32;

                public toString(): void {
                    this.x;
                }
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Cannot find member 'x' in 'Point'"));
    }

    [Test]
    public void InterfaceMemberAccessReturnTypeTest()
    {
        var tree = Parse(
            """
            public type Point = {
                x: i32;
            }

            public test(a: Point): i32 {
                return a.x;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var aNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "a");
        var pointType = typeProvider.GetType("Point");
        Assert.That(aNode, Is.Not.Null);
        Assert.That(aNode.ReturnTypeMetadata, Is.EqualTo(pointType).Using(new MetadataComparer()));

        var xNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "x");
        Assert.That(xNode, Is.Not.Null);
        Assert.That(xNode.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void InterfaceMemberAccessIncorrectPropertyTest()
    {
        var tree = Parse(
            """
            public type Point = {
                x: i32;
            }

            public test(a: Point): i32 {
                return a.c;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Cannot find member 'c' in 'Point'"));
    }

    [Test]
    public void AliasFunctionTypeMemberAccessReturnTypeTest()
    {
        var tree = Parse(
            """
            public type F = () => void;

            public type Test {
                f: F;
            }

            public test(a: Test): F {
                return a.f;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var aNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "a");
        var pointType = typeProvider.GetType("Test");
        Assert.That(aNode, Is.Not.Null);
        Assert.That(aNode.ReturnTypeMetadata, Is.EqualTo(pointType).Using(new MetadataComparer()));

        var xNode = semanticTree.Find<MemberAccessExpression>(m => m.Name == "f");
        var functionType = typeProvider.GetType("F");
        Assert.That(xNode, Is.Not.Null);
        Assert.That(xNode.ReturnTypeMetadata, Is.EqualTo(functionType).Using(new MetadataComparer()));
    }

    [Test]
    public void NewOperatorSetCtorTest()
    {
        var tree = Parse(
            """
            public type Point {
                public constructor(x: i32, y: i32) {
                }
            }

            public test(): void {
                var a: Point = new Point(1, 2);
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = typeProvider.GetType("Point") as TypeMetadata;
        Assert.That(type, Is.Not.Null);

        var ctor = type.GetConstructor([TypeMetadata.I32, TypeMetadata.I32]);
        Assert.That(ctor, Is.Not.Null);

        var newOp = semanticTree.Find<NewObjectExpression>();
        Assert.That(newOp, Is.Not.Null);
        Assert.That(newOp.Metadata, Is.EqualTo(ctor));
    }

    [Test]
    public void NewOperatorForInterfaceTest()
    {
        var tree = Parse(
            """
            public type Point = {
                x: i32;
                y: i32;
            }

            public test(): void {
                var a: Point = new Point(1, 2);
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Cannot create an instance of type 'Point'"));
    }

    [Test]
    public void NewOperatorMissingConstructorTest()
    {
        var tree = Parse(
            """
            public type Point {
                public constructor(x: i32, y: i32) {
                }
            }

            public test(): void {
                var a: Point = new Point(2);
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'Point' type doesn't have 'i32' constructor."));
    }

    [Test]
    public void SetMetadataForDiscriminatedUnionTest()
    {
        var tree = Parse(
            """
            public type DU = {} | i32 | () => void;
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var du = new DiscriminatedUnionMetadata([
            new InterfaceMetadata(),
            TypeMetadata.I32,
            new FunctionTypeMetadata([], TypeMetadata.Void)
        ]);
        var alias = new TypeAliasMetadata("DU", [], du);

        var aliasNode = semanticTree.Find<TypeAliasDeclaration>();
        Assert.That(aliasNode, Is.Not.Null);
        Assert.That(aliasNode.Metadata, Is.EqualTo(alias).Using(new MetadataComparer()));

        var duNode = semanticTree.Find<DiscriminatedUnion>();
        Assert.That(duNode, Is.Not.Null);
        Assert.That(duNode.Metadata, Is.EqualTo(du).Using(new MetadataComparer()));
    }

    [Test]
    public void AccessArrayElementTest()
    {
        var tree = Parse(
            """
            public test(a: i32[]): i32 {
                return a[1];
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var arrayAccess = semanticTree.Find<ArrayAccessExpression>();
        Assert.That(arrayAccess, Is.Not.Null);
        Assert.That(arrayAccess.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void UseArrayAccessorOnNotArrayTest()
    {
        var tree = Parse(
            """
            public test(a: i32): void {
                return a[1];
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Array access must be of type array"));
    }

    [Test]
    public void UseArrayAccessorWithNonNumberTest()
    {
        var tree = Parse(
            """
            public test(a: i32[]): void {
                return a["xxx"];
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Array index must be of type i32"));
    }

    [Test]
    public void GenerateMetadataForExpressionTupleTest()
    {
        var tree = Parse(
            """
            public test(): (i32, i32) {
                return (1, 2);
            }
            """);

        var semantic = new SemanticAnalysis();
        var (_, _, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var expected = new TupleMetadata([TypeMetadata.I32, TypeMetadata.I32]);

        var actual = typeProvider.GetType("(i32, i32)");
        Assert.That(actual, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void NewArrayReturnTypeTest()
    {
        var tree = Parse(
            """
            public main(): i32[] {
                return new i32[10];
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var expected = new TypeArrayMetadata(TypeMetadata.I32);

        var newArray = semanticTree.Find<NewArrayExpression>();
        Assert.That(newArray, Is.Not.Null);
        Assert.That(newArray.ReturnTypeMetadata, Is.EqualTo(expected).Using(new MetadataComparer()));
    }

    [Test]
    public void GenericPropertyTest()
    {
        var tree = Parse(
            """
            public type Test<T> {
                x: T;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var property = semanticTree.Find<PropertyDeclaration>();
        Assert.That(property, Is.Not.Null);
        Assert.That(property.Metadata, Is.Not.Null);
        Assert.That(
            property.Metadata.Type,
            Is.EqualTo(new TypeArgumentMetadata("T")).Using(new MetadataComparer()));
    }

    [Test]
    public void GenericArrayPropertyTest()
    {
        var tree = Parse(
            """
            public type Test<T> {
                x: T[];
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var property = semanticTree.Find<PropertyDeclaration>();
        Assert.That(property, Is.Not.Null);
        Assert.That(property.Metadata, Is.Not.Null);

        var typeArrayMetadata = new TypeArrayMetadata(new TypeArgumentMetadata("T"));
        Assert.That(property.Metadata.Type, Is.EqualTo(typeArrayMetadata).Using(new MetadataComparer()));
    }

    [Test]
    public void SetMetadataForClosedGenericTypeTest()
    {
        var tree = Parse(
            """
            public type List<T> {}
            public type Test = List<i32>;
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var closedType = typeProvider.GetType("List<i32>");
        var genericTypeNode = semanticTree.Find<GenericType>();
        Assert.That(closedType, Is.Not.Null);
        Assert.That(genericTypeNode, Is.Not.Null);
        Assert.That(genericTypeNode.Metadata, Is.EqualTo(closedType).Using(new MetadataComparer()));
    }

    [Test]
    public void FindCtorInGenericTypeTest()
    {
        var tree = Parse(
            """
            public type Test<T> {
                a: T;
            }

            public main(): void {
                var x: Test<i32> = new Test<i32>();
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var closedType = typeProvider.GetType("Test<i32>") as TypeMetadata;
        var ctor = closedType!.GetConstructor([]);

        var newObj = semanticTree.Find<NewObjectExpression>();
        Assert.That(newObj, Is.Not.Null);
        Assert.That(newObj.Metadata, Is.EqualTo(ctor));
    }

    [Test]
    public void SetMetadataForClosedGenericTypeFieldTest()
    {
        var tree = Parse(
            """
            public type Test<T> {
                a: T;
            }

            public main(): i32 {
                var x: Test<i32> = new Test<i32>();
                return x.a;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var returnStmt = semanticTree.Find<ReturnStatement>();
        Assert.That(returnStmt, Is.Not.Null);
        Assert.That(returnStmt.Expression, Is.Not.Null);
        Assert.That(
            returnStmt.Expression.ReturnTypeMetadata,
            Is.EqualTo(TypeMetadata.I32).Using(new MetadataComparer()));
    }

    [Test]
    public void MemberAccessNestedCallTest()
    {
        var tree = Parse(
            """
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

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.Nothing);
    }

    [Test]
    public void SetMetadataForStaticClassTest()
    {
        var tree = Parse(
            """
            public type Test {
                public static test(): void {
                }
            }

            public main(): void {
                Test.test();
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = typeProvider.GetType("Test");
        Assert.That(type, Is.Not.Null);

        var functionType = typeProvider.GetType("() => void");
        Assert.That(functionType, Is.Not.Null);

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
        var tree = Parse(
            """
            public test(a: (i32, string)): string {
                return a.1;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var tupleMember = semanticTree.Find<MemberAccessExpression>();
        Assert.That(tupleMember, Is.Not.Null);
        Assert.That(tupleMember.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.String).Using(new MetadataComparer()));
    }

    [Test]
    public void TupleMemberAccessIndexOutsideTest()
    {
        var tree = Parse(
            """
            public test(a: (i32, string)): string {
                return a.2;
            }
            """);

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree, SemanticAnalysisOptions.Default),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("Cannot find member '2' in '(i32, string)'")
        );
    }

    [Test]
    public void CastExpressionTest()
    {
        var tree = Parse(
            """
            public test(a: i32): i8 {
                return (i8)a;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, _, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var castExp = semanticTree.Find<CastExpression>();
        Assert.That(castExp, Is.Not.Null);
        Assert.That(castExp.ReturnTypeMetadata, Is.EqualTo(TypeMetadata.I8).Using(new MetadataComparer()));
    }

    [Test]
    public void UseMethodBeforeDeclarationTest()
    {
        var tree = Parse(
            """
            public type Test {
                public method1(): void {
                    method2();
                }

                public method2(): void {}
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = typeProvider.GetType("Test")!;
        var method = type.GetMember("method2")!;

        var memberAccess = semanticTree.Find<MemberAccessExpression>();
        Assert.That(memberAccess, Is.Not.Null);
        Assert.That(memberAccess.Reference, Is.EqualTo(method).Using(new MetadataComparer()));
    }

    [Test]
    public void UseMethodBeforeDeclarationInCtorTest()
    {
        var tree = Parse(
            """
            public type Test {
                public constructor() {
                    method2();
                }

                public method2(): void {}
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = typeProvider.GetType("Test")!;
        var method = type.GetMember("method2")!;

        var memberAccess = semanticTree.Find<MemberAccessExpression>();
        Assert.That(memberAccess, Is.Not.Null);
        Assert.That(memberAccess.Reference, Is.EqualTo(method).Using(new MetadataComparer()));
    }

    [Test]
    public void UsePropertyBeforeDeclarationTest()
    {
        var tree = Parse(
            """
            public type Test {
                public method1(): i32 {
                    return prop;
                }

                prop: i32;
            }
            """);

        var semantic = new SemanticAnalysis();
        var (semanticTree, _, typeProvider, _) = semantic.Analyze(tree, SemanticAnalysisOptions.Default);

        var type = typeProvider.GetType("Test")!;
        var method = type.GetMember("prop")!;

        var memberAccess = semanticTree.Find<MemberAccessExpression>();
        Assert.That(memberAccess, Is.Not.Null);
        Assert.That(memberAccess.Reference, Is.EqualTo(method).Using(new MetadataComparer()));
    }
}
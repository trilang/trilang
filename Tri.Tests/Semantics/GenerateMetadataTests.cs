using Tri.Tests.Builders;
using Trilang.Metadata;
using Trilang.Parsing.Ast;
using Trilang.Semantics;

namespace Tri.Tests.Semantics;

public class GenerateMetadataTests
{
    [Test]
    public void GenerateMetadataForTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineField("x", "i32")
                .DefineField("y", "i32")
                .DefineConstructor(b => b
                    .DefineParameter("x", "i32")
                    .DefineParameter("y", "i32"))
                .DefineMethod("toString", b => b
                    .ReturnType("string")
                    .Body(body => body.Return(r => r.String("hello"))))
                .DefineMethod("distance", b => b
                    .AccessModifier(AccessModifier.Private)
                    .DefineParameter("other", "i32")
                    .ReturnType("i32")
                    .Body(body => body.Return(r => r.Number(1)))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expected = new TypeMetadata("Point", [], [], [], []);
        expected.AddField(new FieldMetadata(
            expected,
            AccessModifierMetadata.Public,
            "x",
            TypeMetadata.I32));
        expected.AddField(new FieldMetadata(
            expected,
            AccessModifierMetadata.Public,
            "y",
            TypeMetadata.I32));
        expected.AddConstructor(new ConstructorMetadata(
            expected,
            AccessModifierMetadata.Public,
            [TypeMetadata.I32, TypeMetadata.I32]));
        expected.AddMethod(new MethodMetadata(
            expected,
            AccessModifierMetadata.Public,
            "toString",
            new FunctionTypeMetadata([], TypeMetadata.String)));
        expected.AddMethod(new MethodMetadata(
            expected,
            AccessModifierMetadata.Private,
            "distance",
            new FunctionTypeMetadata([TypeMetadata.I32], TypeMetadata.I32)));

        var actual = semantic.TypeProvider.GetType("Point");
        Assert.That(actual, Is.EqualTo(expected));

        var toStringType = semantic.TypeProvider.GetType("() => string");
        var expectedToStringType = new FunctionTypeMetadata([], TypeMetadata.String);
        Assert.That(toStringType, Is.EqualTo(expectedToStringType));

        var distanceType = semantic.TypeProvider.GetType("(i32) => i32");
        var expectedDistanceType = new FunctionTypeMetadata([TypeMetadata.I32], TypeMetadata.I32);
        Assert.That(distanceType, Is.EqualTo(expectedDistanceType));
    }

    [Test]
    public void GenerateMetadataForTypeWithInterfaceTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .AddInterface("Interface1")
                .AddInterface("Interface2"))
            .DefineAliasType("Interface1", builder => builder
                .Interface())
            .DefineAliasType("Interface2", builder => builder
                .Interface())
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var interface1Metadata = new InterfaceMetadata("Interface1");
        var interface2Metadata = new InterfaceMetadata("Interface2");
        var expected = new TypeMetadata(
            "Point",
            [interface1Metadata, interface2Metadata],
            [],
            [],
            []);

        var actual = semantic.TypeProvider.GetType("Point");
        Assert.That(actual, Is.EqualTo(expected));
    }

    [Test]
    public void GenerateMetadataForTypeMissingFieldTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineField("x", "xxx"))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'x' field has unknown type: 'xxx'."));
    }

    [Test]
    public void GenerateMetadataForTypeMissingParameterTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineMethod("distance", b => b
                    .DefineParameter("other", "xxx")
                    .ReturnType("f64")
                    .Body(body => body.Return())))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'other' parameter has unknown type: 'xxx'."));
    }

    [Test]
    public void GenerateMetadataForTypeMissingReturnTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point", builder => builder
                .DefineMethod("toString", b => b
                    .ReturnType("xxx")
                    .Body(body => body.Return())))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'toString' method has unknown return type: 'xxx'."));
    }

    [Test]
    public void GenerateMetadataForTypeAliasTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("MyInt", new TypeNode("i32"))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expected = new TypeAliasMetadata("MyInt", TypeMetadata.I32);

        var actual = semantic.TypeProvider.GetType("MyInt");
        Assert.That(actual, Is.EqualTo(expected));
    }

    [Test]
    public void GenerateMetadataForTypeAliasMissingTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("MyInt", new TypeNode("xxx"))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'xxx' aliased type is not defined."));
    }

    [Test]
    public void GenerateMetadataForTypeArrayTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("test", builder => builder
                .DefineParameter("arr", new TypeNode("i32[]"))
                .Body(body => body.Return()))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expected = new TypeArrayMetadata(TypeMetadata.I32);

        var actual = semantic.TypeProvider.GetType("i32[]");
        Assert.That(actual, Is.EqualTo(expected));
    }

    [Test]
    public void GenerateMetadataForTypeArrayMissingTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("test", builder => builder
                .DefineParameter("arr", new TypeNode("xxx"))
                .Body(body => body.Return()))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'arr' parameter has unknown type: 'xxx'."));
    }

    [Test]
    public void GenerateMetadataForFunctionTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("test", builder => builder
                .DefineParameter("a", "i32")
                .DefineParameter("b", "i32")
                .ReturnType("i32")
                .Body(body => body.Return(r => r.Number(1))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expected = new FunctionTypeMetadata([TypeMetadata.I32, TypeMetadata.I32], TypeMetadata.I32);
        var actual = semantic.TypeProvider.GetType("(i32, i32) => i32");
        Assert.That(actual, Is.EqualTo(expected));
    }

    [Test]
    public void GenerateMetadataForFunctionTypeMissingParameterTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("test", builder => builder
                .DefineParameter("a", "xxx")
                .Body(body => body.Return()))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The 'a' parameter has unknown type: 'xxx'."));
    }

    [Test]
    public void GenerateMetadataForFunctionTypeMissingReturnTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineFunction("test", builder => builder
                .ReturnType("xxx")
                .Body(body => body.Return()))
            .Build();

        var semantic = new SemanticAnalysis();

        Assert.That(
            () => semantic.Analyze(tree),
            Throws.TypeOf<SemanticAnalysisException>()
                .And.Message.EqualTo("The function has unknown return type: 'xxx'."));
    }

    [Test]
    public void GenerateMetadataForAliasAndTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point")
            .DefineAliasType("MyPoint", new TypeNode("Point"))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expectedType = new TypeMetadata("Point");
        var expectedAlias = new TypeAliasMetadata("MyPoint", expectedType);

        var actualType = semantic.TypeProvider.GetType("Point");
        var actualAlias = semantic.TypeProvider.GetType("MyPoint");

        Assert.That(actualType, Is.EqualTo(expectedType));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias));
    }

    [Test]
    public void GenerateMetadataForForwardRefAliasAndTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("MyPoint", new TypeNode("Point"))
            .DefineType("Point")
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expectedType = new TypeMetadata("Point");
        var expectedAlias = new TypeAliasMetadata("MyPoint", expectedType);

        var actualType = semantic.TypeProvider.GetType("Point");
        var actualAlias = semantic.TypeProvider.GetType("MyPoint");

        Assert.That(actualType, Is.EqualTo(expectedType));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias));
    }

    [Test]
    public void GenerateMetadataForAliasAndArrayTest()
    {
        var tree = new TreeBuilder()
            .DefineType("Point")
            .DefineAliasType("MyPoint", new TypeNode("Point[]"))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expectedType = new TypeMetadata("Point");
        var expectedArrayType = new TypeArrayMetadata(expectedType);
        var expectedAlias = new TypeAliasMetadata("MyPoint", expectedArrayType);

        var actualType = semantic.TypeProvider.GetType("Point");
        var actualArrayType = semantic.TypeProvider.GetType("Point[]");
        var actualAlias = semantic.TypeProvider.GetType("MyPoint");

        Assert.That(actualType, Is.EqualTo(expectedType));
        Assert.That(actualArrayType, Is.EqualTo(expectedArrayType));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias));
    }

    [Test]
    public void GenerateMetadataForForwardRefAliasAndArrayTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("MyPoint", new TypeNode("Point[]"))
            .DefineType("Point")
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expectedType = new TypeMetadata("Point");
        var expectedArrayType = new TypeArrayMetadata(expectedType);
        var expectedAlias = new TypeAliasMetadata("MyPoint", expectedArrayType);

        var actualType = semantic.TypeProvider.GetType("Point");
        var actualArrayType = semantic.TypeProvider.GetType("Point[]");
        var actualAlias = semantic.TypeProvider.GetType("MyPoint");

        Assert.That(actualType, Is.EqualTo(expectedType));
        Assert.That(actualArrayType, Is.EqualTo(expectedArrayType));
        Assert.That(actualAlias, Is.EqualTo(expectedAlias));
    }

    [Test]
    public void GenerateMetadataForInterfaceType()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Point", builder => builder
                .Interface(i => i
                    .DefineField("x", "i32")
                    .DefineField("y", "i32")
                    .DefineMethod("distance", m => m
                        .DefineParameter("other", "Point")
                        .ReturnType("f64"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var expectedInterface = new InterfaceMetadata("{ x: i32; y: i32; distance(Point): f64; }");
        var expectedAlias = new TypeAliasMetadata("Point", expectedInterface);

        var actualInterface = semantic.TypeProvider.GetType(expectedInterface.Name);
        Assert.That(actualInterface, Is.EqualTo(expectedInterface));

        var actualAlias = semantic.TypeProvider.GetType("Point");
        Assert.That(actualAlias, Is.EqualTo(expectedAlias));
    }

    [Test]
    public void GenerateMetadataForDiscriminatedUnionTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("DU", builder => builder
                .DiscriminatedUnion(du => du
                    .AddCase(c => c.Interface())
                    .AddCase(c => c.Type("i32"))
                    .AddCase(c => c.FunctionType(f => f.ReturnType("void")))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var du = new DiscriminatedUnionType("{ } | i32 | () => void");
        du.AddType(new InterfaceMetadata("{ }", [], []));
        du.AddType(TypeMetadata.I32);
        du.AddType(new FunctionTypeMetadata([], TypeMetadata.Void));
        var alias = new TypeAliasMetadata("DU", du);

        var actualAlias = semantic.TypeProvider.GetType("DU");
        Assert.That(actualAlias, Is.EqualTo(alias));

        var actualDu = semantic.TypeProvider.GetType("{ } | i32 | () => void");
        Assert.That(actualDu, Is.EqualTo(du));
    }

    [Test]
    public void GenerateMetadataForTupleTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Tuple", builder => builder
                .Tuple(t => t
                    .AddCase(c => c.Type("i32"))
                    .AddCase(c => c.Type("f64"))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var tuple = new TupleMetadata("(i32, f64)");
        tuple.AddType(TypeMetadata.I32);
        tuple.AddType(TypeMetadata.F64);

        var alias = new TypeAliasMetadata("Tuple", tuple);

        var actualAlias = semantic.TypeProvider.GetType("Tuple");
        Assert.That(actualAlias, Is.EqualTo(alias));

        var actualTuple = semantic.TypeProvider.GetType("(i32, f64)");
        Assert.That(actualTuple, Is.EqualTo(tuple));
    }

    [Test]
    public void GenerateMetadataForNestedTupleTypeTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Tuple", builder => builder
                .Tuple(t => t
                    .AddCase(c => c.Type("i32"))
                    .AddCase(c => c.Tuple(t2 => t2
                        .AddCase(c2 => c2.Type("f64"))
                        .AddCase(c2 => c2.Type("bool"))))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var nestedTuple = new TupleMetadata("(f64, bool)");
        nestedTuple.AddType(TypeMetadata.F64);
        nestedTuple.AddType(TypeMetadata.Bool);

        var tuple = new TupleMetadata("(i32, (f64, bool))");
        tuple.AddType(TypeMetadata.I32);
        tuple.AddType(nestedTuple);

        var alias = new TypeAliasMetadata("Tuple", tuple);

        var actualAlias = semantic.TypeProvider.GetType("Tuple");
        Assert.That(actualAlias, Is.EqualTo(alias));

        var actualTuple = semantic.TypeProvider.GetType("(i32, (f64, bool))");
        Assert.That(actualTuple, Is.EqualTo(tuple));

        var actualNestedTuple = semantic.TypeProvider.GetType("(f64, bool)");
        Assert.That(actualNestedTuple, Is.EqualTo(nestedTuple));
    }

    [Test]
    public void GenerateMetadataForDuInTupleTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Tuple", builder => builder
                .Tuple(t => t
                    .AddCase(c => c.Type("i32"))
                    .AddCase(c => c.DiscriminatedUnion(du => du
                        .AddCase(c2 => c2.Type("bool"))
                        .AddCase(c2 => c2.Type("i8"))))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var du = new DiscriminatedUnionType("bool | i8");
        du.AddType(TypeMetadata.Bool);
        du.AddType(TypeMetadata.I8);

        var tuple = new TupleMetadata("(i32, bool | i8)");
        tuple.AddType(TypeMetadata.I32);
        tuple.AddType(du);

        var alias = new TypeAliasMetadata("Tuple", tuple);

        var actualAlias = semantic.TypeProvider.GetType("Tuple");
        Assert.That(actualAlias, Is.EqualTo(alias));

        var actualTuple = semantic.TypeProvider.GetType("(i32, bool | i8)");
        Assert.That(actualTuple, Is.EqualTo(tuple));

        var actualDu = semantic.TypeProvider.GetType("bool | i8");
        Assert.That(actualDu, Is.EqualTo(du));
    }

    [Test]
    public void GenerateMetadataForTupleInDuTest()
    {
        var tree = new TreeBuilder()
            .DefineAliasType("Tuple", builder => builder
                .DiscriminatedUnion(du => du
                    .AddCase(c => c.Type("i32"))
                    .AddCase(c => c.Tuple(t => t
                        .AddCase(c2 => c2.Type("f64"))
                        .AddCase(c2 => c2.Type("bool"))))))
            .Build();

        var semantic = new SemanticAnalysis();
        semantic.Analyze(tree);

        var tuple = new TupleMetadata("(f64, bool)");
        tuple.AddType(TypeMetadata.F64);
        tuple.AddType(TypeMetadata.Bool);

        var du = new DiscriminatedUnionType("i32 | (f64, bool)");
        du.AddType(TypeMetadata.I32);
        du.AddType(tuple);

        var alias = new TypeAliasMetadata("Tuple", du);

        var actualAlias = semantic.TypeProvider.GetType("Tuple");
        Assert.That(actualAlias, Is.EqualTo(alias));

        var actualTuple = semantic.TypeProvider.GetType("(f64, bool)");
        Assert.That(actualTuple, Is.EqualTo(tuple));

        var actualDu = semantic.TypeProvider.GetType("i32 | (f64, bool)");
        Assert.That(actualDu, Is.EqualTo(du));
    }
}
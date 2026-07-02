using Trilang;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using static Tri.Tests.Factory;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Lowering;

public class ReplacePropertyFieldAndValueWithGeneratedFieldTests
{
    [Test]
    public void GetterWithFieldKeywordTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Example {
                x: i32 {
                    public get {
                        return field;
                    }
                }
            }
            """);
        var (tree, diagnostics, _) = Lower(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var packageNs = new PackageMetadata("test").Namespace;
        var test1Ns = packageNs.CreateChild(["Test1"]);
        var exampleType = new TypeMetadata(null, AccessModifierMetadata.Public, "Example")
        {
            Namespace = test1Ns,
        };
        exampleType.AddConstructor(
            new ConstructorMetadata(
                null,
                exampleType,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], exampleType, rootNamespace)));

        var backingField = new FieldMetadata(exampleType, "<>_x", builtInTypes.I32);
        exampleType.AddField(backingField);

        var property = CreatePropertyMetadata(
            rootNamespace,
            exampleType,
            "x",
            builtInTypes.I32,
            AccessModifierMetadata.Public);
        exampleType.AddProperty(property);
        exampleType.AddMethod(property.Getter!);

        var pointer = new PointerMetadata(null, exampleType);
        var thisMember = new MemberAccessExpression(null, MemberAccessExpression.This)
        {
            Reference = new ParameterMetadata(null, MemberAccessExpression.This, pointer),
            AccessKind = MemberAccessKind.Read,
        };
        var expectedGetterBody = new BlockStatement(null, [
            new ReturnStatement(
                null,
                new MemberAccessExpression(null, thisMember, backingField.Name)
                {
                    Reference = backingField,
                    AccessKind = MemberAccessKind.Read,
                })
        ]);

        var getter = tree.Find<PropertyGetter>()!;
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(getter.Body, Is.EqualTo(expectedGetterBody).Using(SemanticComparer.Instance));
    }

    [Test]
    public void SetterWithValueKeywordTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Example {
                x: i32 {
                    private set {
                        field = value;
                    }
                }
            }
            """);
        var (tree, diagnostics, _) = Lower(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var packageNs = new PackageMetadata("test").Namespace;
        var test1Ns = packageNs.CreateChild(["Test1"]);
        var exampleType = new TypeMetadata(null, AccessModifierMetadata.Public, "Example")
        {
            Namespace = test1Ns,
        };
        exampleType.AddConstructor(
            new ConstructorMetadata(
                null,
                exampleType,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], exampleType, rootNamespace)));

        var backingField = new FieldMetadata(exampleType, "<>_x", builtInTypes.I32);
        exampleType.AddField(backingField);

        var property = CreatePropertyMetadata(
            rootNamespace,
            exampleType,
            "x",
            builtInTypes.I32,
            null,
            AccessModifierMetadata.Private);
        exampleType.AddProperty(property);
        exampleType.AddMethod(property.Setter!);

        var pointer = new PointerMetadata(null, exampleType);
        var thisMember = new MemberAccessExpression(null, MemberAccessExpression.This)
        {
            Reference = new ParameterMetadata(null, MemberAccessExpression.This, pointer),
            AccessKind = MemberAccessKind.Read,
        };
        var valueMember = new MemberAccessExpression(null, MemberAccessExpression.Value)
        {
            Reference = new ParameterMetadata(null, MemberAccessExpression.Value, builtInTypes.I32),
            AccessKind = MemberAccessKind.Read,
        };
        var expectedSetterBody = new BlockStatement(null, [
            new ExpressionStatement(
                null,
                new BinaryExpression(
                    null,
                    BinaryExpressionKind.Assignment,
                    new MemberAccessExpression(null, thisMember, backingField.Name)
                    {
                        Reference = backingField,
                        AccessKind = MemberAccessKind.Write,
                    },
                    valueMember
                )
                {
                    ReturnTypeMetadata = builtInTypes.I32,
                }
            )
        ]);

        var setter = tree.Find<PropertySetter>()!;
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(setter.Body, Is.EqualTo(expectedSetterBody).Using(SemanticComparer.Instance));
    }

    [Test]
    public void GetterAndSetterWithFieldAndValueTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Example {
                x: i32 {
                    public get {
                        return field;
                    }

                    private set {
                        field = value;
                    }
                }
            }
            """);
        var (tree, diagnostics, _) = Lower(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var packageNs = new PackageMetadata("test").Namespace;
        var test1Ns = packageNs.CreateChild(["Test1"]);
        var exampleType = new TypeMetadata(null, AccessModifierMetadata.Public, "Example")
        {
            Namespace = test1Ns,
        };
        exampleType.AddConstructor(
            new ConstructorMetadata(
                null,
                exampleType,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], exampleType, rootNamespace)));

        var backingField = new FieldMetadata(exampleType, "<>_x", builtInTypes.I32);
        exampleType.AddField(backingField);

        var property = CreatePropertyMetadata(
            rootNamespace,
            exampleType,
            "x",
            builtInTypes.I32,
            AccessModifierMetadata.Public,
            AccessModifierMetadata.Private);
        exampleType.AddProperty(property);
        exampleType.AddMethod(property.Getter!);
        exampleType.AddMethod(property.Setter!);

        var pointer = new PointerMetadata(null, exampleType);
        var expectedGetterBody = new BlockStatement(null, [
            new ReturnStatement(
                null,
                new MemberAccessExpression(
                    null,
                    new MemberAccessExpression(null, MemberAccessExpression.This)
                    {
                        Reference = new ParameterMetadata(null, MemberAccessExpression.This, pointer),
                        AccessKind = MemberAccessKind.Read,
                    },
                    backingField.Name)
                {
                    Reference = backingField,
                    AccessKind = MemberAccessKind.Read,
                })
        ]);
        var expectedSetterBody = new BlockStatement(null, [
            new ExpressionStatement(
                null,
                new BinaryExpression(
                    null,
                    BinaryExpressionKind.Assignment,
                    new MemberAccessExpression(
                        null,
                        new MemberAccessExpression(null, MemberAccessExpression.This)
                        {
                            Reference = new ParameterMetadata(null, MemberAccessExpression.This, pointer),
                            AccessKind = MemberAccessKind.Read,
                        },
                        backingField.Name)
                    {
                        Reference = backingField,
                        AccessKind = MemberAccessKind.Write,
                    },
                    new MemberAccessExpression(null, MemberAccessExpression.Value)
                    {
                        Reference = new ParameterMetadata(null, MemberAccessExpression.Value, builtInTypes.I32),
                        AccessKind = MemberAccessKind.Read,
                    }
                )
                {
                    ReturnTypeMetadata = builtInTypes.I32,
                }
            )
        ]);

        var getter = tree.Find<PropertyGetter>()!;
        var setter = tree.Find<PropertySetter>()!;
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(getter.Body, Is.EqualTo(expectedGetterBody).Using(SemanticComparer.Instance));
        Assert.That(setter.Body, Is.EqualTo(expectedSetterBody).Using(SemanticComparer.Instance));
    }

    [Test]
    public void PropertyWithoutFieldOrValueTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Example {
                x: i32 {
                    public get {
                        return 0;
                    }

                    private set {
                        return;
                    }
                }
            }
            """);
        var (tree, diagnostics, compilationContext) = Lower(file);

        var builtInTypes = new BuiltInTypes();
        var expectedRootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var expectedPackageNs = new PackageMetadata("test").Namespace;
        var expectedTest1Ns = expectedPackageNs.CreateChild(["Test1"]);
        var expectedExampleType = new TypeMetadata(null, AccessModifierMetadata.Public, "Example")
        {
            Namespace = expectedTest1Ns,
        };
        expectedExampleType.AddConstructor(
            new ConstructorMetadata(
                null,
                expectedExampleType,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], expectedExampleType, expectedRootNamespace)));

        var property = CreatePropertyMetadata(
            expectedRootNamespace,
            expectedExampleType,
            "x",
            builtInTypes.I32,
            AccessModifierMetadata.Public,
            AccessModifierMetadata.Private);
        expectedExampleType.AddProperty(property);
        expectedExampleType.AddMethod(property.Getter!);
        expectedExampleType.AddMethod(property.Setter!);

        var expectedGetterBody = new BlockStatement(null, [
            new ReturnStatement(
                null,
                new LiteralExpression(null, LiteralExpressionKind.I32, 0)
                {
                    ReturnTypeMetadata = builtInTypes.I32,
                })
        ]);
        var expectedSetterBody = new BlockStatement(null, [
            new ReturnStatement(null)
        ]);

        var test1Ns = compilationContext.FindNamespace(["Test1"]).Namespace!;
        var exampleType = (TypeMetadata)test1Ns.FindType("Example")!;
        var getter = tree.Find<PropertyGetter>()!;
        var setter = tree.Find<PropertySetter>()!;
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(getter.Body, Is.EqualTo(expectedGetterBody).Using(SemanticComparer.Instance));
        Assert.That(setter.Body, Is.EqualTo(expectedSetterBody).Using(SemanticComparer.Instance));
        Assert.That(exampleType.Fields, Is.Empty);
    }

    [Test]
    public void MultiplePropertiesWithFieldAndValueTest()
    {
        var file = CreateFile(
            """
            namespace Test1;

            public type Example {
                x: i32 {
                    public get {
                        return field;
                    }

                    private set {
                        field = value;
                    }
                }

                y: i32 {
                    public get {
                        return field;
                    }

                    private set {
                        field = value;
                    }
                }
            }
            """);
        var (tree, diagnostics, _) = Lower(file);

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var packageNs = new PackageMetadata("test").Namespace;
        var test1Ns = packageNs.CreateChild(["Test1"]);
        var exampleType = new TypeMetadata(null, AccessModifierMetadata.Public, "Example")
        {
            Namespace = test1Ns,
        };
        exampleType.AddConstructor(
            new ConstructorMetadata(
                null,
                exampleType,
                AccessModifierMetadata.Public,
                [],
                CreateFunctionType([], exampleType, rootNamespace)));

        var xBackingField = new FieldMetadata(exampleType, "<>_x", builtInTypes.I32);
        exampleType.AddField(xBackingField);

        var yBackingField = new FieldMetadata(exampleType, "<>_y", builtInTypes.I32);
        exampleType.AddField(yBackingField);

        var xProperty = CreatePropertyMetadata(
            rootNamespace,
            exampleType,
            "x",
            builtInTypes.I32,
            AccessModifierMetadata.Public,
            AccessModifierMetadata.Private);
        exampleType.AddProperty(xProperty);
        exampleType.AddMethod(xProperty.Getter!);
        exampleType.AddMethod(xProperty.Setter!);

        var yProperty = CreatePropertyMetadata(
            rootNamespace,
            exampleType,
            "y",
            builtInTypes.I32,
            AccessModifierMetadata.Public,
            AccessModifierMetadata.Private);
        exampleType.AddProperty(yProperty);
        exampleType.AddMethod(yProperty.Getter!);
        exampleType.AddMethod(yProperty.Setter!);

        var pointer = new PointerMetadata(null, exampleType);
        var xGetterBody = new BlockStatement(null, [
            new ReturnStatement(
                null,
                new MemberAccessExpression(
                    null,
                    new MemberAccessExpression(null, MemberAccessExpression.This)
                    {
                        Reference = new ParameterMetadata(null, MemberAccessExpression.This, pointer),
                        AccessKind = MemberAccessKind.Read,
                    },
                    xBackingField.Name)
                {
                    Reference = xBackingField,
                    AccessKind = MemberAccessKind.Read,
                })
        ]);

        var xSetterBody = new BlockStatement(null, [
            new ExpressionStatement(
                null,
                new BinaryExpression(
                    null,
                    BinaryExpressionKind.Assignment,
                    new MemberAccessExpression(
                        null,
                        new MemberAccessExpression(null, MemberAccessExpression.This)
                        {
                            Reference = new ParameterMetadata(null, MemberAccessExpression.This, pointer),
                            AccessKind = MemberAccessKind.Read,
                        },
                        xBackingField.Name)
                    {
                        Reference = xBackingField,
                        AccessKind = MemberAccessKind.Write,
                    },
                    new MemberAccessExpression(null, MemberAccessExpression.Value)
                    {
                        Reference = new ParameterMetadata(null, MemberAccessExpression.Value, builtInTypes.I32),
                        AccessKind = MemberAccessKind.Read,
                    }
                )
                {
                    ReturnTypeMetadata = builtInTypes.I32,
                }
            )
        ]);

        var yGetterBody = new BlockStatement(null, [
            new ReturnStatement(
                null,
                new MemberAccessExpression(
                    null,
                    new MemberAccessExpression(null, MemberAccessExpression.This)
                    {
                        Reference = new ParameterMetadata(null, MemberAccessExpression.This, pointer),
                        AccessKind = MemberAccessKind.Read,
                    },
                    yBackingField.Name)
                {
                    Reference = yBackingField,
                    AccessKind = MemberAccessKind.Read,
                })
        ]);

        var ySetterBody = new BlockStatement(null, [
            new ExpressionStatement(
                null,
                new BinaryExpression(
                    null,
                    BinaryExpressionKind.Assignment,
                    new MemberAccessExpression(
                        null,
                        new MemberAccessExpression(null, MemberAccessExpression.This)
                        {
                            Reference = new ParameterMetadata(null, MemberAccessExpression.This, pointer),
                            AccessKind = MemberAccessKind.Read,
                        },
                        yBackingField.Name)
                    {
                        Reference = yBackingField,
                        AccessKind = MemberAccessKind.Write,
                    },
                    new MemberAccessExpression(null, MemberAccessExpression.Value)
                    {
                        Reference = new ParameterMetadata(null, MemberAccessExpression.Value, builtInTypes.I32),
                        AccessKind = MemberAccessKind.Read,
                    }
                )
                {
                    ReturnTypeMetadata = builtInTypes.I32,
                }
            )
        ]);

        var xPropertyDeclaration = tree.Find<PropertyDeclaration>(p => p.Name == "x")!;
        var yPropertyDeclaration = tree.Find<PropertyDeclaration>(p => p.Name == "y")!;
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(xPropertyDeclaration.Getter!.Body, Is.EqualTo(xGetterBody).Using(SemanticComparer.Instance));
        Assert.That(xPropertyDeclaration.Setter!.Body, Is.EqualTo(xSetterBody).Using(SemanticComparer.Instance));
        Assert.That(yPropertyDeclaration.Getter!.Body, Is.EqualTo(yGetterBody).Using(SemanticComparer.Instance));
        Assert.That(yPropertyDeclaration.Setter!.Body, Is.EqualTo(ySetterBody).Using(SemanticComparer.Instance));
    }
}
using Trilang;
using Trilang.Metadata;
using Trilang.Semantics.Model;
using static Tri.Tests.Factory;
using static Tri.Tests.Helpers;

namespace Tri.Tests.Lowering;

public class AddThisInLocalMemberAccessTests
{
    [Test]
    public void AddThisBeforePropertyTest()
    {
        var (tree, diagnostics, _) = Lower(
            CreateFile(
                """
                namespace Test1;

                public type Test {
                    count: i32;

                    public getCount(): i32 {
                        return count;
                    }
                }
                """));

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var packageNs = NamespaceMetadata.CreateForPackage();
        var test1Ns = packageNs.CreateChild(["Test1"]);
        var typeMetadata = new TypeMetadata(null, "Test")
        {
            Namespace = test1Ns,
        };
        typeMetadata.AddConstructor(new ConstructorMetadata(
            null,
            typeMetadata,
            AccessModifierMetadata.Public,
            [],
            CreateFunctionType([], builtInTypes.Void, rootNamespace)));
        typeMetadata.AddField(new FieldMetadata(typeMetadata, "<>_count", builtInTypes.I32));
        var propertyMetadata = CreatePropertyMetadata(
            rootNamespace,
            typeMetadata,
            "count",
            builtInTypes.I32);
        typeMetadata.AddProperty(propertyMetadata);
        typeMetadata.AddMethod(propertyMetadata.Getter!);
        typeMetadata.AddMethod(propertyMetadata.Setter!);
        typeMetadata.AddMethod(new MethodMetadata(
            null,
            typeMetadata,
            AccessModifierMetadata.Public,
            false,
            "getCount",
            [],
            CreateFunctionType([], builtInTypes.I32, rootNamespace)));

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
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(returnStatement, Is.Not.Null);
        Assert.That(returnStatement.Expression, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }

    [Test]
    public void AddThisBeforeMethodTest()
    {
        var (tree, diagnostics, _) = Lower(
            CreateFile(
                """
                namespace Test1;

                public type Test {
                    public print(): void { }

                    public test(): void {
                        print();
                    }
                }
                """));

        var builtInTypes = new BuiltInTypes();
        var rootNamespace = RootNamespaceMetadata.Create(builtInTypes);
        var packageNs = NamespaceMetadata.CreateForPackage();
        var test1Ns = packageNs.CreateChild(["Test1"]);
        var typeMetadata = new TypeMetadata(null, "Test")
        {
            Namespace = test1Ns,
        };
        typeMetadata.AddConstructor(new ConstructorMetadata(
            null,
            typeMetadata,
            AccessModifierMetadata.Public,
            [],
            CreateFunctionType([], builtInTypes.Void, rootNamespace)));
        var methodMetadata = new MethodMetadata(
            null,
            typeMetadata,
            AccessModifierMetadata.Public,
            false,
            "print",
            [],
            CreateFunctionType([], builtInTypes.Void, rootNamespace));
        typeMetadata.AddMethod(methodMetadata);
        typeMetadata.AddMethod(new MethodMetadata(
            null,
            typeMetadata,
            AccessModifierMetadata.Public,
            false,
            "test",
            [],
            CreateFunctionType([], builtInTypes.Void, rootNamespace)));

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
        Assert.That(diagnostics.Diagnostics, Is.Empty);
        Assert.That(returnStatement, Is.Not.Null);
        Assert.That(returnStatement.Member, Is.EqualTo(expected).Using(SemanticComparer.Instance));
    }
}
using Trilang.Semantics.Model;
using Type = Trilang.Semantics.Model.Type;

namespace Tri.Tests;

// ReSharper disable all UnusedParameter.Local
internal class SemanticComparer : IEqualityComparer<ISemanticNode>
{
    public static readonly SemanticComparer Instance = new SemanticComparer();

    public bool Equals(ISemanticNode? x, ISemanticNode? y)
        => (x, y) switch
        {
            (null, null) => true,
            (null, _) => throw new Exception("x is null"),
            (_, null) => throw new Exception("y is null"),

            (AliasDeclaration x1, AliasDeclaration y1)
                => CompareAliasDeclarationNode(x1, y1),
            (ArrayAccessExpression x1, ArrayAccessExpression y1)
                => CompareArrayAccessExpressionNode(x1, y1),
            (ArrayType x1, ArrayType y1)
                => CompareArrayTypeNode(x1, y1),
            (BinaryExpression x1, BinaryExpression y1)
                => CompareBinaryExpressionNode(x1, y1),
            (BlockStatement x1, BlockStatement y1)
                => CompareBlockStatementNode(x1, y1),
            (Break x1, Break y1)
                => CompareBreakNode(x1, y1),
            (CallExpression x1, CallExpression y1)
                => CompareCallExpressionNode(x1, y1),
            (CastExpression x1, CastExpression y1)
                => CompareCastExpressionNode(x1, y1),
            (ConstructorDeclaration x1, ConstructorDeclaration y1)
                => CompareConstructorDeclarationNode(x1, y1),
            (Continue x1, Continue y1)
                => CompareContinueNode(x1, y1),
            (DiscriminatedUnion x1, DiscriminatedUnion y1)
                => CompareDiscriminatedUnionNode(x1, y1),
            (ExpressionBlock x1, ExpressionBlock y1)
                => CompareExpressionBlockNode(x1, y1),
            (ExpressionStatement x1, ExpressionStatement y1)
                => CompareExpressionStatementNode(x1, y1),
            (FakeDeclaration, FakeDeclaration)
                => true,
            (FakeExpression, FakeExpression)
                => true,
            (FakeStatement, FakeStatement)
                => true,
            (FakeType, FakeType)
                => true,
            (FunctionDeclaration x1, FunctionDeclaration y1)
                => CompareFunctionDeclarationNode(x1, y1),
            (FunctionType x1, FunctionType y1)
                => CompareFunctionTypeNode(x1, y1),
            (GenericType x1, GenericType y1)
                => CompareGenericTypeNode(x1, y1),
            (GoTo x1, GoTo y1)
                => CompareGoToNode(x1, y1),
            (IfDirective x1, IfDirective y1)
                => CompareIfDirectiveNode(x1, y1),
            (IfStatement x1, IfStatement y1)
                => CompareIfStatementNode(x1, y1),
            (InterfaceMethod x1, InterfaceMethod y1)
                => CompareInterfaceMethodNode(x1, y1),
            (Interface x1, Interface y1)
                => CompareInterfaceNode(x1, y1),
            (InterfaceProperty x1, InterfaceProperty y1)
                => CompareInterfacePropertyNode(x1, y1),
            (IsExpression x1, IsExpression y1)
                => CompareAsExpressionNode(x1, y1),
            (Label x1, Label y1)
                => CompareLabelNode(x1, y1),
            (LiteralExpression x1, LiteralExpression y1)
                => CompareLiteralExpressionNode(x1, y1),
            (MemberAccessExpression x1, MemberAccessExpression y1)
                => CompareMemberAccessExpressionNode(x1, y1),
            (MethodDeclaration x1, MethodDeclaration y1)
                => CompareMethodDeclarationNode(x1, y1),
            (NewArrayExpression x1, NewArrayExpression y1)
                => CompareNewArrayExpressionNode(x1, y1),
            (NewObjectExpression x1, NewObjectExpression y1)
                => CompareNewObjectExpressionNode(x1, y1),
            (NullExpression x1, NullExpression y1)
                => CompareNullExpressionNode(x1, y1),
            (Parameter x1, Parameter y1)
                => CompareParameterNode(x1, y1),
            (PropertyDeclaration x1, PropertyDeclaration y1)
                => ComparePropertyDeclarationNode(x1, y1),
            (PropertyGetter x1, PropertyGetter y1)
                => ComparePropertyGetterNode(x1, y1),
            (PropertySetter x1, PropertySetter y1)
                => ComparePropertySetterNode(x1, y1),
            (ReturnStatement x1, ReturnStatement y1)
                => CompareReturnStatementNode(x1, y1),
            (SemanticTree x1, SemanticTree y1)
                => CompareSyntaxTree(x1, y1),
            (TupleExpression x1, TupleExpression y1)
                => CompareTupleExpressionNode(x1, y1),
            (TupleType x1, TupleType y1)
                => CompareTupleTypeNode(x1, y1),
            (TypeDeclaration x1, TypeDeclaration y1)
                => CompareTypeDeclarationNode(x1, y1),
            (Type x1, Type y1)
                => CompareTypeNode(x1, y1),
            (UnaryExpression x1, UnaryExpression y1)
                => CompareUnaryExpressionNode(x1, y1),
            (VariableDeclaration x1, VariableDeclaration y1)
                => CompareVariableDeclarationStatementNode(x1, y1),
            (While x1, While y1)
                => CompareWhileNode(x1, y1),

            _ => throw new Exception($"{x.GetType()} != {y.GetType()}"),
        };

    private bool CompareAliasDeclarationNode(AliasDeclaration x, AliasDeclaration y)
    {
        if (x.AccessModifier != y.AccessModifier)
            throw new Exception($"AccessModifier doesn't match. {x.AccessModifier} != {y.AccessModifier}.");

        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        if (!x.GenericArguments.SequenceEqual(y.GenericArguments, this))
            throw new Exception("GenericArguments don't match.");

        if (!Equals(x.Type, y.Type))
            throw new Exception("Type doesn't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareArrayAccessExpressionNode(ArrayAccessExpression x, ArrayAccessExpression y)
    {
        if (!Equals(x.Member, y.Member))
            throw new Exception("Expression doesn't match.");

        if (!Equals(x.Index, y.Index))
            throw new Exception("Index doesn't match.");

        if (!new MetadataComparer().Equals(x.ReturnTypeMetadata, y.ReturnTypeMetadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareArrayTypeNode(ArrayType x, ArrayType y)
    {
        if (!Equals(x.ElementType, y.ElementType))
            throw new Exception("ElementType doesn't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareBinaryExpressionNode(BinaryExpression x, BinaryExpression y)
    {
        if (!Equals(x.Left, y.Left))
            throw new Exception("Left doesn't match.");

        if (x.Kind != y.Kind)
            throw new Exception($"Operator doesn't match. {x.Kind} != {y.Kind}.");

        if (!Equals(x.Right, y.Right))
            throw new Exception("Right doesn't match.");

        if (!new MetadataComparer().Equals(x.ReturnTypeMetadata, y.ReturnTypeMetadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareBlockStatementNode(BlockStatement x, BlockStatement y)
    {
        if (!x.Statements.SequenceEqual(y.Statements, this))
            throw new Exception("Statements don't match.");

        return true;
    }

    private bool CompareBreakNode(Break x, Break y)
    {
        return true;
    }

    private bool CompareCallExpressionNode(CallExpression x, CallExpression y)
    {
        if (!Equals(x.Member, y.Member))
            throw new Exception("Expression doesn't match.");

        if (!x.Parameters.SequenceEqual(y.Parameters, this))
            throw new Exception("Arguments don't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareCastExpressionNode(CastExpression x, CastExpression y)
    {
        if (!Equals(x.Type, y.Type))
            throw new Exception("TargetType doesn't match.");

        if (!Equals(x.Expression, y.Expression))
            throw new Exception("Expression doesn't match.");

        return true;
    }

    private bool CompareConstructorDeclarationNode(ConstructorDeclaration x, ConstructorDeclaration y)
    {
        if (x.AccessModifier != y.AccessModifier)
            throw new Exception($"AccessModifier doesn't match. {x.AccessModifier} != {y.AccessModifier}.");

        if (!x.Parameters.SequenceEqual(y.Parameters, this))
            throw new Exception("Parameters don't match.");

        if (!Equals(x.Body, y.Body))
            throw new Exception("Body doesn't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareContinueNode(Continue x, Continue y)
    {
        return true;
    }

    private bool CompareDiscriminatedUnionNode(DiscriminatedUnion x, DiscriminatedUnion y)
    {
        if (!x.Types.SequenceEqual(y.Types, this))
            throw new Exception("Cases don't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareExpressionBlockNode(ExpressionBlock x, ExpressionBlock y)
    {
        if (!x.Statements.SequenceEqual(y.Statements, this))
            throw new Exception("Expression doesn't match.");

        return true;
    }

    private bool CompareExpressionStatementNode(ExpressionStatement x, ExpressionStatement y)
    {
        if (!Equals(x.Expression, y.Expression))
            throw new Exception("Expression doesn't match.");

        return true;
    }

    private bool CompareFunctionDeclarationNode(FunctionDeclaration x, FunctionDeclaration y)
    {
        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        if (!x.Parameters.SequenceEqual(y.Parameters, this))
            throw new Exception("Parameters don't match.");

        if (!Equals(x.ReturnType, y.ReturnType))
            throw new Exception("ReturnType doesn't match.");

        if (!Equals(x.Body, y.Body))
            throw new Exception("Body doesn't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareFunctionTypeNode(FunctionType x, FunctionType y)
    {
        if (!x.ParameterTypes.SequenceEqual(y.ParameterTypes, this))
            throw new Exception("Parameters don't match.");

        if (!Equals(x.ReturnType, y.ReturnType))
            throw new Exception("ReturnType doesn't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareGenericTypeNode(GenericType x, GenericType y)
    {
        if (x.Name != y.Name)
            throw new Exception("BaseType doesn't match.");

        if (!x.TypeArguments.SequenceEqual(y.TypeArguments, this))
            throw new Exception("Arguments don't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareGoToNode(GoTo x, GoTo y)
    {
        if (x.Label != y.Label)
            throw new Exception($"Label doesn't match. {x.Label} != {y.Label}.");

        return true;
    }

    private bool CompareIfDirectiveNode(IfDirective x, IfDirective y)
    {
        if (x.DirectiveName != y.DirectiveName)
            throw new Exception($"Condition doesn't match. {x.DirectiveName} != {y.DirectiveName}.");

        if (!x.Then.SequenceEqual(y.Then, this))
            throw new Exception("IfBranch doesn't match.");

        if (!x.Else.SequenceEqual(y.Else, this))
            throw new Exception("ElseBranch doesn't match.");

        return true;
    }

    private bool CompareIfStatementNode(IfStatement x, IfStatement y)
    {
        if (!Equals(x.Condition, y.Condition))
            throw new Exception("Condition doesn't match.");

        if (!Equals(x.Then, y.Then))
            throw new Exception("IfBranch doesn't match.");

        if (!Equals(x.Else, y.Else))
            throw new Exception("ElseBranch doesn't match.");

        return true;
    }

    private bool CompareInterfaceMethodNode(InterfaceMethod x, InterfaceMethod y)
    {
        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        if (!x.ParameterTypes.SequenceEqual(y.ParameterTypes, this))
            throw new Exception("Parameters don't match.");

        if (!Equals(x.ReturnType, y.ReturnType))
            throw new Exception("ReturnType doesn't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareInterfaceNode(Interface x, Interface y)
    {
        if (!x.Properties.SequenceEqual(y.Properties, this))
            throw new Exception("Properties don't match.");

        if (!x.Methods.SequenceEqual(y.Methods, this))
            throw new Exception("Methods don't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareInterfacePropertyNode(InterfaceProperty x, InterfaceProperty y)
    {
        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        if (!Equals(x.Type, y.Type))
            throw new Exception("Type doesn't match.");

        if (x.GetterModifier != y.GetterModifier)
            throw new Exception($"HasGetter doesn't match. {x.GetterModifier} != {y.GetterModifier}.");

        if (x.SetterModifier != y.SetterModifier)
            throw new Exception($"HasSetter doesn't match. {x.SetterModifier} != {y.SetterModifier}.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareAsExpressionNode(IsExpression x, IsExpression y)
    {
        if (!Equals(x.Expression, y.Expression))
            throw new Exception("Expression doesn't match.");

        if (!Equals(x.Type, y.Type))
            throw new Exception("TargetType doesn't match.");

        if (!new MetadataComparer().Equals(x.ReturnTypeMetadata, y.ReturnTypeMetadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareLabelNode(Label x, Label y)
    {
        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        return true;
    }

    private bool CompareLiteralExpressionNode(LiteralExpression x, LiteralExpression y)
    {
        if (x.Kind != y.Kind)
            throw new Exception($"Literal doesn't match. {x.Kind} != {y.Kind}.");

        if (!x.Value.Equals(y.Value))
            throw new Exception($"Value doesn't match. {x.Value} != {y.Value}.");

        if (!new MetadataComparer().Equals(x.ReturnTypeMetadata, y.ReturnTypeMetadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareMemberAccessExpressionNode(MemberAccessExpression x, MemberAccessExpression y)
    {
        if (!Equals(x.Member, y.Member))
            throw new Exception("Expression doesn't match.");

        if (x.Name != y.Name)
            throw new Exception($"Member doesn't match. {x.Name} != {y.Name}.");

        if (!Equals(x.Reference, y.Reference))
            throw new Exception("Reference doesn't match.");

        if (x.AccessKind != y.AccessKind)
            throw new Exception($"AccessKind doesn't match. {x.AccessKind} != {y.AccessKind}.");

        return true;
    }

    private bool CompareMethodDeclarationNode(MethodDeclaration x, MethodDeclaration y)
    {
        if (x.AccessModifier != y.AccessModifier)
            throw new Exception($"AccessModifier doesn't match. {x.AccessModifier} != {y.AccessModifier}.");

        if (x.IsStatic != y.IsStatic)
            throw new Exception($"IsStatic doesn't match. {x.IsStatic} != {y.IsStatic}.");

        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        if (!x.Parameters.SequenceEqual(y.Parameters, this))
            throw new Exception("Parameters don't match.");

        if (!Equals(x.ReturnType, y.ReturnType))
            throw new Exception("ReturnType doesn't match.");

        if (!Equals(x.Body, y.Body))
            throw new Exception("Body doesn't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareNewArrayExpressionNode(NewArrayExpression x, NewArrayExpression y)
    {
        if (!Equals(x.Type, y.Type))
            throw new Exception("Type doesn't match.");

        if (!Equals(x.Size, y.Size))
            throw new Exception("Sizes don't match.");

        if (!new MetadataComparer().Equals(x.ReturnTypeMetadata, y.ReturnTypeMetadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareNewObjectExpressionNode(NewObjectExpression x, NewObjectExpression y)
    {
        if (!Equals(x.Type, y.Type))
            throw new Exception("Type doesn't match.");

        if (!x.Parameters.SequenceEqual(y.Parameters, this))
            throw new Exception("Arguments don't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareNullExpressionNode(NullExpression x, NullExpression y)
    {
        return true;
    }

    private bool CompareParameterNode(Parameter x, Parameter y)
    {
        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        if (!Equals(x.Type, y.Type))
            throw new Exception("Type doesn't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool ComparePropertyDeclarationNode(PropertyDeclaration x, PropertyDeclaration y)
    {
        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        if (!Equals(x.Type, y.Type))
            throw new Exception("Type doesn't match.");

        if (!Equals(x.Getter, y.Getter))
            throw new Exception("Getter doesn't match.");

        if (!Equals(x.Setter, y.Setter))
            throw new Exception("Setter doesn't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool ComparePropertyGetterNode(PropertyGetter x, PropertyGetter y)
    {
        if (x.AccessModifier != y.AccessModifier)
            throw new Exception($"AccessModifier doesn't match. {x.AccessModifier} != {y.AccessModifier}.");

        if (!Equals(x.Body, y.Body))
            throw new Exception("Body doesn't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool ComparePropertySetterNode(PropertySetter x, PropertySetter y)
    {
        if (x.AccessModifier != y.AccessModifier)
            throw new Exception($"AccessModifier doesn't match. {x.AccessModifier} != {y.AccessModifier}.");

        if (!Equals(x.Body, y.Body))
            throw new Exception("Body doesn't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareReturnStatementNode(ReturnStatement x, ReturnStatement y)
    {
        if (!Equals(x.Expression, y.Expression))
            throw new Exception("Expression doesn't match.");

        return true;
    }

    private bool CompareSyntaxTree(SemanticTree x, SemanticTree y)
    {
        if (!x.Declarations.SequenceEqual(y.Declarations, this))
            throw new Exception("Members don't match.");

        return true;
    }

    private bool CompareTupleExpressionNode(TupleExpression x, TupleExpression y)
    {
        if (!x.Expressions.SequenceEqual(y.Expressions, this))
            throw new Exception("Elements don't match.");

        if (!new MetadataComparer().Equals(x.ReturnTypeMetadata, y.ReturnTypeMetadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareTupleTypeNode(TupleType x, TupleType y)
    {
        if (!x.Types.SequenceEqual(y.Types, this))
            throw new Exception("Elements don't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareTypeDeclarationNode(TypeDeclaration x, TypeDeclaration y)
    {
        if (x.AccessModifier != y.AccessModifier)
            throw new Exception($"AccessModifier doesn't match. {x.AccessModifier} != {y.AccessModifier}.");

        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        if (!x.GenericArguments.SequenceEqual(y.GenericArguments, this))
            throw new Exception("GenericArguments don't match.");

        if (!x.Properties.SequenceEqual(y.Properties, this))
            throw new Exception("Properties don't match.");

        if (!x.Constructors.SequenceEqual(y.Constructors, this))
            throw new Exception("Constructors don't match.");

        if (!x.Methods.SequenceEqual(y.Methods, this))
            throw new Exception("Methods don't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareTypeNode(Type x, Type y)
    {
        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareUnaryExpressionNode(UnaryExpression x, UnaryExpression y)
    {
        if (x.Kind != y.Kind)
            throw new Exception($"Operator doesn't match. {x.Kind} != {y.Kind}.");

        if (!Equals(x.Operand, y.Operand))
            throw new Exception("Operand doesn't match.");

        if (!new MetadataComparer().Equals(x.ReturnTypeMetadata, y.ReturnTypeMetadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareVariableDeclarationStatementNode(VariableDeclaration x, VariableDeclaration y)
    {
        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        if (!Equals(x.Type, y.Type))
            throw new Exception("Type doesn't match.");

        if (!Equals(x.Expression, y.Expression))
            throw new Exception("Initializer doesn't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareWhileNode(While x, While y)
    {
        if (!Equals(x.Condition, y.Condition))
            throw new Exception("Condition doesn't match.");

        if (!Equals(x.Body, y.Body))
            throw new Exception("Body doesn't match.");

        return true;
    }

    public int GetHashCode(ISemanticNode obj)
        => obj.GetHashCode();
}
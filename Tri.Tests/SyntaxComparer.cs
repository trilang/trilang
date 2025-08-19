using Trilang.Parsing.Ast;

namespace Tri.Tests;

internal class SyntaxComparer : IEqualityComparer<ISyntaxNode>
{
    public static readonly SyntaxComparer Instance = new SyntaxComparer();

    public bool Equals(ISyntaxNode? x, ISyntaxNode? y)
        => (x, y) switch
        {
            (null, null) => true,
            (null, _) => throw new Exception("x is null"),
            (_, null) => throw new Exception("y is null"),

            (ArrayAccessExpressionNode x1, ArrayAccessExpressionNode y1)
                => CompareArrayAccessExpressionNode(x1, y1),
            (ArrayTypeNode x1, ArrayTypeNode y1)
                => CompareArrayTypeNode(x1, y1),
            (AsExpressionNode x1, AsExpressionNode y1)
                => CompareAsExpressionNode(x1, y1),
            (BinaryExpressionNode x1, BinaryExpressionNode y1)
                => CompareBinaryExpressionNode(x1, y1),
            (BlockStatementNode x1, BlockStatementNode y1)
                => CompareBlockStatementNode(x1, y1),
            (BreakNode x1, BreakNode y1)
                => CompareBreakNode(x1, y1),
            (CallExpressionNode x1, CallExpressionNode y1)
                => CompareCallExpressionNode(x1, y1),
            (CastExpressionNode x1, CastExpressionNode y1)
                => CompareCastExpressionNode(x1, y1),
            (ConstructorDeclarationNode x1, ConstructorDeclarationNode y1)
                => CompareConstructorDeclarationNode(x1, y1),
            (ContinueNode x1, ContinueNode y1)
                => CompareContinueNode(x1, y1),
            (DiscriminatedUnionNode x1, DiscriminatedUnionNode y1)
                => CompareDiscriminatedUnionNode(x1, y1),
            (ExpressionBlockNode x1, ExpressionBlockNode y1)
                => CompareExpressionBlockNode(x1, y1),
            (ExpressionStatementNode x1, ExpressionStatementNode y1)
                => CompareExpressionStatementNode(x1, y1),
            (FunctionDeclarationNode x1, FunctionDeclarationNode y1)
                => CompareFunctionDeclarationNode(x1, y1),
            (FunctionTypeNode x1, FunctionTypeNode y1)
                => CompareFunctionTypeNode(x1, y1),
            (GenericTypeNode x1, GenericTypeNode y1)
                => CompareGenericTypeNode(x1, y1),
            (GoToNode x1, GoToNode y1)
                => CompareGoToNode(x1, y1),
            (IfDirectiveNode x1, IfDirectiveNode y1)
                => CompareIfDirectiveNode(x1, y1),
            (IfStatementNode x1, IfStatementNode y1)
                => CompareIfStatementNode(x1, y1),
            (InterfaceMethodNode x1, InterfaceMethodNode y1)
                => CompareInterfaceMethodNode(x1, y1),
            (InterfaceNode x1, InterfaceNode y1)
                => CompareInterfaceNode(x1, y1),
            (InterfacePropertyNode x1, InterfacePropertyNode y1)
                => CompareInterfacePropertyNode(x1, y1),
            (LabelNode x1, LabelNode y1)
                => CompareLabelNode(x1, y1),
            (LiteralExpressionNode x1, LiteralExpressionNode y1)
                => CompareLiteralExpressionNode(x1, y1),
            (MemberAccessExpressionNode x1, MemberAccessExpressionNode y1)
                => CompareMemberAccessExpressionNode(x1, y1),
            (MethodDeclarationNode x1, MethodDeclarationNode y1)
                => CompareMethodDeclarationNode(x1, y1),
            (NewArrayExpressionNode x1, NewArrayExpressionNode y1)
                => CompareNewArrayExpressionNode(x1, y1),
            (NewObjectExpressionNode x1, NewObjectExpressionNode y1)
                => CompareNewObjectExpressionNode(x1, y1),
            (NullExpressionNode x1, NullExpressionNode y1)
                => CompareNullExpressionNode(x1, y1),
            (ParameterNode x1, ParameterNode y1)
                => CompareParameterNode(x1, y1),
            (PropertyDeclarationNode x1, PropertyDeclarationNode y1)
                => ComparePropertyDeclarationNode(x1, y1),
            (PropertyGetterNode x1, PropertyGetterNode y1)
                => ComparePropertyGetterNode(x1, y1),
            (PropertySetterNode x1, PropertySetterNode y1)
                => ComparePropertySetterNode(x1, y1),
            (ReturnStatementNode x1, ReturnStatementNode y1)
                => CompareReturnStatementNode(x1, y1),
            (SyntaxTree x1, SyntaxTree y1)
                => CompareSyntaxTree(x1, y1),
            (TupleExpressionNode x1, TupleExpressionNode y1)
                => CompareTupleExpressionNode(x1, y1),
            (TupleTypeNode x1, TupleTypeNode y1)
                => CompareTupleTypeNode(x1, y1),
            (TypeAliasDeclarationNode x1, TypeAliasDeclarationNode y1)
                => CompareTypeAliasDeclarationNode(x1, y1),
            (TypeDeclarationNode x1, TypeDeclarationNode y1)
                => CompareTypeDeclarationNode(x1, y1),
            (TypeNode x1, TypeNode y1)
                => CompareTypeNode(x1, y1),
            (UnaryExpressionNode x1, UnaryExpressionNode y1)
                => CompareUnaryExpressionNode(x1, y1),
            (VariableDeclarationStatementNode x1, VariableDeclarationStatementNode y1)
                => CompareVariableDeclarationStatementNode(x1, y1),
            (WhileNode x1, WhileNode y1)
                => CompareWhileNode(x1, y1),

            _ => throw new Exception($"{x.GetType()} != {y.GetType()}"),
        };

    private bool CompareArrayAccessExpressionNode(ArrayAccessExpressionNode x, ArrayAccessExpressionNode y)
    {
        if (!Equals(x.Member, y.Member))
            throw new Exception("Expression doesn't match.");

        if (!Equals(x.Index, y.Index))
            throw new Exception("Index doesn't match.");

        if (!new MetadataComparer().Equals(x.ReturnTypeMetadata, y.ReturnTypeMetadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareArrayTypeNode(ArrayTypeNode x, ArrayTypeNode y)
    {
        if (!Equals(x.ElementType, y.ElementType))
            throw new Exception("ElementType doesn't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareAsExpressionNode(AsExpressionNode x, AsExpressionNode y)
    {
        if (!Equals(x.Expression, y.Expression))
            throw new Exception("Expression doesn't match.");

        if (!Equals(x.Type, y.Type))
            throw new Exception("TargetType doesn't match.");

        if (!new MetadataComparer().Equals(x.ReturnTypeMetadata, y.ReturnTypeMetadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareBinaryExpressionNode(BinaryExpressionNode x, BinaryExpressionNode y)
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

    private bool CompareBlockStatementNode(BlockStatementNode x, BlockStatementNode y)
    {
        if (!x.Statements.SequenceEqual(y.Statements, this))
            throw new Exception("Statements don't match.");

        return true;
    }

    private bool CompareBreakNode(BreakNode x, BreakNode y)
    {
        return true;
    }

    private bool CompareCallExpressionNode(CallExpressionNode x, CallExpressionNode y)
    {
        if (!Equals(x.Member, y.Member))
            throw new Exception("Expression doesn't match.");

        if (!x.Parameters.SequenceEqual(y.Parameters, this))
            throw new Exception("Arguments don't match.");

        return true;
    }

    private bool CompareCastExpressionNode(CastExpressionNode x, CastExpressionNode y)
    {
        if (!Equals(x.Type, y.Type))
            throw new Exception("TargetType doesn't match.");

        if (!Equals(x.Expression, y.Expression))
            throw new Exception("Expression doesn't match.");

        return true;
    }

    private bool CompareConstructorDeclarationNode(ConstructorDeclarationNode x, ConstructorDeclarationNode y)
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

    private bool CompareContinueNode(ContinueNode x, ContinueNode y)
    {
        return true;
    }

    private bool CompareDiscriminatedUnionNode(DiscriminatedUnionNode x, DiscriminatedUnionNode y)
    {
        if (!x.Types.SequenceEqual(y.Types, this))
            throw new Exception("Cases don't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareExpressionBlockNode(ExpressionBlockNode x, ExpressionBlockNode y)
    {
        if (!x.Statements.SequenceEqual(y.Statements, this))
            throw new Exception("Expression doesn't match.");

        return true;
    }

    private bool CompareExpressionStatementNode(ExpressionStatementNode x, ExpressionStatementNode y)
    {
        if (!Equals(x.Expression, y.Expression))
            throw new Exception("Expression doesn't match.");

        return true;
    }

    private bool CompareFunctionDeclarationNode(FunctionDeclarationNode x, FunctionDeclarationNode y)
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

    private bool CompareFunctionTypeNode(FunctionTypeNode x, FunctionTypeNode y)
    {
        if (!x.ParameterTypes.SequenceEqual(y.ParameterTypes, this))
            throw new Exception("Parameters don't match.");

        if (!Equals(x.ReturnType, y.ReturnType))
            throw new Exception("ReturnType doesn't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareGenericTypeNode(GenericTypeNode x, GenericTypeNode y)
    {
        if (x.Name != y.Name)
            throw new Exception("BaseType doesn't match.");

        if (!x.TypeArguments.SequenceEqual(y.TypeArguments, this))
            throw new Exception("Arguments don't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareGoToNode(GoToNode x, GoToNode y)
    {
        if (x.Label != y.Label)
            throw new Exception($"Label doesn't match. {x.Label} != {y.Label}.");

        return true;
    }

    private bool CompareIfDirectiveNode(IfDirectiveNode x, IfDirectiveNode y)
    {
        if (x.DirectiveName != y.DirectiveName)
            throw new Exception($"Condition doesn't match. {x.DirectiveName} != {y.DirectiveName}.");

        if (!x.Then.SequenceEqual(y.Then, this))
            throw new Exception("IfBranch doesn't match.");

        if (!x.Else.SequenceEqual(y.Else, this))
            throw new Exception("ElseBranch doesn't match.");

        return true;
    }

    private bool CompareIfStatementNode(IfStatementNode x, IfStatementNode y)
    {
        if (!Equals(x.Condition, y.Condition))
            throw new Exception("Condition doesn't match.");

        if (!Equals(x.Then, y.Then))
            throw new Exception("IfBranch doesn't match.");

        if (!Equals(x.Else, y.Else))
            throw new Exception("ElseBranch doesn't match.");

        return true;
    }

    private bool CompareInterfaceMethodNode(InterfaceMethodNode x, InterfaceMethodNode y)
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

    private bool CompareInterfaceNode(InterfaceNode x, InterfaceNode y)
    {
        if (!x.Properties.SequenceEqual(y.Properties, this))
            throw new Exception("Properties don't match.");

        if (!x.Methods.SequenceEqual(y.Methods, this))
            throw new Exception("Methods don't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareInterfacePropertyNode(InterfacePropertyNode x, InterfacePropertyNode y)
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

    private bool CompareLabelNode(LabelNode x, LabelNode y)
    {
        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        return true;
    }

    private bool CompareLiteralExpressionNode(LiteralExpressionNode x, LiteralExpressionNode y)
    {
        if (x.Kind != y.Kind)
            throw new Exception($"Literal doesn't match. {x.Kind} != {y.Kind}.");

        if (!x.Value.Equals(y.Value))
            throw new Exception($"Value doesn't match. {x.Value} != {y.Value}.");

        if (!new MetadataComparer().Equals(x.ReturnTypeMetadata, y.ReturnTypeMetadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareMemberAccessExpressionNode(MemberAccessExpressionNode x, MemberAccessExpressionNode y)
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

    private bool CompareMethodDeclarationNode(MethodDeclarationNode x, MethodDeclarationNode y)
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

    private bool CompareNewArrayExpressionNode(NewArrayExpressionNode x, NewArrayExpressionNode y)
    {
        if (!Equals(x.Type, y.Type))
            throw new Exception("Type doesn't match.");

        if (!Equals(x.Size, y.Size))
            throw new Exception("Sizes don't match.");

        if (!new MetadataComparer().Equals(x.ReturnTypeMetadata, y.ReturnTypeMetadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareNewObjectExpressionNode(NewObjectExpressionNode x, NewObjectExpressionNode y)
    {
        if (!Equals(x.Type, y.Type))
            throw new Exception("Type doesn't match.");

        if (!x.Parameters.SequenceEqual(y.Parameters, this))
            throw new Exception("Arguments don't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareNullExpressionNode(NullExpressionNode x, NullExpressionNode y)
    {
        return true;
    }

    private bool CompareParameterNode(ParameterNode x, ParameterNode y)
    {
        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        if (!Equals(x.Type, y.Type))
            throw new Exception("Type doesn't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool ComparePropertyDeclarationNode(PropertyDeclarationNode x, PropertyDeclarationNode y)
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

    private bool ComparePropertyGetterNode(PropertyGetterNode x, PropertyGetterNode y)
    {
        if (x.AccessModifier != y.AccessModifier)
            throw new Exception($"AccessModifier doesn't match. {x.AccessModifier} != {y.AccessModifier}.");

        if (!Equals(x.Body, y.Body))
            throw new Exception("Body doesn't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool ComparePropertySetterNode(PropertySetterNode x, PropertySetterNode y)
    {
        if (x.AccessModifier != y.AccessModifier)
            throw new Exception($"AccessModifier doesn't match. {x.AccessModifier} != {y.AccessModifier}.");

        if (!Equals(x.Body, y.Body))
            throw new Exception("Body doesn't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareReturnStatementNode(ReturnStatementNode x, ReturnStatementNode y)
    {
        if (!Equals(x.Expression, y.Expression))
            throw new Exception("Expression doesn't match.");

        return true;
    }

    private bool CompareSyntaxTree(SyntaxTree x, SyntaxTree y)
    {
        if (!x.Declarations.SequenceEqual(y.Declarations, this))
            throw new Exception("Members don't match.");

        return true;
    }

    private bool CompareTupleExpressionNode(TupleExpressionNode x, TupleExpressionNode y)
    {
        if (!x.Expressions.SequenceEqual(y.Expressions, this))
            throw new Exception("Elements don't match.");

        if (!new MetadataComparer().Equals(x.ReturnTypeMetadata, y.ReturnTypeMetadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareTupleTypeNode(TupleTypeNode x, TupleTypeNode y)
    {
        if (!x.Types.SequenceEqual(y.Types, this))
            throw new Exception("Elements don't match.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareTypeAliasDeclarationNode(TypeAliasDeclarationNode x, TypeAliasDeclarationNode y)
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

    private bool CompareTypeDeclarationNode(TypeDeclarationNode x, TypeDeclarationNode y)
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

    private bool CompareTypeNode(TypeNode x, TypeNode y)
    {
        if (x.Name != y.Name)
            throw new Exception($"Name doesn't match. {x.Name} != {y.Name}.");

        if (!new MetadataComparer().Equals(x.Metadata, y.Metadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareUnaryExpressionNode(UnaryExpressionNode x, UnaryExpressionNode y)
    {
        if (x.Kind != y.Kind)
            throw new Exception($"Operator doesn't match. {x.Kind} != {y.Kind}.");

        if (!Equals(x.Operand, y.Operand))
            throw new Exception("Operand doesn't match.");

        if (!new MetadataComparer().Equals(x.ReturnTypeMetadata, y.ReturnTypeMetadata))
            throw new Exception("Metadata doesn't match.");

        return true;
    }

    private bool CompareVariableDeclarationStatementNode(VariableDeclarationStatementNode x, VariableDeclarationStatementNode y)
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

    private bool CompareWhileNode(WhileNode x, WhileNode y)
    {
        if (!Equals(x.Condition, y.Condition))
            throw new Exception("Condition doesn't match.");

        if (!Equals(x.Body, y.Body))
            throw new Exception("Body doesn't match.");

        return true;
    }

    public int GetHashCode(ISyntaxNode obj)
        => obj.GetHashCode();
}
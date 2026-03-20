using System.Text;
using Trilang.Parsing.Ast;

namespace Tri.Tests;

// ReSharper disable all UnusedParameter.Local
internal class SyntaxComparer : IEqualityComparer<ISyntaxNode>
{
    public static readonly SyntaxComparer Instance = new SyntaxComparer();

    public bool Equals(ISyntaxNode? x, ISyntaxNode? y)
    {
        var errors = new List<string>();
        CompareNodes(x, y, errors);

        if (errors.Count == 0)
            return true;

        var sb = new StringBuilder();
        sb.AppendLine("Syntax comparison failed with the following mismatches:");
        foreach (var err in errors)
            sb.AppendLine(" - " + err);

        throw new Exception(sb.ToString());
    }

    private void CompareNodes(ISyntaxNode? x, ISyntaxNode? y, List<string> errors)
    {
        switch (x, y)
        {
            case (null, null):
                return;
            case (null, _):
                errors.Add("x is null but y is not.");
                return;
            case (_, null):
                errors.Add("y is null but x is not.");
                return;

            case (AliasDeclarationNode x1, AliasDeclarationNode y1):
                CompareAliasDeclarationNode(x1, y1, errors);
                return;
            case (ArrayAccessExpressionNode x1, ArrayAccessExpressionNode y1):
                CompareArrayAccessExpressionNode(x1, y1, errors);
                return;
            case (ArrayTypeNode x1, ArrayTypeNode y1):
                CompareArrayTypeNode(x1, y1, errors);
                return;
            case (BinaryExpressionNode x1, BinaryExpressionNode y1):
                CompareBinaryExpressionNode(x1, y1, errors);
                return;
            case (BlockStatementNode x1, BlockStatementNode y1):
                CompareBlockStatementNode(x1, y1, errors);
                return;
            case (BreakNode x1, BreakNode y1):
                CompareBreakNode(x1, y1, errors);
                return;
            case (CallExpressionNode x1, CallExpressionNode y1):
                CompareCallExpressionNode(x1, y1, errors);
                return;
            case (CastExpressionNode x1, CastExpressionNode y1):
                CompareCastExpressionNode(x1, y1, errors);
                return;
            case (ConstructorDeclarationNode x1, ConstructorDeclarationNode y1):
                CompareConstructorDeclarationNode(x1, y1, errors);
                return;
            case (ContinueNode x1, ContinueNode y1):
                CompareContinueNode(x1, y1, errors);
                return;
            case (DiscriminatedUnionNode x1, DiscriminatedUnionNode y1):
                CompareDiscriminatedUnionNode(x1, y1, errors);
                return;
            case (ExpressionStatementNode x1, ExpressionStatementNode y1):
                CompareExpressionStatementNode(x1, y1, errors);
                return;
            case (FakeDeclarationNode x1, FakeDeclarationNode y1):
                CompareFakeDeclarationNode(x1, y1, errors);
                return;
            case (FakeExpressionNode x1, FakeExpressionNode y1):
                CompareFakeExpressionNode(x1, y1, errors);
                return;
            case (FakeStatementNode x1, FakeStatementNode y1):
                CompareFakeStatementNode(x1, y1, errors);
                return;
            case (FakeTypeNode x1, FakeTypeNode y1):
                CompareFakeTypeNode(x1, y1, errors);
                return;
            case (FunctionDeclarationNode x1, FunctionDeclarationNode y1):
                CompareFunctionDeclarationNode(x1, y1, errors);
                return;
            case (FunctionTypeNode x1, FunctionTypeNode y1):
                CompareFunctionTypeNode(x1, y1, errors);
                return;
            case (GenericApplicationNode x1, GenericApplicationNode y1):
                CompareGenericTypeNode(x1, y1, errors);
                return;
            case (IfDirectiveNode x1, IfDirectiveNode y1):
                CompareIfDirectiveNode(x1, y1, errors);
                return;
            case (IfStatementNode x1, IfStatementNode y1):
                CompareIfStatementNode(x1, y1, errors);
                return;
            case (InterfaceMethodNode x1, InterfaceMethodNode y1):
                CompareInterfaceMethodNode(x1, y1, errors);
                return;
            case (InterfaceNode x1, InterfaceNode y1):
                CompareInterfaceNode(x1, y1, errors);
                return;
            case (InterfacePropertyNode x1, InterfacePropertyNode y1):
                CompareInterfacePropertyNode(x1, y1, errors);
                return;
            case (IsExpressionNode x1, IsExpressionNode y1):
                CompareAsExpressionNode(x1, y1, errors);
                return;
            case (LiteralExpressionNode x1, LiteralExpressionNode y1):
                CompareLiteralExpressionNode(x1, y1, errors);
                return;
            case (MemberAccessExpressionNode x1, MemberAccessExpressionNode y1):
                CompareMemberAccessExpressionNode(x1, y1, errors);
                return;
            case (MethodDeclarationNode x1, MethodDeclarationNode y1):
                CompareMethodDeclarationNode(x1, y1, errors);
                return;
            case (NamespaceNode x1, NamespaceNode y1):
                CompareNamespaceNode(x1, y1, errors);
                return;
            case (NewArrayExpressionNode x1, NewArrayExpressionNode y1):
                CompareNewArrayExpressionNode(x1, y1, errors);
                return;
            case (NewObjectExpressionNode x1, NewObjectExpressionNode y1):
                CompareNewObjectExpressionNode(x1, y1, errors);
                return;
            case (NullExpressionNode x1, NullExpressionNode y1):
                CompareNullExpressionNode(x1, y1, errors);
                return;
            case (ParameterNode x1, ParameterNode y1):
                CompareParameterNode(x1, y1, errors);
                return;
            case (PropertyDeclarationNode x1, PropertyDeclarationNode y1):
                ComparePropertyDeclarationNode(x1, y1, errors);
                return;
            case (PropertyGetterNode x1, PropertyGetterNode y1):
                ComparePropertyGetterNode(x1, y1, errors);
                return;
            case (PropertySetterNode x1, PropertySetterNode y1):
                ComparePropertySetterNode(x1, y1, errors);
                return;
            case (ReturnStatementNode x1, ReturnStatementNode y1):
                CompareReturnStatementNode(x1, y1, errors);
                return;
            case (SyntaxTree x1, SyntaxTree y1):
                CompareSyntaxTree(x1, y1, errors);
                return;
            case (TupleExpressionNode x1, TupleExpressionNode y1):
                CompareTupleExpressionNode(x1, y1, errors);
                return;
            case (TupleTypeNode x1, TupleTypeNode y1):
                CompareTupleTypeNode(x1, y1, errors);
                return;
            case (TypeDeclarationNode x1, TypeDeclarationNode y1):
                CompareTypeDeclarationNode(x1, y1, errors);
                return;
            case (TypeRefNode x1, TypeRefNode y1):
                CompareTypeNode(x1, y1, errors);
                return;
            case (UnaryExpressionNode x1, UnaryExpressionNode y1):
                CompareUnaryExpressionNode(x1, y1, errors);
                return;
            case (UseNode x1, UseNode y1):
                CompareUseNode(x1, y1, errors);
                return;
            case (VariableDeclarationNode x1, VariableDeclarationNode y1):
                CompareVariableDeclarationStatementNode(x1, y1, errors);
                return;
            case (WhileNode x1, WhileNode y1):
                CompareWhileNode(x1, y1, errors);
                return;

            default:
                errors.Add($"{x.GetType()} != {y.GetType()}");
                return;
        }
    }

    private void CompareAliasDeclarationNode(AliasDeclarationNode x, AliasDeclarationNode y, List<string> errors)
    {
        if (x.AccessModifier != y.AccessModifier)
            errors.Add($"TypeAlias: AccessModifier mismatch. {x.AccessModifier} != {y.AccessModifier}.");

        if (x.Name != y.Name)
            errors.Add($"TypeAlias: Name mismatch. {x.Name} != {y.Name}.");

        CompareNodes(x.Type, y.Type, errors);
        CompareSequences(x.GenericArguments, y.GenericArguments, errors, "TypeAlias.GenericArguments");
    }

    private void CompareArrayAccessExpressionNode(ArrayAccessExpressionNode x, ArrayAccessExpressionNode y, List<string> errors)
    {
        CompareNodes(x.Member, y.Member, errors);
        CompareNodes(x.Index, y.Index, errors);
    }

    private void CompareArrayTypeNode(ArrayTypeNode x, ArrayTypeNode y, List<string> errors)
    {
        CompareNodes(x.ElementType, y.ElementType, errors);
    }

    private void CompareBinaryExpressionNode(BinaryExpressionNode x, BinaryExpressionNode y, List<string> errors)
    {
        CompareNodes(x.Left, y.Left, errors);

        if (x.Kind != y.Kind)
            errors.Add($"BinaryExpression: Operator mismatch. {x.Kind} != {y.Kind}.");

        CompareNodes(x.Right, y.Right, errors);
    }

    private void CompareBlockStatementNode(BlockStatementNode x, BlockStatementNode y, List<string> errors)
    {
        CompareSequences(x.Statements, y.Statements, errors, "BlockStatement.Statements");
    }

    private void CompareBreakNode(BreakNode x, BreakNode y, List<string> errors)
    {
    }

    private void CompareCallExpressionNode(CallExpressionNode x, CallExpressionNode y, List<string> errors)
    {
        CompareNodes(x.Member, y.Member, errors);
        CompareSequences(x.Parameters, y.Parameters, errors, "CallExpression.Parameters");
    }

    private void CompareCastExpressionNode(CastExpressionNode x, CastExpressionNode y, List<string> errors)
    {
        CompareNodes(x.Type, y.Type, errors);
        CompareNodes(x.Expression, y.Expression, errors);
    }

    private void CompareConstructorDeclarationNode(ConstructorDeclarationNode x, ConstructorDeclarationNode y, List<string> errors)
    {
        if (x.AccessModifier != y.AccessModifier)
            errors.Add($"Constructor: AccessModifier mismatch. {x.AccessModifier} != {y.AccessModifier}.");

        CompareSequences(x.Parameters, y.Parameters, errors, "Constructor.Parameters");
        CompareNodes(x.Body, y.Body, errors);
    }

    private void CompareContinueNode(ContinueNode x, ContinueNode y, List<string> errors)
    {
    }

    private void CompareDiscriminatedUnionNode(DiscriminatedUnionNode x, DiscriminatedUnionNode y, List<string> errors)
    {
        CompareSequences(x.Types, y.Types, errors, "DiscriminatedUnion.Types");
    }

    private void CompareExpressionStatementNode(ExpressionStatementNode x, ExpressionStatementNode y, List<string> errors)
    {
        CompareNodes(x.Expression, y.Expression, errors);
    }

    private void CompareFakeDeclarationNode(FakeDeclarationNode x, FakeDeclarationNode y, List<string> errors)
    {
    }

    private void CompareFakeExpressionNode(FakeExpressionNode x, FakeExpressionNode y, List<string> errors)
    {
    }

    private void CompareFakeStatementNode(FakeStatementNode x, FakeStatementNode y, List<string> errors)
    {
    }

    private void CompareFakeTypeNode(FakeTypeNode x, FakeTypeNode y, List<string> errors)
    {
    }

    private void CompareFunctionDeclarationNode(FunctionDeclarationNode x, FunctionDeclarationNode y, List<string> errors)
    {
        if (x.Name != y.Name)
            errors.Add($"Function: Name mismatch. {x.Name} != {y.Name}.");

        CompareSequences(x.Parameters, y.Parameters, errors, "Function.Parameters");
        CompareNodes(x.ReturnType, y.ReturnType, errors);
        CompareNodes(x.Body, y.Body, errors);
    }

    private void CompareFunctionTypeNode(FunctionTypeNode x, FunctionTypeNode y, List<string> errors)
    {
        CompareSequences(x.ParameterTypes, y.ParameterTypes, errors, "FunctionType.Parameters");
        CompareNodes(x.ReturnType, y.ReturnType, errors);
    }

    private void CompareGenericTypeNode(GenericApplicationNode x, GenericApplicationNode y, List<string> errors)
    {
        CompareSequences(x.TypeArguments, y.TypeArguments, errors, "GenericType.Arguments");
    }

    private void CompareIfDirectiveNode(IfDirectiveNode x, IfDirectiveNode y, List<string> errors)
    {
        if (x.DirectiveName != y.DirectiveName)
            errors.Add($"IfDirective: DirectiveName mismatch. {x.DirectiveName} != {y.DirectiveName}.");

        CompareSequences(x.Then, y.Then, errors, "IfDirective.Then");
        CompareSequences(x.Else, y.Else, errors, "IfDirective.Else");
    }

    private void CompareIfStatementNode(IfStatementNode x, IfStatementNode y, List<string> errors)
    {
        CompareNodes(x.Condition, y.Condition, errors);
        CompareNodes(x.Then, y.Then, errors);
        CompareNodes(x.Else, y.Else, errors);
    }

    private void CompareInterfaceMethodNode(InterfaceMethodNode x, InterfaceMethodNode y, List<string> errors)
    {
        if (x.Name != y.Name)
            errors.Add($"InterfaceMethod: Name mismatch. {x.Name} != {y.Name}.");

        CompareSequences(x.ParameterTypes, y.ParameterTypes, errors, "InterfaceMethod.Parameters");
        CompareNodes(x.ReturnType, y.ReturnType, errors);
    }

    private void CompareInterfaceNode(InterfaceNode x, InterfaceNode y, List<string> errors)
    {
        CompareSequences(x.Properties, y.Properties, errors, "Interface.Properties");
        CompareSequences(x.Methods, y.Methods, errors, "Interface.Methods");
    }

    private void CompareInterfacePropertyNode(InterfacePropertyNode x, InterfacePropertyNode y, List<string> errors)
    {
        if (x.Name != y.Name)
            errors.Add($"InterfaceProperty: Name mismatch. {x.Name} != {y.Name}.");

        CompareNodes(x.Type, y.Type, errors);

        if (x.GetterModifier != y.GetterModifier)
            errors.Add($"InterfaceProperty: GetterModifier mismatch. {x.GetterModifier} != {y.GetterModifier}.");

        if (x.SetterModifier != y.SetterModifier)
            errors.Add($"InterfaceProperty: SetterModifier mismatch. {x.SetterModifier} != {y.SetterModifier}.");
    }

    private void CompareAsExpressionNode(IsExpressionNode x, IsExpressionNode y, List<string> errors)
    {
        CompareNodes(x.Expression, y.Expression, errors);
        CompareNodes(x.Type, y.Type, errors);
    }

    private void CompareLiteralExpressionNode(LiteralExpressionNode x, LiteralExpressionNode y, List<string> errors)
    {
        if (x.Kind != y.Kind)
            errors.Add($"LiteralExpression: Kind mismatch. {x.Kind} != {y.Kind}.");

        if (!x.Value.Equals(y.Value))
            errors.Add($"LiteralExpression: Value mismatch. {x.Value} != {y.Value}.");
    }

    private void CompareMemberAccessExpressionNode(MemberAccessExpressionNode x, MemberAccessExpressionNode y, List<string> errors)
    {
        CompareNodes(x.Member, y.Member, errors);

        if (x.Name != y.Name)
            errors.Add($"MemberAccessExpression: Name mismatch. {x.Name} != {y.Name}.");
    }

    private void CompareMethodDeclarationNode(MethodDeclarationNode x, MethodDeclarationNode y, List<string> errors)
    {
        if (x.AccessModifier != y.AccessModifier)
            errors.Add($"Method: AccessModifier mismatch. {x.AccessModifier} != {y.AccessModifier}.");

        if (x.IsStatic != y.IsStatic)
            errors.Add($"Method: IsStatic mismatch. {x.IsStatic} != {y.IsStatic}.");

        if (x.Name != y.Name)
            errors.Add($"Method: Name mismatch. {x.Name} != {y.Name}.");

        CompareSequences(x.Parameters, y.Parameters, errors, "Method.Parameters");
        CompareNodes(x.ReturnType, y.ReturnType, errors);
        CompareNodes(x.Body, y.Body, errors);
    }

    private void CompareNamespaceNode(NamespaceNode x, NamespaceNode y, List<string> errors)
    {
        if (!x.Parts.SequenceEqual(y.Parts))
            errors.Add($"Namespace: Parts mismatch. {string.Join(".", x.Parts)} != {string.Join(".", y.Parts)}.");
    }

    private void CompareNewArrayExpressionNode(NewArrayExpressionNode x, NewArrayExpressionNode y, List<string> errors)
    {
        CompareNodes(x.Type, y.Type, errors);
        CompareNodes(x.Size, y.Size, errors);
    }

    private void CompareNewObjectExpressionNode(NewObjectExpressionNode x, NewObjectExpressionNode y, List<string> errors)
    {
        CompareNodes(x.Type, y.Type, errors);
        CompareSequences(x.Parameters, y.Parameters, errors, "NewObjectExpression.Parameters");
    }

    private void CompareNullExpressionNode(NullExpressionNode x, NullExpressionNode y, List<string> errors)
    {
    }

    private void CompareParameterNode(ParameterNode x, ParameterNode y, List<string> errors)
    {
        if (x.Name != y.Name)
            errors.Add($"Parameter: Name mismatch. {x.Name} != {y.Name}.");

        CompareNodes(x.Type, y.Type, errors);
    }

    private void ComparePropertyDeclarationNode(PropertyDeclarationNode x, PropertyDeclarationNode y, List<string> errors)
    {
        if (x.Name != y.Name)
            errors.Add($"Property: Name mismatch. {x.Name} != {y.Name}.");

        CompareNodes(x.Type, y.Type, errors);
        CompareNodes(x.Getter, y.Getter, errors);
        CompareNodes(x.Setter, y.Setter, errors);
    }

    private void ComparePropertyGetterNode(PropertyGetterNode x, PropertyGetterNode y, List<string> errors)
    {
        CompareNodes(x.Body, y.Body, errors);
    }

    private void ComparePropertySetterNode(PropertySetterNode x, PropertySetterNode y, List<string> errors)
    {
        CompareNodes(x.Body, y.Body, errors);
    }

    private void CompareReturnStatementNode(ReturnStatementNode x, ReturnStatementNode y, List<string> errors)
    {
        CompareNodes(x.Expression, y.Expression, errors);
    }

    private void CompareSyntaxTree(SyntaxTree x, SyntaxTree y, List<string> errors)
    {
        CompareSequences(x.UseNodes, y.UseNodes, errors, "SyntaxTree.UseNodes");
        CompareNodes(x.Namespace, y.Namespace, errors);
        CompareSequences(x.Declarations, y.Declarations, errors, "SyntaxTree.Declarations");

        if (!x.SourceFile.Equals(y.SourceFile))
            errors.Add($"SyntaxTree: SourceFile mismatch. Expected {x.SourceFile}, got {y.SourceFile}.");
    }

    private void CompareTupleExpressionNode(TupleExpressionNode x, TupleExpressionNode y, List<string> errors)
    {
        CompareSequences(x.Expressions, y.Expressions, errors, "TupleExpression.Elements");
    }

    private void CompareTupleTypeNode(TupleTypeNode x, TupleTypeNode y, List<string> errors)
    {
        CompareSequences(x.Types, y.Types, errors, "TupleType.Elements");
    }

    private void CompareTypeDeclarationNode(TypeDeclarationNode x, TypeDeclarationNode y, List<string> errors)
    {
        if (x.AccessModifier != y.AccessModifier)
            errors.Add($"TypeDeclaration: AccessModifier mismatch. {x.AccessModifier} != {y.AccessModifier}.");

        if (x.Name != y.Name)
            errors.Add($"TypeDeclaration: Name mismatch. {x.Name} != {y.Name}.");

        CompareSequences(x.GenericArguments, y.GenericArguments, errors, "TypeDeclaration.GenericArguments");
        CompareSequences(x.Interfaces, y.Interfaces, errors, "TypeDeclaration.Interfaces");
        CompareSequences(x.Properties, y.Properties, errors, "TypeDeclaration.Properties");
        CompareSequences(x.Constructors, y.Constructors, errors, "TypeDeclaration.Constructors");
        CompareSequences(x.Methods, y.Methods, errors, "TypeDeclaration.Methods");
    }

    private void CompareTypeNode(TypeRefNode x, TypeRefNode y, List<string> errors)
    {
        if (!x.Parts.SequenceEqual(y.Parts))
            errors.Add($"TypeRef: Parts mismatch. {string.Join(".", x.Parts)} != {string.Join(".", y.Parts)}.");
    }

    private void CompareUnaryExpressionNode(UnaryExpressionNode x, UnaryExpressionNode y, List<string> errors)
    {
        if (x.Kind != y.Kind)
            errors.Add($"UnaryExpression: Operator mismatch. {x.Kind} != {y.Kind}.");

        CompareNodes(x.Operand, y.Operand, errors);
    }

    private void CompareUseNode(UseNode x, UseNode y, List<string> errors)
    {
        if (!x.Parts.SequenceEqual(y.Parts))
            errors.Add($"Use: Parts mismatch. {string.Join(".", x.Parts)} != {string.Join(".", y.Parts)}.");
    }

    private void CompareVariableDeclarationStatementNode(VariableDeclarationNode x, VariableDeclarationNode y, List<string> errors)
    {
        if (x.Name != y.Name)
            errors.Add($"VariableDeclaration: Name mismatch. {x.Name} != {y.Name}.");

        CompareNodes(x.Type, y.Type, errors);
        CompareNodes(x.Expression, y.Expression, errors);
    }

    private void CompareWhileNode(WhileNode x, WhileNode y, List<string> errors)
    {
        CompareNodes(x.Condition, y.Condition, errors);
        CompareNodes(x.Body, y.Body, errors);
    }

    private void CompareSequences<T>(IReadOnlyList<T> xs, IReadOnlyList<T> ys, List<string> errors, string context)
        where T : ISyntaxNode
    {
        if (xs.Count != ys.Count)
        {
            errors.Add($"{context}: Count mismatch. {xs.Count} != {ys.Count}.");
        }
        else
        {
            for (var i = 0; i < xs.Count; i++)
                CompareNodes(xs[i], ys[i], errors);
        }
    }

    public int GetHashCode(ISyntaxNode obj) => obj.GetHashCode();
}
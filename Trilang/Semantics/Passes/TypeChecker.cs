using Trilang.Metadata;
using Trilang.Semantics.Model;
using static Trilang.Semantics.Model.BinaryExpressionKind;
using static Trilang.Semantics.Model.UnaryExpressionKind;
using static Trilang.Metadata.TypeMetadata;
using Type = Trilang.Semantics.Model.Type;

namespace Trilang.Semantics.Passes;

internal class TypeChecker : IVisitor, ISemanticPass
{
    private IEnumerable<string> directives = null!;
    private SymbolTableMap symbolTableMap = null!;

    public void Analyze(SemanticTree tree, SemanticPassContext context)
    {
        directives = context.Directives;
        symbolTableMap = context.SymbolTableMap!;

        tree.Accept(this);
    }

    public void VisitArrayAccess(ArrayAccessExpression node)
    {
        node.Member.Accept(this);
        node.Index.Accept(this);

        if (node.Member.ReturnTypeMetadata is not TypeArrayMetadata typeArray)
            throw new SemanticAnalysisException("Array access must be of type array");

        if (!Equals(node.Index.ReturnTypeMetadata, I32))
            throw new SemanticAnalysisException("Array index must be of type i32");

        node.ReturnTypeMetadata = typeArray.ItemMetadata;
    }

    public void VisitArrayType(ArrayType node)
    {
        node.ElementType.Accept(this);

        var typeProvider = symbolTableMap.Get(node).TypeProvider;
        node.Metadata = typeProvider.GetType(node.Name) ??
                        throw new SemanticAnalysisException($"Unknown array type '{node.Name}'");
    }

    public void VisitBinaryExpression(BinaryExpression node)
    {
        node.Left.Accept(this);
        node.Right.Accept(this);

        // TODO: more complex logic
        if (node.Kind == BinaryExpressionKind.Unknown ||
            node.Left.ReturnTypeMetadata is null ||
            node.Right.ReturnTypeMetadata is null)
            throw new SemanticAnalysisException("Invalid binary expression: unknown kind or operand types");

        if (node.Kind is Addition or Subtraction or Multiplication or Division or Modulus &&
            ((Equals(node.Left.ReturnTypeMetadata, I8) && Equals(node.Right.ReturnTypeMetadata, I8)) ||
             (Equals(node.Left.ReturnTypeMetadata, I16) && Equals(node.Right.ReturnTypeMetadata, I16)) ||
             (Equals(node.Left.ReturnTypeMetadata, I32) && Equals(node.Right.ReturnTypeMetadata, I32)) ||
             (Equals(node.Left.ReturnTypeMetadata, I64) && Equals(node.Right.ReturnTypeMetadata, I64)) ||
             (Equals(node.Left.ReturnTypeMetadata, U8) && Equals(node.Right.ReturnTypeMetadata, U8)) ||
             (Equals(node.Left.ReturnTypeMetadata, U16) && Equals(node.Right.ReturnTypeMetadata, U16)) ||
             (Equals(node.Left.ReturnTypeMetadata, U32) && Equals(node.Right.ReturnTypeMetadata, U32)) ||
             (Equals(node.Left.ReturnTypeMetadata, U64) && Equals(node.Right.ReturnTypeMetadata, U64)) ||
             (Equals(node.Left.ReturnTypeMetadata, F32) && Equals(node.Right.ReturnTypeMetadata, F32)) ||
             (Equals(node.Left.ReturnTypeMetadata, F64) && Equals(node.Right.ReturnTypeMetadata, F64))))
        {
            node.ReturnTypeMetadata = node.Left.ReturnTypeMetadata;
        }
        else if (node.Kind is BitwiseAnd or BitwiseOr or BitwiseXor &&
                 ((Equals(node.Left.ReturnTypeMetadata, I8) && Equals(node.Right.ReturnTypeMetadata, I8)) ||
                  (Equals(node.Left.ReturnTypeMetadata, I16) && Equals(node.Right.ReturnTypeMetadata, I16)) ||
                  (Equals(node.Left.ReturnTypeMetadata, I32) && Equals(node.Right.ReturnTypeMetadata, I32)) ||
                  (Equals(node.Left.ReturnTypeMetadata, I64) && Equals(node.Right.ReturnTypeMetadata, I64)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U8) && Equals(node.Right.ReturnTypeMetadata, U8)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U16) && Equals(node.Right.ReturnTypeMetadata, U16)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U32) && Equals(node.Right.ReturnTypeMetadata, U32)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U64) && Equals(node.Right.ReturnTypeMetadata, U64))))
        {
            node.ReturnTypeMetadata = node.Left.ReturnTypeMetadata;
        }
        else if (node.Kind is ConditionalAnd or ConditionalOr &&
                 Equals(node.Left.ReturnTypeMetadata, Bool) && Equals(node.Right.ReturnTypeMetadata, Bool))
        {
            node.ReturnTypeMetadata = Bool;
        }
        else if (node.Kind is Equality or Inequality or
                     LessThan or LessThanOrEqual or
                     GreaterThan or GreaterThanOrEqual &&
                 ((Equals(node.Left.ReturnTypeMetadata, I8) && Equals(node.Right.ReturnTypeMetadata, I8)) ||
                  (Equals(node.Left.ReturnTypeMetadata, I16) && Equals(node.Right.ReturnTypeMetadata, I16)) ||
                  (Equals(node.Left.ReturnTypeMetadata, I32) && Equals(node.Right.ReturnTypeMetadata, I32)) ||
                  (Equals(node.Left.ReturnTypeMetadata, I64) && Equals(node.Right.ReturnTypeMetadata, I64)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U8) && Equals(node.Right.ReturnTypeMetadata, U8)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U16) && Equals(node.Right.ReturnTypeMetadata, U16)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U32) && Equals(node.Right.ReturnTypeMetadata, U32)) ||
                  (Equals(node.Left.ReturnTypeMetadata, U64) && Equals(node.Right.ReturnTypeMetadata, U64))))
        {
            node.ReturnTypeMetadata = Bool;
        }
        else if (node.Kind is Assignment or AdditionAssignment or
                     SubtractionAssignment or MultiplicationAssignment or
                     DivisionAssignment or ModulusAssignment &&
                 node.Left is MemberAccessExpression or ArrayAccessExpression &&
                 Equals(node.Left.ReturnTypeMetadata, node.Right.ReturnTypeMetadata))
        {
            node.ReturnTypeMetadata = node.Right.ReturnTypeMetadata;
        }
        else if (node.Kind is AdditionAssignment or SubtractionAssignment or MultiplicationAssignment or DivisionAssignment or ModulusAssignment &&
                 node.Left is MemberAccessExpression &&
                 (Equals(node.Right.ReturnTypeMetadata, I8) ||
                  Equals(node.Right.ReturnTypeMetadata, I16) ||
                  Equals(node.Right.ReturnTypeMetadata, I32) ||
                  Equals(node.Right.ReturnTypeMetadata, I64) ||
                  Equals(node.Right.ReturnTypeMetadata, U8) ||
                  Equals(node.Right.ReturnTypeMetadata, U16) ||
                  Equals(node.Right.ReturnTypeMetadata, U32) ||
                  Equals(node.Right.ReturnTypeMetadata, U64) ||
                  Equals(node.Right.ReturnTypeMetadata, F32) ||
                  Equals(node.Right.ReturnTypeMetadata, F64)))
        {
            node.ReturnTypeMetadata = node.Right.ReturnTypeMetadata;
        }
        else if (node.Kind is BitwiseAndAssignment or BitwiseOrAssignment or BitwiseXorAssignment &&
                 node.Left is MemberAccessExpression &&
                 (Equals(node.Right.ReturnTypeMetadata, I8) ||
                  Equals(node.Right.ReturnTypeMetadata, I16) ||
                  Equals(node.Right.ReturnTypeMetadata, I32) ||
                  Equals(node.Right.ReturnTypeMetadata, I64) ||
                  Equals(node.Right.ReturnTypeMetadata, U8) ||
                  Equals(node.Right.ReturnTypeMetadata, U16) ||
                  Equals(node.Right.ReturnTypeMetadata, U32) ||
                  Equals(node.Right.ReturnTypeMetadata, U64)))
        {
            node.ReturnTypeMetadata = node.Right.ReturnTypeMetadata;
        }
        else
        {
            throw new SemanticAnalysisException($"Invalid binary expression: incompatible operand types '{node.Left.ReturnTypeMetadata}' and '{node.Right.ReturnTypeMetadata}' for operator '{node.Kind}'");
        }
    }

    public void VisitBlock(BlockStatement node)
    {
        foreach (var statement in node.Statements)
            statement.Accept(this);
    }

    public void VisitBreak(Break node)
    {
    }

    public void VisitCall(CallExpression node)
    {
        // TODO: unused return value
        node.Member.Accept(this);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        if (node.Member.ReturnTypeMetadata is not FunctionTypeMetadata function)
            throw new SemanticAnalysisException("Cannot call a non-function member");

        for (var i = 0; i < node.Parameters.Count; i++)
        {
            var actual = node.Parameters[i].ReturnTypeMetadata;
            var expected = function.ParameterTypes.ElementAt(i);
            if (!expected.Equals(actual))
                throw new SemanticAnalysisException($"Expected '{expected}' but got '{actual}'");
        }
    }

    public void VisitCast(CastExpression node)
    {
        node.Type.Accept(this);
        node.Expression.Accept(this);
    }

    public void VisitConstructor(ConstructorDeclaration node)
    {
        foreach (var parameter in node.Parameters)
            parameter.Metadata = node.Metadata!.Parameters.FirstOrDefault(x => x.Name == parameter.Name);

        node.Body.Accept(this);
    }

    public void VisitContinue(Continue node)
    {
    }

    public void VisitDiscriminatedUnion(DiscriminatedUnion node)
    {
        // TODO: eliminate duplicates
        // TODO: restrict recursive types
        foreach (var type in node.Types)
            type.Accept(this);

        var typeProvider = symbolTableMap.Get(node).TypeProvider;
        var metadata = typeProvider.GetType(node.Name) ??
                       throw new SemanticAnalysisException($"Unknown discriminated union type '{node.Name}'");

        node.Metadata = metadata;
    }

    public void VisitExpressionBlock(ExpressionBlock node)
        => throw new SemanticAnalysisException("Expression blocks are not supported");

    public void VisitExpressionStatement(ExpressionStatement node)
    {
        // TODO: check whether the result of expression is used
        node.Expression.Accept(this);
    }

    public void VisitFakeDeclaration(FakeDeclaration node)
    {
        // FakeDeclaration is used to recover from parsing errors, so we just skip it here.
    }

    public void VisitFakeExpression(FakeExpression node)
    {
        // FakeExpression is used to recover from parsing errors, so we just skip it here.
    }

    public void VisitFakeStatement(FakeStatement node)
    {
        // FakeStatement is used to recover from parsing errors, so we just skip it here.
    }

    public void VisitFakeType(FakeType node)
    {
        // FakeType is used to recover from parsing errors, so we just skip it here.
    }

    public void VisitFunctionSignature(FunctionDeclaration node)
    {
        var parameters = new ParameterMetadata[node.Parameters.Count];
        for (var i = 0; i < node.Parameters.Count; i++)
        {
            var parameter = node.Parameters[i];
            parameter.Accept(this);

            parameters[i] = new ParameterMetadata(parameter.Name, parameter.Type.Metadata!);
        }

        node.ReturnType.Accept(this);

        // we can be sure that node.Metadata is not null here
        // because it is set as a part of TypeNode
        var typeProvider = symbolTableMap.Get(node).TypeProvider;
        var parameterTypes = node.Parameters.Select(x => x.Type.Metadata!);
        var returnType = node.ReturnType.Metadata!;
        var functionType = new FunctionTypeMetadata(parameterTypes, returnType);
        functionType = typeProvider.GetType(functionType.ToString()) as FunctionTypeMetadata ??
                       throw new SemanticAnalysisException($"Unknown function type '{functionType}'");

        node.Metadata = new FunctionMetadata(GetAccessModifierMetadata(node.AccessModifier), node.Name, parameters, functionType);

        foreach (var parameter in node.Parameters)
            parameter.Metadata = node.Metadata!.Parameters.FirstOrDefault(x => x.Name == parameter.Name);
    }

    public void VisitFunction(FunctionDeclaration node)
        => node.Body.Accept(this);

    public void VisitFunctionType(FunctionType node)
    {
        foreach (var parameterType in node.ParameterTypes)
            parameterType.Accept(this);

        node.ReturnType.Accept(this);

        var typeProvider = symbolTableMap.Get(node).TypeProvider;
        var parameters = node.ParameterTypes.Select(x => x.Metadata!);
        var returnType = node.ReturnType.Metadata!;
        var functionType = new FunctionTypeMetadata(parameters, returnType);
        functionType = typeProvider.GetType(functionType.ToString()) as FunctionTypeMetadata ??
                       throw new SemanticAnalysisException($"Unknown function type '{functionType}'");

        node.Metadata = functionType;
    }

    public void VisitGenericType(GenericType node)
    {
        foreach (var typeArgument in node.TypeArguments)
            typeArgument.Accept(this);

        var typeProvider = symbolTableMap.Get(node).TypeProvider;
        var metadata = typeProvider.GetType(node.Name) ??
                       typeProvider.GetType(node.GetOpenGenericName()) ??
                       throw new SemanticAnalysisException($"Unknown generic type '{node.Name}'");

        node.Metadata = metadata;
    }

    public void VisitGoTo(GoTo node)
    {
    }

    public void VisitIfDirective(IfDirective node)
    {
        if (directives.Contains(node.DirectiveName))
        {
            foreach (var then in node.Then)
                then.Accept(this);
        }
        else
        {
            foreach (var @else in node.Else)
                @else.Accept(this);
        }
    }

    public void VisitIf(IfStatement node)
    {
        // TODO: data flow
        node.Condition.Accept(this);
        node.Then.Accept(this);
        node.Else?.Accept(this);

        if (!Equals(node.Condition.ReturnTypeMetadata, Bool))
            throw new SemanticAnalysisException("The condition returns non-boolean type.");
    }

    public void VisitInterface(Interface node)
    {
        var typeProvider = symbolTableMap.Get(node).TypeProvider;
        var metadata = typeProvider.GetType(node.Name) as InterfaceMetadata ??
                       throw new SemanticAnalysisException($"Unknown interface type '{node.Name}'");

        node.Metadata = metadata;

        foreach (var property in node.Properties)
            property.Accept(this);

        foreach (var method in node.Methods)
            method.Accept(this);
    }

    public void VisitInterfaceProperty(InterfaceProperty node)
    {
        node.Type.Accept(this);

        var type = (InterfaceMetadata)((Interface)node.Parent!).Metadata!;
        node.Metadata = type.GetProperty(node.Name);
    }

    public void VisitInterfaceMethod(InterfaceMethod node)
    {
        foreach (var parameter in node.ParameterTypes)
            parameter.Accept(this);

        node.ReturnType.Accept(this);

        var type = (InterfaceMetadata)((Interface)node.Parent!).Metadata!;
        node.Metadata = type.GetMethod(node.Name);
    }

    public void VisitIsExpression(IsExpression node)
    {
        node.Expression.Accept(this);
        node.Type.Accept(this);
    }

    public void VisitLabel(Label node)
    {
    }

    public void VisitLiteral(LiteralExpression node)
    {
        node.ReturnTypeMetadata = node.Kind switch
        {
            // TODO: other types
            LiteralExpressionKind.Integer => I32,
            LiteralExpressionKind.Float => F64,
            LiteralExpressionKind.Boolean => Bool,
            LiteralExpressionKind.String => TypeMetadata.String,
            LiteralExpressionKind.Char => TypeMetadata.Char,

            _ => throw new ArgumentOutOfRangeException(nameof(node.Kind), $"Unsupported literal expression kind: {node.Kind}"),
        };
    }

    public void VisitMemberAccess(MemberAccessExpression node)
    {
        if (node.IsFirstMember)
        {
            VisitFirstMemberAccess(node);
            return;
        }

        VisitNestedMemberAccess(node);

        if (node.ReturnTypeMetadata is null)
            throw new SemanticAnalysisException($"Cannot determine return type for member '{node.Name}'");
    }

    private void VisitFirstMemberAccess(MemberAccessExpression node)
    {
        var symbolTable = symbolTableMap.Get(node);
        var symbol = symbolTable.GetId(node.Name);
        if (symbol is not null)
        {
            node.Reference = symbol.Node switch
            {
                PropertyDeclaration propertyDeclarationNode
                    => propertyDeclarationNode.Metadata,

                VariableDeclaration variableStatementNode
                    => variableStatementNode.Metadata,

                Parameter parameterNode
                    => parameterNode.Metadata,

                FunctionDeclaration functionNode
                    => functionNode.Metadata,

                MethodDeclaration methodNode
                    => methodNode.Metadata,

                TypeDeclaration typeDeclarationNode when node.IsThis
                    => new ParameterMetadata(MemberAccessExpression.This, typeDeclarationNode.Metadata!),

                _ => throw new SemanticAnalysisException("Unknown symbol"),
            };

            return;
        }

        // static access
        var typeProvider = symbolTable.TypeProvider;
        node.Reference = typeProvider.GetType(node.Name);
    }

    private void VisitNestedMemberAccess(MemberAccessExpression node)
    {
        node.Member!.Accept(this);

        var returnTypeMetadata = node.Member.ReturnTypeMetadata!;
        node.Reference = returnTypeMetadata.GetMember(node.Name) ??
                         throw new SemanticAnalysisException($"Cannot find member '{node.Name}' in '{returnTypeMetadata}'");
    }

    public void VisitMethod(MethodDeclaration node)
    {
        foreach (var parameter in node.Parameters)
            parameter.Metadata = node.Metadata!.Parameters.FirstOrDefault(x => x.Name == parameter.Name);

        node.Body.Accept(this);
    }

    public void VisitNewArray(NewArrayExpression node)
    {
        node.Type.Accept(this);
        node.Size.Accept(this);

        node.ReturnTypeMetadata = node.Type.Metadata;
    }

    public void VisitNewObject(NewObjectExpression node)
    {
        node.Type.Accept(this);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        if (node.Type.Metadata is not TypeMetadata type || type.IsValueType)
            throw new SemanticAnalysisException($"Cannot create an instance of type '{node.Type.Metadata}'");

        var parameters = node.Parameters.Select(x => x.ReturnTypeMetadata!).ToList();
        var ctor = type.GetConstructor(parameters) ??
                   throw new SemanticAnalysisException($"The '{type.Name}' type doesn't have '{string.Join(", ", parameters)}' constructor.");

        node.Metadata = ctor;
    }

    public void VisitNull(NullExpression node)
    {
    }

    public void VisitReturn(ReturnStatement node)
    {
        node.Expression?.Accept(this);

        var expressionType = node.Expression?.ReturnTypeMetadata ?? TypeMetadata.Void;
        var method = node.FindInParent<MethodDeclaration>();
        if (method is not null)
        {
            var methodReturnType = method.Metadata?.Type.ReturnType;
            if (!Equals(methodReturnType, expressionType))
                throw new SemanticAnalysisException($"Method return type mismatch: expected '{methodReturnType}', got '{expressionType}'");

            return;
        }

        var constructor = node.FindInParent<ConstructorDeclaration>();
        if (constructor is not null)
        {
            if (!Equals(TypeMetadata.Void, expressionType))
                throw new SemanticAnalysisException($"Constructor return type mismatch: expected '{TypeMetadata.Void.Name}', got '{expressionType}'");

            return;
        }

        var function = node.FindInParent<FunctionDeclaration>();
        if (function is not null)
        {
            var functionReturnType = function.Metadata?.Type.ReturnType;
            if (!Equals(functionReturnType, expressionType))
                throw new SemanticAnalysisException($"Function return type mismatch: expected '{functionReturnType}', got '{expressionType}'");

            return;
        }

        var getter = node.FindInParent<PropertyGetter>();
        if (getter is not null)
        {
            var getterReturnType = ((PropertyDeclaration)getter.Parent!).Metadata!.Type;
            if (!Equals(getterReturnType, expressionType))
                throw new SemanticAnalysisException($"Property getter return type mismatch: expected '{getterReturnType}', got '{expressionType}'");

            return;
        }

        var setter = node.FindInParent<PropertySetter>();
        if (setter is not null)
        {
            var setterReturnType = ((PropertyDeclaration)setter.Parent!).Metadata!.Type;
            if (!Equals(setterReturnType, expressionType))
                throw new SemanticAnalysisException($"Property setter return type mismatch: expected '{setterReturnType}', got '{expressionType}'");

            return;
        }
    }

    public void VisitParameter(Parameter node)
        => node.Type.Accept(this);

    public void VisitProperty(PropertyDeclaration node)
    {
        node.Type.Accept(this);
        node.Getter?.Accept(this);
        node.Setter?.Accept(this);
    }

    public void VisitGetter(PropertyGetter node)
    {
        var property = (PropertyDeclaration)node.Parent!;
        var propertyMetadata = property.Metadata!;
        node.Metadata = propertyMetadata.Getter;

        node.Body?.Accept(this);
    }

    public void VisitSetter(PropertySetter node)
    {
        var property = (PropertyDeclaration)node.Parent!;
        var propertyMetadata = property.Metadata!;
        node.Metadata = propertyMetadata.Setter;

        // TODO: check the backing field is set?
        node.Body?.Accept(this);
    }

    public void VisitTree(SemanticTree node)
    {
        // preprocess function to generate correct metadata before processing bodies/other types
        foreach (var function in node.Declarations.OfType<FunctionDeclaration>())
            VisitFunctionSignature(function);

        foreach (var statement in node.Declarations)
            statement.Accept(this);
    }

    public void VisitTuple(TupleExpression node)
    {
        foreach (var expression in node.Expressions)
            expression.Accept(this);

        var typeProvider = symbolTableMap.Get(node).TypeProvider;

        // we can't generate metadata for this tuple in GenerateMetadata
        // because we don't know the types of the expressions yet
        var types = node.Expressions.Select(x => x.ReturnTypeMetadata!);
        var tuple = new TupleMetadata(types);
        var existingTuple = typeProvider.GetType(tuple.ToString());
        if (existingTuple is null)
        {
            typeProvider.DefineType(tuple.ToString(), tuple);
            existingTuple = tuple;
        }

        node.ReturnTypeMetadata = existingTuple;
    }

    public void VisitTupleType(TupleType node)
    {
        // TODO: restrict recursive types
        foreach (var type in node.Types)
            type.Accept(this);

        var typeProvider = symbolTableMap.Get(node).TypeProvider;
        var types = node.Types.Select(x => x.Metadata!);
        var tuple = new TupleMetadata(types);
        tuple = typeProvider.GetType(tuple.ToString()) as TupleMetadata ??
                throw new SemanticAnalysisException($"Unknown tuple type '{tuple}'");

        node.Metadata = tuple;
    }

    public void VisitTypeAlias(TypeAliasDeclaration node)
    {
        foreach (var genericArgument in node.GenericArguments)
            genericArgument.Accept(this);

        node.Type.Accept(this);

        var typeProvider = symbolTableMap.Get(node).TypeProvider;
        node.Metadata = typeProvider.GetType(node.FullName) ??
                        throw new SemanticAnalysisException($"Unknown type '{node.Name}'");
    }

    public void VisitType(TypeDeclaration node)
    {
        var typeProvider = symbolTableMap.Get(node).TypeProvider;
        var metadata = typeProvider.GetType(node.FullName);
        if (metadata is not TypeMetadata type)
            throw new SemanticAnalysisException($"The '{node.Name}' type is not a type.");

        node.Metadata = type;

        foreach (var genericArgument in node.GenericArguments)
            genericArgument.Accept(this);

        foreach (var @interface in node.Interfaces)
            @interface.Accept(this);

        // visit signatures first
        foreach (var property in node.Properties)
            VisitPropertySignature(property);

        foreach (var constructor in node.Constructors)
            VisitConstructorSignature(constructor);

        foreach (var method in node.Methods)
            VisitMethodSignature(method);

        // visit bodies later to support forward references
        foreach (var property in node.Properties)
            property.Accept(this);

        foreach (var constructor in node.Constructors)
            constructor.Accept(this);

        foreach (var method in node.Methods)
            method.Accept(this);
    }

    private void VisitPropertySignature(PropertyDeclaration node)
    {
        var type = ((TypeDeclaration)node.Parent!).Metadata!;
        node.Metadata = type.GetProperty(node.Name);
    }

    private void VisitConstructorSignature(ConstructorDeclaration node)
    {
        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        var type = ((TypeDeclaration)node.Parent!).Metadata!;
        node.Metadata = type.GetConstructor(node.Parameters.Select(x => x.Type.Metadata!));
    }

    private void VisitMethodSignature(MethodDeclaration node)
    {
        foreach (var parameter in node.Parameters)
            parameter.Accept(this);

        node.ReturnType.Accept(this);

        var type = ((TypeDeclaration)node.Parent!).Metadata!;
        node.Metadata = type.GetMethod(node.Name);
    }

    public void VisitTypeNode(Type node)
    {
        var typeProvider = symbolTableMap.Get(node).TypeProvider;
        var type = typeProvider.GetType(node.Name) ??
                   throw new SemanticAnalysisException($"Referenced unknown type '{node.Name}'");

        node.Metadata = type;
    }

    public void VisitUnaryExpression(UnaryExpression node)
    {
        node.Operand.Accept(this);

        // TODO: more complex logic
        if (node.Kind == UnaryExpressionKind.Unknown || node.Operand.ReturnTypeMetadata is null)
            throw new SemanticAnalysisException("Invalid unary expression: unknown kind or operand type");

        if (node.Kind == UnaryMinus &&
            (Equals(node.Operand.ReturnTypeMetadata, I8) ||
             Equals(node.Operand.ReturnTypeMetadata, I16) ||
             Equals(node.Operand.ReturnTypeMetadata, I32) ||
             Equals(node.Operand.ReturnTypeMetadata, I64) ||
             Equals(node.Operand.ReturnTypeMetadata, U8) ||
             Equals(node.Operand.ReturnTypeMetadata, U16) ||
             Equals(node.Operand.ReturnTypeMetadata, U32) ||
             Equals(node.Operand.ReturnTypeMetadata, U64) ||
             Equals(node.Operand.ReturnTypeMetadata, F32) ||
             Equals(node.Operand.ReturnTypeMetadata, F64)))
        {
            node.ReturnTypeMetadata = node.Operand.ReturnTypeMetadata;
        }
        else if (node.Kind == UnaryPlus &&
                 (Equals(node.Operand.ReturnTypeMetadata, I8) ||
                  Equals(node.Operand.ReturnTypeMetadata, I16) ||
                  Equals(node.Operand.ReturnTypeMetadata, I32) ||
                  Equals(node.Operand.ReturnTypeMetadata, I64) ||
                  Equals(node.Operand.ReturnTypeMetadata, U8) ||
                  Equals(node.Operand.ReturnTypeMetadata, U16) ||
                  Equals(node.Operand.ReturnTypeMetadata, U32) ||
                  Equals(node.Operand.ReturnTypeMetadata, U64) ||
                  Equals(node.Operand.ReturnTypeMetadata, F32) ||
                  Equals(node.Operand.ReturnTypeMetadata, F64)))
        {
            node.ReturnTypeMetadata = node.Operand.ReturnTypeMetadata;
        }
        else if (node.Kind == LogicalNot && Equals(node.Operand.ReturnTypeMetadata, Bool))
        {
            node.ReturnTypeMetadata = Bool;
        }
        else if (node.Kind == BitwiseNot &&
                 (Equals(node.Operand.ReturnTypeMetadata, I8) ||
                  Equals(node.Operand.ReturnTypeMetadata, I16) ||
                  Equals(node.Operand.ReturnTypeMetadata, I32) ||
                  Equals(node.Operand.ReturnTypeMetadata, I64) ||
                  Equals(node.Operand.ReturnTypeMetadata, U8) ||
                  Equals(node.Operand.ReturnTypeMetadata, U16) ||
                  Equals(node.Operand.ReturnTypeMetadata, U32) ||
                  Equals(node.Operand.ReturnTypeMetadata, U64)))
        {
            node.ReturnTypeMetadata = node.Operand.ReturnTypeMetadata;
        }
        else
        {
            throw new SemanticAnalysisException($"Invalid unary expression: incompatible operand type '{node.Operand.ReturnTypeMetadata}' for operator '{node.Kind}'");
        }
    }

    public void VisitVariable(VariableDeclaration node)
    {
        // TODO: infer type
        // TODO: unused variable
        node.Type.Accept(this);
        node.Expression.Accept(this);

        if (node.Expression.ReturnTypeMetadata is null)
            throw new SemanticAnalysisException($"Cannot determine type of expression in variable declaration '{node.Name}'");

        if (!node.Expression.ReturnTypeMetadata.Equals(node.Type.Metadata))
            throw new SemanticAnalysisException($"Type mismatch in variable declaration '{node.Name}': expected '{node.Type.Metadata}', got '{node.Expression.ReturnTypeMetadata}'");

        node.Metadata = new VariableMetadata(node.Name, node.Type.Metadata);
    }

    public void VisitWhile(While node)
    {
        node.Condition.Accept(this);
        node.Body.Accept(this);

        if (!Equals(node.Condition.ReturnTypeMetadata, Bool))
            throw new SemanticAnalysisException("Condition must be a boolean");
    }

    private static AccessModifierMetadata GetAccessModifierMetadata(AccessModifier accessModifier)
        => accessModifier switch
        {
            AccessModifier.Public => AccessModifierMetadata.Public,
            AccessModifier.Internal => AccessModifierMetadata.Internal,
            AccessModifier.Private => AccessModifierMetadata.Private,

            _ => throw new ArgumentOutOfRangeException(nameof(accessModifier), accessModifier, null)
        };

    public string Name => nameof(TypeChecker);

    public IEnumerable<string> DependsOn => [nameof(MetadataGenerator)];
}
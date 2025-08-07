using Trilang.Metadata;
using Trilang.Parsing;
using Trilang.Parsing.Ast;
using static Trilang.Parsing.Ast.BinaryExpressionKind;
using static Trilang.Parsing.Ast.UnaryExpressionKind;
using static Trilang.Metadata.TypeMetadata;

namespace Trilang.Semantics;

internal class TypeChecker : IVisitor<TypeCheckerContext>
{
    public void VisitArrayAccess(ArrayAccessExpressionNode node, TypeCheckerContext context)
    {
        node.Member.Accept(this, context);
        node.Index.Accept(this, context);

        if (node.Member.ReturnTypeMetadata is not TypeArrayMetadata typeArray)
            throw new SemanticAnalysisException("Array access must be of type array");

        if (!Equals(node.Index.ReturnTypeMetadata, I32))
            throw new SemanticAnalysisException("Array index must be of type i32");

        node.ReturnTypeMetadata = typeArray.ItemMetadata;
    }

    public void VisitArrayType(ArrayTypeNode node, TypeCheckerContext context)
    {
        node.ElementType.Accept(this, context);

        var typeProvider = node.SymbolTable!.TypeProvider;
        node.Metadata = typeProvider.GetType(node.Name) ??
                        throw new SemanticAnalysisException($"Unknown array type '{node.Name}'");
    }

    public void VisitAsExpression(AsExpressionNode node, TypeCheckerContext context)
    {
        node.Expression.Accept(this, context);
        node.Type.Accept(this, context);

        if (node.Expression.ReturnTypeMetadata is null || node.Type.Metadata is null)
            throw new SemanticAnalysisException("Cannot determine return type for expression");

        var typeProvider = node.SymbolTable!.TypeProvider;

        var metadata = new DiscriminatedUnionMetadata([node.Type.Metadata, Null]);
        typeProvider.DefineType(metadata.ToString(), metadata);
        node.ReturnTypeMetadata = metadata;
    }

    public void VisitBinaryExpression(BinaryExpressionNode node, TypeCheckerContext context)
    {
        node.Left.Accept(this, context);
        node.Right.Accept(this, context);

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
                 node.Left is MemberAccessExpressionNode &&
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
                 node.Left is MemberAccessExpressionNode &&
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

    public void VisitBlock(BlockStatementNode node, TypeCheckerContext context)
    {
        foreach (var statement in node.Statements)
            statement.Accept(this, context);
    }

    public void VisitBreak(BreakNode node, TypeCheckerContext context)
    {
    }

    public void VisitCall(CallExpressionNode node, TypeCheckerContext context)
    {
        // TODO: unused return value
        node.Member.Accept(this, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);

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

    public void VisitConstructor(ConstructorDeclarationNode node, TypeCheckerContext context)
    {
        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);

        var type = ((TypeDeclarationNode)node.Parent!).Metadata!;
        node.Metadata = type.GetConstructor(node.Parameters.Select(x => x.Type.Metadata!));

        foreach (var parameter in node.Parameters)
            parameter.Metadata = node.Metadata!.Parameters.FirstOrDefault(x => x.Name == parameter.Name);

        node.Body.Accept(this, context);
    }

    public void VisitContinue(ContinueNode node, TypeCheckerContext context)
    {
    }

    public void VisitDiscriminatedUnion(DiscriminatedUnionNode node, TypeCheckerContext context)
    {
        // TODO: eliminate duplicates
        foreach (var type in node.Types)
            type.Accept(this, context);

        var typeProvider = node.SymbolTable!.TypeProvider;
        var metadata = typeProvider.GetType(node.Name) ??
                       throw new SemanticAnalysisException($"Unknown discriminated union type '{node.Name}'");

        node.Metadata = metadata;
    }

    public void VisitExpressionBlock(ExpressionBlockNode node, TypeCheckerContext context)
        => throw new SemanticAnalysisException("Expression blocks are not supported");

    public void VisitExpressionStatement(ExpressionStatementNode node, TypeCheckerContext context)
    {
        // TODO: check whether the result of expression is used
        node.Expression.Accept(this, context);
    }

    public void VisitFunction(FunctionDeclarationNode node, TypeCheckerContext context)
    {
        var parameters = new ParameterMetadata[node.Parameters.Count];
        for (var i = 0; i < node.Parameters.Count; i++)
        {
            var parameter = node.Parameters[i];
            parameter.Accept(this, context);

            parameters[i] = new ParameterMetadata(parameter.Name, parameter.Type.Metadata!);
        }

        node.ReturnType.Accept(this, context);

        // we can be sure that node.Metadata is not null here
        // because it is set as a part of TypeNode
        var typeProvider = node.SymbolTable!.TypeProvider;
        var parameterTypes = node.Parameters.Select(x => x.Type.Metadata!);
        var returnType = node.ReturnType.Metadata!;
        var functionType = new FunctionTypeMetadata(parameterTypes, returnType);
        functionType = typeProvider.GetType(functionType.ToString()) as FunctionTypeMetadata ??
                       throw new SemanticAnalysisException($"Unknown function type '{functionType}'");

        node.Metadata = new FunctionMetadata(node.Name, parameters, functionType);

        foreach (var parameter in node.Parameters)
            parameter.Metadata = node.Metadata!.Parameters.FirstOrDefault(x => x.Name == parameter.Name);

        node.Body.Accept(this, context);
    }

    public void VisitFunctionType(FunctionTypeNode node, TypeCheckerContext context)
    {
        foreach (var parameterType in node.ParameterTypes)
            parameterType.Accept(this, context);

        node.ReturnType.Accept(this, context);

        var typeProvider = node.SymbolTable!.TypeProvider;
        var parameters = node.ParameterTypes.Select(x => x.Metadata!);
        var returnType = node.ReturnType.Metadata!;
        var functionType = new FunctionTypeMetadata(parameters, returnType);
        functionType = typeProvider.GetType(functionType.ToString()) as FunctionTypeMetadata ??
                       throw new SemanticAnalysisException($"Unknown function type '{functionType}'");

        node.Metadata = functionType;
    }

    public void VisitGenericType(GenericTypeNode node, TypeCheckerContext context)
    {
        foreach (var typeArgument in node.TypeArguments)
            typeArgument.Accept(this, context);

        var typeProvider = node.SymbolTable!.TypeProvider;
        var metadata = typeProvider.GetType(node.Name) ??
                       typeProvider.GetType(node.GetOpenGenericName()) ??
                       throw new SemanticAnalysisException($"Unknown generic type '{node.Name}'");

        node.Metadata = metadata;
    }

    public void VisitGoTo(GoToNode node, TypeCheckerContext context)
    {
    }

    public void VisitIfDirective(IfDirectiveNode node, TypeCheckerContext context)
    {
        if (context.HasDirective(node.DirectiveName))
            foreach (var then in node.Then)
                then.Accept(this, context);
        else
            foreach (var @else in node.Else)
                @else.Accept(this, context);
    }

    public void VisitIf(IfStatementNode node, TypeCheckerContext context)
    {
        // TODO: data flow
        node.Condition.Accept(this, context);
        node.Then.Accept(this, context);
        node.Else?.Accept(this, context);

        if (!Equals(node.Condition.ReturnTypeMetadata, Bool))
            throw new SemanticAnalysisException("The condition returns non-boolean type.");
    }

    public void VisitInterface(InterfaceNode node, TypeCheckerContext context)
    {
        var typeProvider = node.SymbolTable!.TypeProvider;
        var metadata = typeProvider.GetType(node.Name) as InterfaceMetadata ??
                       throw new SemanticAnalysisException($"Unknown interface type '{node.Name}'");

        node.Metadata = metadata;

        foreach (var property in node.Properties)
            property.Accept(this, context);

        foreach (var method in node.Methods)
            method.Accept(this, context);
    }

    public void VisitInterfaceProperty(InterfacePropertyNode node, TypeCheckerContext context)
    {
        node.Type.Accept(this, context);

        var type = (InterfaceMetadata)((InterfaceNode)node.Parent!).Metadata!;
        node.Metadata = type.GetProperty(node.Name);
    }

    public void VisitInterfaceMethod(InterfaceMethodNode node, TypeCheckerContext context)
    {
        foreach (var parameter in node.ParameterTypes)
            parameter.Accept(this, context);

        node.ReturnType.Accept(this, context);

        var type = (InterfaceMetadata)((InterfaceNode)node.Parent!).Metadata!;
        node.Metadata = type.GetMethod(node.Name);
    }

    public void VisitLabel(LabelNode node, TypeCheckerContext context)
    {
    }

    public void VisitLiteral(LiteralExpressionNode node, TypeCheckerContext context)
    {
        node.ReturnTypeMetadata = node.Kind switch
        {
            // TODO: other types
            LiteralExpressionKind.Number => I32,
            LiteralExpressionKind.Boolean => Bool,
            LiteralExpressionKind.String => TypeMetadata.String,
            LiteralExpressionKind.Char => TypeMetadata.Char,

            _ => throw new ArgumentOutOfRangeException(nameof(node.Kind), $"Unsupported literal expression kind: {node.Kind}"),
        };
    }

    public void VisitMemberAccess(MemberAccessExpressionNode node, TypeCheckerContext context)
    {
        if (node.IsFirstMember)
        {
            VisitFirstMemberAccess(node, context);
            return;
        }

        VisitNestedMemberAccess(node, context);

        if (node.ReturnTypeMetadata is null)
            throw new SemanticAnalysisException($"Cannot determine return type for member '{node.Name}'");
    }

    private static void VisitFirstMemberAccess(MemberAccessExpressionNode node, TypeCheckerContext context)
    {
        var symbol = node.SymbolTable!.GetId(node.Name);
        if (symbol is not null)
        {
            node.Reference = symbol.Node switch
            {
                PropertyDeclarationNode propertyDeclarationNode
                    => propertyDeclarationNode.Metadata,

                VariableDeclarationStatementNode variableStatementNode
                    => variableStatementNode.Metadata,

                ParameterNode parameterNode
                    => parameterNode.Metadata,

                FunctionDeclarationNode functionNode
                    => functionNode.Metadata,

                MethodDeclarationNode methodNode
                    => methodNode.Metadata,

                TypeDeclarationNode typeDeclarationNode
                    => node.IsThis
                        ? new ParameterMetadata(MemberAccessExpressionNode.This, typeDeclarationNode.Metadata!)
                        : typeDeclarationNode.Metadata,

                _ => throw new SemanticAnalysisException(),
            };

            return;
        }

        // static access
        var typeProvider = node.SymbolTable!.TypeProvider;
        var type = typeProvider.GetType(node.Name);
        if (type is not null)
        {
            node.Reference = type;

            return;
        }

        throw new SemanticAnalysisException($"Unknown member '{node.Name}'");
    }

    private void VisitNestedMemberAccess(MemberAccessExpressionNode node, TypeCheckerContext context)
    {
        node.Member!.Accept(this, context);

        var returnTypeMetadata = node.Member.ReturnTypeMetadata.Unpack();
        if (returnTypeMetadata is TypeMetadata type)
        {
            var property = type.GetProperty(node.Name);
            if (property is not null)
            {
                node.Reference = property;

                return;
            }

            var method = type.GetMethod(node.Name);
            if (method is not null)
            {
                node.Reference = method;

                return;
            }

            throw new SemanticAnalysisException($"Cannot find member '{node.Name}' in type '{node.Member.ReturnTypeMetadata}'");
        }

        if (returnTypeMetadata is InterfaceMetadata @interface)
        {
            var property = @interface.GetProperty(node.Name);
            if (property is not null)
            {
                node.Reference = property;

                return;
            }

            var method = @interface.GetMethod(node.Name);
            if (method is not null)
            {
                node.Reference = method;

                return;
            }

            throw new SemanticAnalysisException($"Cannot find member '{node.Name}' in interface '{node.Member.ReturnTypeMetadata}'");
        }
    }

    public void VisitMethod(MethodDeclarationNode node, TypeCheckerContext context)
    {
        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);

        node.ReturnType.Accept(this, context);

        var type = ((TypeDeclarationNode)node.Parent!).Metadata!;
        node.Metadata = type.GetMethod(node.Name);

        foreach (var parameter in node.Parameters)
            parameter.Metadata = node.Metadata!.Parameters.FirstOrDefault(x => x.Name == parameter.Name);

        node.Body.Accept(this, context);
    }

    public void VisitNewArray(NewArrayExpressionNode node, TypeCheckerContext context)
    {
        node.Type.Accept(this, context);
        node.Size.Accept(this, context);

        node.ReturnTypeMetadata = node.Type.Metadata;
    }

    public void VisitNewObject(NewObjectExpressionNode node, TypeCheckerContext context)
    {
        node.Type.Accept(this, context);

        foreach (var parameter in node.Parameters)
            parameter.Accept(this, context);

        if (node.Type.Metadata is not TypeMetadata type)
            throw new SemanticAnalysisException($"Cannot create an instance of type '{node.Type.Metadata}'");

        var parameters = node.Parameters.Select(x => x.ReturnTypeMetadata!).ToList();
        var ctor = type.GetConstructor(parameters) ??
                   throw new SemanticAnalysisException($"The '{type.Name}' type doesn't have '{string.Join(", ", parameters)}' constructor.");

        node.Metadata = ctor;
    }

    public void VisitNull(NullExpressionNode node, TypeCheckerContext context)
    {
    }

    public void VisitReturn(ReturnStatementNode node, TypeCheckerContext context)
    {
        node.Expression?.Accept(this, context);

        var expressionType = node.Expression?.ReturnTypeMetadata ?? TypeMetadata.Void;
        var method = node.FindInParent<MethodDeclarationNode>();
        if (method is not null)
        {
            var methodReturnType = method.Metadata?.TypeMetadata.ReturnType;
            if (!Equals(methodReturnType, expressionType))
                throw new SemanticAnalysisException($"Method return type mismatch: expected '{methodReturnType}', got '{expressionType}'");

            return;
        }

        var constructor = node.FindInParent<ConstructorDeclarationNode>();
        if (constructor is not null)
        {
            if (!Equals(TypeMetadata.Void, expressionType))
                throw new SemanticAnalysisException($"Constructor return type mismatch: expected '{TypeMetadata.Void.Name}', got '{expressionType}'");

            return;
        }

        var function = node.FindInParent<FunctionDeclarationNode>();
        if (function is not null)
        {
            var functionReturnType = function.Metadata?.TypeMetadata.ReturnType;
            if (!Equals(functionReturnType, expressionType))
                throw new SemanticAnalysisException($"Function return type mismatch: expected '{functionReturnType}', got '{expressionType}'");

            return;
        }

        var getter = node.FindInParent<PropertyGetterNode>();
        if (getter is not null)
        {
            var getterReturnType = ((PropertyDeclarationNode)getter.Parent!).Metadata!.Type;
            if (!Equals(getterReturnType, expressionType))
                throw new SemanticAnalysisException($"Property getter return type mismatch: expected '{getterReturnType}', got '{expressionType}'");

            return;
        }

        var setter = node.FindInParent<PropertySetterNode>();
        if (setter is not null)
        {
            var setterReturnType = ((PropertyDeclarationNode)setter.Parent!).Metadata!.Type;
            if (!Equals(setterReturnType, expressionType))
                throw new SemanticAnalysisException($"Property setter return type mismatch: expected '{setterReturnType}', got '{expressionType}'");

            return;
        }
    }

    public void VisitParameter(ParameterNode node, TypeCheckerContext context)
        => node.Type.Accept(this, context);

    public void VisitProperty(PropertyDeclarationNode node, TypeCheckerContext context)
    {
        var type = ((TypeDeclarationNode)node.Parent!).Metadata!;
        node.Metadata = type.GetProperty(node.Name);

        node.Type.Accept(this, context);
        node.Getter?.Accept(this, context);
        node.Setter?.Accept(this, context);
    }

    public void VisitGetter(PropertyGetterNode node, TypeCheckerContext context)
    {
        var property = (PropertyDeclarationNode)node.Parent!;
        var propertyMetadata = property.Metadata!;
        node.Metadata = propertyMetadata.Getter;

        node.Body?.Accept(this, context);
    }

    public void VisitSetter(PropertySetterNode node, TypeCheckerContext context)
    {
        var property = (PropertyDeclarationNode)node.Parent!;
        var propertyMetadata = property.Metadata!;
        node.Metadata = propertyMetadata.Setter;

        // TODO: check the backing field is set?
        node.Body?.Accept(this, context);
    }

    public void VisitTree(SyntaxTree node, TypeCheckerContext context)
    {
        foreach (var statement in node.Declarations)
            statement.Accept(this, context);
    }

    public void VisitTuple(TupleExpressionNode node, TypeCheckerContext context)
    {
        foreach (var expression in node.Expressions)
            expression.Accept(this, context);

        var typeProvider = node.SymbolTable!.TypeProvider;

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

    public void VisitTupleType(TupleTypeNode node, TypeCheckerContext context)
    {
        foreach (var type in node.Types)
            type.Accept(this, context);

        var typeProvider = node.SymbolTable!.TypeProvider;
        var types = node.Types.Select(x => x.Metadata!);
        var tuple = new TupleMetadata(types);
        tuple = typeProvider.GetType(tuple.ToString()) as TupleMetadata ??
                throw new SemanticAnalysisException($"Unknown tuple type '{tuple}'");

        node.Metadata = tuple;
    }

    public void VisitTypeAlias(TypeAliasDeclarationNode node, TypeCheckerContext context)
    {
        node.Type.Accept(this, context);

        var typeProvider = node.SymbolTable!.TypeProvider;
        node.Metadata = typeProvider.GetType(node.FullName) ??
                        throw new SemanticAnalysisException($"Unknown type '{node.Name}'");
    }

    public void VisitType(TypeDeclarationNode node, TypeCheckerContext context)
    {
        var typeProvider = node.SymbolTable!.TypeProvider;
        var metadata = typeProvider.GetType(node.FullName);
        if (metadata is not TypeMetadata type)
            throw new SemanticAnalysisException($"The '{node.Name}' type is not a type.");

        node.Metadata = type;

        foreach (var @interface in node.Interfaces)
            @interface.Accept(this, context);

        foreach (var property in node.Properties)
            property.Accept(this, context);

        foreach (var constructor in node.Constructors)
            constructor.Accept(this, context);

        foreach (var method in node.Methods)
            method.Accept(this, context);
    }

    public void VisitTypeNode(TypeNode node, TypeCheckerContext context)
    {
        var typeProvider = node.SymbolTable!.TypeProvider;
        var type = typeProvider.GetType(node.Name) ??
                   throw new SemanticAnalysisException($"Referenced unknown type '{node.Name}'");

        node.Metadata = type;
    }

    public void VisitUnaryExpression(UnaryExpressionNode node, TypeCheckerContext context)
    {
        node.Operand.Accept(this, context);

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

    public void VisitVariable(VariableDeclarationStatementNode node, TypeCheckerContext context)
    {
        // TODO: infer type
        // TODO: unused variable
        node.Type.Accept(this, context);
        node.Expression.Accept(this, context);

        if (node.Expression.ReturnTypeMetadata is null)
            throw new SemanticAnalysisException($"Cannot determine type of expression in variable declaration '{node.Name}'");

        if (!node.Expression.ReturnTypeMetadata.Equals(node.Type.Metadata))
            throw new SemanticAnalysisException($"Type mismatch in variable declaration '{node.Name}': expected '{node.Type.Metadata}', got '{node.Expression.ReturnTypeMetadata}'");

        node.Metadata = new VariableMetadata(node.Name, node.Type.Metadata);
    }

    public void VisitWhile(WhileNode node, TypeCheckerContext context)
    {
        node.Condition.Accept(this, context);
        node.Body.Accept(this, context);

        if (!Equals(node.Condition.ReturnTypeMetadata, Bool))
            throw new SemanticAnalysisException("Condition must be a boolean");
    }
}
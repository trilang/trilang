using Trilang.Metadata;
using Trilang.Semantics.Model;
using Type = Trilang.Semantics.Model.Type;

namespace Trilang.Compilation.Diagnostics;

public class SemanticDiagnosticReporter
{
    private readonly DiagnosticCollection diagnostics;

    public SemanticDiagnosticReporter(DiagnosticCollection diagnostics)
        => this.diagnostics = diagnostics;

    public void CyclicTypeAlias(TypeAliasMetadata alias)
        => diagnostics.Error(
            DiagnosticId.S0001CyclicTypeAlias,
            alias.Definition ?? new SourceLocation(default, default),
            $"The cyclic type alias detected: '{alias.Name}'.");

    public void FunctionAlreadyDefined(FunctionDeclaration function)
        => diagnostics.Error(
            DiagnosticId.S0002FunctionAlreadyDefined,
            function.GetLocation(),
            $"The '{function.Name}' function is already defined.");

    public void InterfacePropertyAlreadyDefined(InterfaceProperty property)
        => diagnostics.Error(
            DiagnosticId.S0003PropertyAlreadyDefined,
            property.GetLocation(),
            $"The '{property.Name}' property is already defined.");

    public void InterfaceMethodAlreadyDefined(InterfaceMethod method)
        => diagnostics.Error(
            DiagnosticId.S0004MethodAlreadyDefined,
            method.GetLocation(),
            $"The '{method.Name}' method is already defined.");

    public void PropertyAlreadyDefined(PropertyDeclaration property)
        => diagnostics.Error(
            DiagnosticId.S0003PropertyAlreadyDefined,
            property.GetLocation(),
            $"The '{property.Name}' property is already defined.");

    public void MethodAlreadyDefined(MethodDeclaration method)
        => diagnostics.Error(
            DiagnosticId.S0004MethodAlreadyDefined,
            method.GetLocation(),
            $"The '{method.Name}' method is already defined.");

    public void ParameterAlreadyDefined(Parameter parameter)
        => diagnostics.Error(
            DiagnosticId.S0005ParameterAlreadyDefined,
            parameter.GetLocation(),
            $"The '{parameter.Name}' parameter is already defined.");

    public void ParameterAlreadyDefined(ISemanticNode parameter, string name)
        => diagnostics.Error(
            DiagnosticId.S0005ParameterAlreadyDefined,
            parameter.GetLocation(),
            $"The '{name}' parameter is already defined.");

    public void TypeAlreadyDefined(TypeDeclaration type)
        => diagnostics.Error(
            DiagnosticId.S0006TypeAlreadyDefined,
            type.GetLocation(),
            $"The '{type.Name}' type is already defined.");

    public void TypeAlreadyDefined(TypeAliasDeclaration type)
        => diagnostics.Error(
            DiagnosticId.S0006TypeAlreadyDefined,
            type.GetLocation(),
            $"The '{type.Name}' type is already defined.");

    public void VariableAlreadyDefined(VariableDeclaration variable)
        => diagnostics.Error(
            DiagnosticId.S0007VariableAlreadyDefined,
            variable.GetLocation(),
            $"The '{variable.Name}' variable is already defined.");

    public void UnknownType(IInlineType type)
        => diagnostics.Error(
            DiagnosticId.S0008UnknownType,
            type.GetLocation(),
            $"Unknown type: '{type.Name}'.");

    public void ReturnTypeMismatch(
        ReturnStatement returnType,
        ITypeMetadata expected,
        ITypeMetadata actual)
        => diagnostics.Error(
            DiagnosticId.S0009ReturnTypeMismatch,
            returnType.GetLocation(),
            $"Return type mismatch: expected '{expected}', got '{actual}'.");

    public void TypeArgumentAlreadyDefined(Type genericArgument)
        => diagnostics.Error(
            DiagnosticId.S0006TypeAlreadyDefined,
            genericArgument.GetLocation(),
            $"The '{genericArgument.Name}' type argument is already defined.");

    public void TypeMismatch(IExpression exp, ITypeMetadata expected, ITypeMetadata? actual)
        => diagnostics.Error(
            DiagnosticId.S0010TypeMismatch,
            exp.GetLocation(),
            $"Type mismatch: expected '{expected}', got '{actual}'.");

    public void ExpectedArray(IExpression array)
        => diagnostics.Error(
            DiagnosticId.S0011ExpectedArray,
            array.GetLocation(),
            $"Expected an array, got '{array.ReturnTypeMetadata}'.");

    public void ExpectedFunction(IExpression function)
        => diagnostics.Error(
            DiagnosticId.S0012ExpectedFunction,
            function.GetLocation(),
            $"Expected a function, got '{function.ReturnTypeMetadata}'.");

    public void UnknownMember(MemberAccessExpression member, ITypeMetadata type)
        => diagnostics.Error(
            DiagnosticId.S0013UnknownMember,
            member.GetLocation(),
            $"The '{type}' type doesn't have '{member.Name}'.");

    public void UnknownConstructor(NewObjectExpression node, IEnumerable<ITypeMetadata> parameters)
        => diagnostics.Error(
            DiagnosticId.S0013UnknownMember,
            node.GetLocation(),
            $"The '{node.Type.Metadata}' type doesn't have '{string.Join(", ", parameters)}' constructor.");

    public void UnknownGetter(MemberAccessExpression node, PropertyMetadata property)
        => diagnostics.Error(
            DiagnosticId.S0013UnknownMember,
            node.GetLocation(),
            $"The '{property.Name}' property doesn't have a getter.");

    public void UnknownSetter(MemberAccessExpression node, PropertyMetadata property)
        => diagnostics.Error(
            DiagnosticId.S0013UnknownMember,
            node.GetLocation(),
            $"The '{property.Name}' property doesn't have a setter.");

    public void UnknownSymbol(MemberAccessExpression node)
        => diagnostics.Error(
            DiagnosticId.S0013UnknownMember,
            node.GetLocation(),
            $"Unknown symbol: '{node.Name}'.");

    public void CantCreateObject(NewObjectExpression node)
        => diagnostics.Error(
            DiagnosticId.S0014CantCreateObject,
            node.GetLocation(),
            $"Cannot create an instance of type '{node.Type.Metadata}'");

    public void IncompatibleUnaryOperand(UnaryExpression node)
        => diagnostics.Error(
            DiagnosticId.S0015IncompatibleUnaryOperator,
            node.GetLocation(),
            $"Incompatible operand type '{node.Operand.ReturnTypeMetadata}' for operator '{node.Kind}'.");

    public void IncompatibleBinaryOperand(BinaryExpression node)
        => diagnostics.Error(
            DiagnosticId.S0016IncompatibleBinaryOperator,
            node.GetLocation(),
            $"Incompatible operand types '{node.Left.ReturnTypeMetadata}' and '{node.Right.ReturnTypeMetadata}' for operator '{node.Kind}'.");

    public void BreakOutsideLoop(Break node)
        => diagnostics.Error(
            DiagnosticId.S0017BreakOutsideLoop,
            node.GetLocation(),
            "The 'break' keyword can only be used within a loop.");

    public void ContinueOutsideLoop(Continue node)
        => diagnostics.Error(
            DiagnosticId.S0018ContinueOutsideLoop,
            node.GetLocation(),
            "The 'continue' keyword can only be used within a loop.");

    public void ConstructorNotAccessible(NewObjectExpression node, ITypeMetadata type)
        => diagnostics.Error(
            DiagnosticId.S0019MemberNotAccessible,
            node.GetLocation(),
            $"The constructor of '{type}' is not accessible.");

    public void GetterNotAccessible(MemberAccessExpression node, PropertyMetadata property)
        => diagnostics.Error(
            DiagnosticId.S0019MemberNotAccessible,
            node.GetLocation(),
            $"The getter of '{property.Name}' is not accessible.");

    public void SetterNotAccessible(MemberAccessExpression node, PropertyMetadata property)
        => diagnostics.Error(
            DiagnosticId.S0019MemberNotAccessible,
            node.GetLocation(),
            $"The setter of '{property.Name}' is not accessible.");

    public void FieldNotAccessible(MemberAccessExpression node)
        => diagnostics.Error(
            DiagnosticId.S0019MemberNotAccessible,
            node.GetLocation(),
            $"The '{node.Name}' field is not accessible.");

    // TODO: highlight not a function name
    public void NotAllPathsReturnValue(IFunctionMetadata function)
        => diagnostics.Error(
            DiagnosticId.S0020NotAllPathsReturnValue,
            function.Definition ?? new SourceLocation(default, default),
            $"Not all paths return a value in '{function.Name}'.");

    public void ThisInStaticMethod(MemberAccessExpression node)
        => diagnostics.Error(
            DiagnosticId.S0021ThisInStaticMethod,
            node.GetLocation(),
            "The 'this' keyword cannot be used in a static method.");

    public void ThisOutsideOfType(MemberAccessExpression node)
        => diagnostics.Error(
            DiagnosticId.S0022ThisOutsideOfType,
            node.GetLocation(),
            "The 'this' keyword can only be used within a type.");

    public void StaticMethodAsInstance(MemberAccessExpression node)
        => diagnostics.Error(
            DiagnosticId.S0023StaticMethodAsInstance,
            node.GetLocation(),
            $"The static method '{node.Name}' cannot be called as an instance one.");

    public void InstanceMethodAsStatic(MemberAccessExpression node)
        => diagnostics.Error(
            DiagnosticId.S0024InstanceMethodAsStatic,
            node.GetLocation(),
            $"The instance method '{node.Name}' cannot be called as a static one.");

    public void VariableUsedBeforeDeclaration(MemberAccessExpression node)
        => diagnostics.Error(
            DiagnosticId.S0025VariableUsedBeforeDeclaration,
            node.GetLocation(),
            $"The '{node.Name}' variable is used before its declaration.");

    public void PropertyIsNotImplemented(TypeDeclaration node, InterfacePropertyMetadata interfaceProperty)
        => diagnostics.Error(
            DiagnosticId.S0026MemberIsNotImplemented,
            node.GetLocation(),
            $"The '{interfaceProperty.Name}' property is not implemented.");

    public void MethodIsNotImplemented(TypeDeclaration node, InterfaceMethodMetadata interfaceMethod)
        => diagnostics.Error(
            DiagnosticId.S0026MemberIsNotImplemented,
            node.GetLocation(),
            $"The '{interfaceMethod.Name}' method is not implemented.");

    public void MethodImplementationHasIncorrectType(MethodMetadata method, InterfaceMethodMetadata interfaceMethod)
        => diagnostics.Error(
            DiagnosticId.S0026MemberIsNotImplemented,
            method.Definition ?? new SourceLocation(default, default),
            $"The '{interfaceMethod.Name}' method is not of the correct type. Expected '{interfaceMethod.Type}', got '{method.Type}'.");

    public void MethodImplementationIsNotPublic(MethodMetadata method)
        => diagnostics.Error(
            DiagnosticId.S0026MemberIsNotImplemented,
            method.Definition ?? new SourceLocation(default, default),
            $"The implementation of the interface method '{method.Name}' is not public.");

    public void PropertyImplementationHasIncorrectType(PropertyMetadata property, InterfacePropertyMetadata interfaceProperty)
        => diagnostics.Error(
            DiagnosticId.S0026MemberIsNotImplemented,
            property.Definition ?? new SourceLocation(default, default),
            $"The '{interfaceProperty.Name}' property is not of the correct type. Expected '{property.Type}', got '{interfaceProperty.Type}'.");

    public void PropertyGetterIncorrectAccessModifier(PropertyMetadata property)
        => diagnostics.Error(
            DiagnosticId.S0026MemberIsNotImplemented,
            property.Definition ?? new SourceLocation(default, default),
            $"The implementation of an interface property getter '{property.Name}' cannot be private.");

    public void PropertySetterIncorrectAccessModifier(PropertyMetadata property)
        => diagnostics.Error(
            DiagnosticId.S0026MemberIsNotImplemented,
            property.Definition ?? new SourceLocation(default, default),
            $"The implementation of an interface property setter '{property.Name}' cannot be private.");

    public void InterfaceGetterCantBePrivate(InterfacePropertyMetadata property)
        => diagnostics.Error(
            DiagnosticId.S0027InterfacePropertyCantBePrivate,
            property.Definition ?? new SourceLocation(default, default),
            $"The getter of the interface property '{property.Name}' cannot be private.");

    public void InterfaceSetterCantBePrivate(InterfacePropertyMetadata property)
        => diagnostics.Error(
            DiagnosticId.S0027InterfacePropertyCantBePrivate,
            property.Definition ?? new SourceLocation(default, default),
            $"The setter of the interface property '{property.Name}' cannot be private.");
}
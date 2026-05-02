using Trilang.Metadata;
using Trilang.Semantics.Model;

namespace Trilang.Compilation.Diagnostics;

public readonly record struct SemanticDiagnosticReporter(DiagnosticCollection Diagnostics)
{
    public void CyclicTypeAlias(AliasMetadata alias)
        => Diagnostics.Error(
            DiagnosticId.S0001CyclicTypeAlias,
            alias.Definition!,
            $"The cyclic type alias detected: '{alias.Name}'.");

    public void FunctionAlreadyDefined(FunctionDeclaration function)
        => Diagnostics.Error(
            DiagnosticId.S0002AlreadyDefined,
            function.GetLocation(),
            $"The '{function.Name}' function is already defined.");

    public void InterfacePropertyAlreadyDefined(InterfaceProperty property)
        => Diagnostics.Error(
            DiagnosticId.S0002AlreadyDefined,
            property.GetLocation(),
            $"The '{property.Name}' property is already defined.");

    public void InterfaceMethodAlreadyDefined(InterfaceMethod method)
        => Diagnostics.Error(
            DiagnosticId.S0002AlreadyDefined,
            method.GetLocation(),
            $"The '{method.Name}' method is already defined.");

    public void PropertyAlreadyDefined(PropertyDeclaration property)
        => Diagnostics.Error(
            DiagnosticId.S0002AlreadyDefined,
            property.GetLocation(),
            $"The '{property.Name}' property is already defined.");

    public void MethodAlreadyDefined(MethodDeclaration method)
        => Diagnostics.Error(
            DiagnosticId.S0002AlreadyDefined,
            method.GetLocation(),
            $"The '{method.Name}' method is already defined.");

    public void ParameterAlreadyDefined(Parameter parameter)
        => Diagnostics.Error(
            DiagnosticId.S0002AlreadyDefined,
            parameter.GetLocation(),
            $"The '{parameter.Name}' parameter is already defined.");

    public void TypeAlreadyDefined(TypeDeclaration type)
        => Diagnostics.Error(
            DiagnosticId.S0002AlreadyDefined,
            type.GetLocation(),
            $"The '{type.Name}' type is already defined.");

    public void TypeAlreadyDefined(AliasDeclaration type)
        => Diagnostics.Error(
            DiagnosticId.S0002AlreadyDefined,
            type.GetLocation(),
            $"The '{type.Name}' type is already defined.");

    public void VariableAlreadyDefined(VariableDeclaration variable)
        => Diagnostics.Error(
            DiagnosticId.S0002AlreadyDefined,
            variable.GetLocation(),
            $"The '{variable.Name}' variable is already defined.");

    public void UnknownType(IInlineType type)
        => Diagnostics.Error(
            DiagnosticId.S0003UnknownType,
            type.GetLocation(),
            $"Unknown type: '{type.Name}'.");

    public void ReturnTypeMismatch(
        ReturnStatement returnType,
        ITypeMetadata expected,
        ITypeMetadata actual)
        => Diagnostics.Error(
            DiagnosticId.S0004ReturnTypeMismatch,
            returnType.GetLocation(),
            $"Return type mismatch: expected '{expected}', got '{actual}'.");

    public void TypeArgumentAlreadyDefined(TypeRef genericArgument)
        => Diagnostics.Error(
            DiagnosticId.S0002AlreadyDefined,
            genericArgument.GetLocation(),
            $"The '{genericArgument.Name}' type argument is already defined.");

    public void TypeMismatch(IExpression exp, ITypeMetadata expected, ITypeMetadata? actual)
        => Diagnostics.Error(
            DiagnosticId.S0005TypeMismatch,
            exp.GetLocation(),
            $"Type mismatch: expected '{expected}', got '{actual}'.");

    public void ExpectedArray(IExpression array)
        => Diagnostics.Error(
            DiagnosticId.S0006ExpectedArray,
            array.GetLocation(),
            $"Expected an array, got '{array.ReturnTypeMetadata}'.");

    public void ExpectedFunction(IExpression function)
        => Diagnostics.Error(
            DiagnosticId.S0007ExpectedFunction,
            function.GetLocation(),
            $"Expected a function, got '{function.ReturnTypeMetadata}'.");

    public void UnknownMember(MemberAccessExpression member, ITypeMetadata type)
        => Diagnostics.Error(
            DiagnosticId.S0008UnknownMember,
            member.GetLocation(),
            $"The '{type}' type doesn't have '{member.Name}'.");

    public void UnknownConstructor(NewObjectExpression node, IEnumerable<ITypeMetadata> parameters)
        => Diagnostics.Error(
            DiagnosticId.S0008UnknownMember,
            node.GetLocation(),
            $"The '{node.Type.Metadata}' type doesn't have '{string.Join(", ", parameters)}' constructor.");

    public void UnknownGetter(MemberAccessExpression node, PropertyMetadata property)
        => Diagnostics.Error(
            DiagnosticId.S0008UnknownMember,
            node.GetLocation(),
            $"The '{property.Name}' property doesn't have a getter.");

    public void UnknownSetter(MemberAccessExpression node, PropertyMetadata property)
        => Diagnostics.Error(
            DiagnosticId.S0008UnknownMember,
            node.GetLocation(),
            $"The '{property.Name}' property doesn't have a setter.");

    public void UnknownSymbol(MemberAccessExpression node)
        => Diagnostics.Error(
            DiagnosticId.S0008UnknownMember,
            node.GetLocation(),
            $"Unknown symbol: '{node.Name}'.");

    public void CantCreateObject(NewObjectExpression node)
        => Diagnostics.Error(
            DiagnosticId.S0009CantCreateObject,
            node.GetLocation(),
            $"Cannot create an instance of type '{node.Type.Metadata}'");

    public void IncompatibleUnaryOperand(UnaryExpression node)
        => Diagnostics.Error(
            DiagnosticId.S0010IncompatibleUnaryOperator,
            node.GetLocation(),
            $"Incompatible operand type '{node.Operand.ReturnTypeMetadata}' for operator '{node.Kind}'.");

    public void IncompatibleBinaryOperand(BinaryExpression node)
        => Diagnostics.Error(
            DiagnosticId.S0011IncompatibleBinaryOperator,
            node.GetLocation(),
            $"Incompatible operand types '{node.Left.ReturnTypeMetadata}' and '{node.Right.ReturnTypeMetadata}' for operator '{node.Kind}'.");

    public void BreakOutsideLoop(Break node)
        => Diagnostics.Error(
            DiagnosticId.S0012BreakOutsideLoop,
            node.GetLocation(),
            "The 'break' keyword can only be used within a loop.");

    public void ContinueOutsideLoop(Continue node)
        => Diagnostics.Error(
            DiagnosticId.S0013ContinueOutsideLoop,
            node.GetLocation(),
            "The 'continue' keyword can only be used within a loop.");

    public void ConstructorNotAccessible(NewObjectExpression node, ITypeMetadata type)
        => Diagnostics.Error(
            DiagnosticId.S0014MemberNotAccessible,
            node.GetLocation(),
            $"The constructor of '{type}' is not accessible.");

    public void GetterNotAccessible(MemberAccessExpression node, PropertyMetadata property)
        => Diagnostics.Error(
            DiagnosticId.S0014MemberNotAccessible,
            node.GetLocation(),
            $"The getter of '{property.Name}' is not accessible.");

    public void SetterNotAccessible(MemberAccessExpression node, PropertyMetadata property)
        => Diagnostics.Error(
            DiagnosticId.S0014MemberNotAccessible,
            node.GetLocation(),
            $"The setter of '{property.Name}' is not accessible.");

    public void FieldNotAccessible(MemberAccessExpression node)
        => Diagnostics.Error(
            DiagnosticId.S0014MemberNotAccessible,
            node.GetLocation(),
            $"The '{node.Name}' field is not accessible.");

    public void MethodNotAccessible(MemberAccessExpression node, MethodMetadata method)
        => Diagnostics.Error(
            DiagnosticId.S0014MemberNotAccessible,
            node.GetLocation(),
            $"The '{method.Name}' method is not accessible.");

    public void FunctionNotAccessible(MemberAccessExpression node, FunctionMetadata function)
        => Diagnostics.Error(
            DiagnosticId.S0014MemberNotAccessible,
            node.GetLocation(),
            $"The '{function.Name}' function is not accessible.");

    // TODO: highlight a function name
    public void NotAllPathsReturnValue(IMetadata function)
        => Diagnostics.Error(
            DiagnosticId.S0015NotAllPathsReturnValue,
            function.Definition!,
            $"Not all paths return a value.");

    public void ThisInStaticMethod(MemberAccessExpression node)
        => Diagnostics.Error(
            DiagnosticId.S0016ThisInStaticMethod,
            node.GetLocation(),
            "The 'this' keyword cannot be used in a static method.");

    public void ThisOutsideOfType(MemberAccessExpression node)
        => Diagnostics.Error(
            DiagnosticId.S0017ThisOutsideOfType,
            node.GetLocation(),
            "The 'this' keyword can only be used within a type.");

    public void StaticMethodAsInstance(MemberAccessExpression node)
        => Diagnostics.Error(
            DiagnosticId.S0018StaticMethodAsInstance,
            node.GetLocation(),
            $"The static method '{node.Name}' cannot be called as an instance one.");

    public void InstanceMethodAsStatic(MemberAccessExpression node)
        => Diagnostics.Error(
            DiagnosticId.S0019InstanceMethodAsStatic,
            node.GetLocation(),
            $"The instance method '{node.Name}' cannot be called as a static one.");

    public void VariableUsedBeforeDeclaration(MemberAccessExpression node)
        => Diagnostics.Error(
            DiagnosticId.S0020VariableUsedBeforeDeclaration,
            node.GetLocation(),
            $"The '{node.Name}' variable is used before its declaration.");

    public void PropertyIsNotImplemented(TypeDeclaration node, InterfacePropertyMetadata interfaceProperty)
        => Diagnostics.Error(
            DiagnosticId.S0021MemberIsNotImplemented,
            node.GetLocation(),
            $"The '{interfaceProperty.Name}' property is not implemented.");

    public void MethodIsNotImplemented(TypeDeclaration node, InterfaceMethodMetadata interfaceMethod)
        => Diagnostics.Error(
            DiagnosticId.S0021MemberIsNotImplemented,
            node.GetLocation(),
            $"The '{interfaceMethod.Name}' method is not implemented.");

    public void MethodImplementationHasIncorrectType(MethodMetadata method, InterfaceMethodMetadata interfaceMethod)
        => Diagnostics.Error(
            DiagnosticId.S0021MemberIsNotImplemented,
            method.Definition!,
            $"The '{interfaceMethod.Name}' method is not of the correct type. Expected '{interfaceMethod.Type}', got '{method.Type}'.");

    public void MethodImplementationIsNotPublic(MethodMetadata method)
        => Diagnostics.Error(
            DiagnosticId.S0021MemberIsNotImplemented,
            method.Definition!,
            $"The implementation of the interface method '{method.Name}' is not public.");

    public void PropertyImplementationHasIncorrectType(PropertyMetadata property, InterfacePropertyMetadata interfaceProperty)
        => Diagnostics.Error(
            DiagnosticId.S0021MemberIsNotImplemented,
            property.Definition!,
            $"The '{interfaceProperty.Name}' property is not of the correct type. Expected '{property.Type}', got '{interfaceProperty.Type}'.");

    public void PropertyGetterIncorrectAccessModifier(PropertyMetadata property)
        => Diagnostics.Error(
            DiagnosticId.S0021MemberIsNotImplemented,
            property.Definition!,
            $"The implementation of an interface property getter '{property.Name}' cannot be private.");

    public void PropertySetterIncorrectAccessModifier(PropertyMetadata property)
        => Diagnostics.Error(
            DiagnosticId.S0021MemberIsNotImplemented,
            property.Definition!,
            $"The implementation of an interface property setter '{property.Name}' cannot be private.");

    public void InterfaceGetterCantBePrivate(InterfacePropertyMetadata property)
        => Diagnostics.Error(
            DiagnosticId.S0022InterfacePropertyCantBePrivate,
            property.Definition!,
            $"The getter of the interface property '{property.Name}' cannot be private.");

    public void InterfaceSetterCantBePrivate(InterfacePropertyMetadata property)
        => Diagnostics.Error(
            DiagnosticId.S0022InterfacePropertyCantBePrivate,
            property.Definition!,
            $"The setter of the interface property '{property.Name}' cannot be private.");

    public void NoSuitableOverload(MemberAccessExpression node)
        => Diagnostics.Error(
            DiagnosticId.S0023NoSuitableOverload,
            node.GetLocation(),
            $"No suitable overload found for '{node.Name}'.");

    public void MultipleOverloads(MemberAccessExpression node)
        => Diagnostics.Error(
            DiagnosticId.S0024MultipleOverloads,
            node.GetLocation(),
            $"Multiple overloads found for '{node.Name}'.");

    public void ExtraArgument(IExpression node)
        => Diagnostics.Error(
            DiagnosticId.S0025ExtraArgument,
            node.GetLocation(),
            $"Extra argument: '{node}'.");

    public void MissingArgument(CallExpression node, ITypeMetadata type)
        => Diagnostics.Error(
            DiagnosticId.S0026MissingArgument,
            node.GetLocation(),
            $"Missing argument: '{type}'.");

    public void MultipleMembersFound(ISemanticNode node, IEnumerable<IMetadata> types)
        => Diagnostics.Error(
            DiagnosticId.S0027MultipleMembersFound,
            node.GetLocation(),
            $"Multiple members found:\n{string.Join("\n", types.Select(x => $"{x}: {x.Definition}"))}.");

    public void UnknownNamespace(Use node)
        => Diagnostics.Error(
            DiagnosticId.S0028UnknownNamespace,
            node.GetLocation(),
            $"Unknown namespace: {string.Join(".", node.Parts)}.");

    public void UnknownNamespace(IInlineType inlineType)
    {
        if (inlineType is TypeRef typeRef)
            Diagnostics.Error(
                DiagnosticId.S0028UnknownNamespace,
                typeRef.GetLocation(),
                $"Namespace '{string.Join(".", typeRef.Parts)}' not found.");
        else if (inlineType is GenericApplication genericApplication)
            Diagnostics.Error(
                DiagnosticId.S0028UnknownNamespace,
                genericApplication.GetLocation(),
                $"Namespace '{string.Join(".", genericApplication.Type.Parts)}' not found.");
        else
            throw new ArgumentException($"Unknown inline type: {inlineType.GetType()}.");
    }

    public void UnknownPackage(Use node)
        => Diagnostics.Error(
            DiagnosticId.S0029UnknownPackage,
            node.GetLocation(),
            $"Unknown package: {node.Package}.");
}
using Trilang.Metadata;
using Trilang.Semantics.Model;

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
            new SourceLocation(function.GetRoot().SourceFile, function.SourceSpan ?? default),
            $"The '{function.Name}' function is already defined.");

    public void InterfacePropertyAlreadyDefined(InterfaceProperty property)
        => diagnostics.Error(
            DiagnosticId.S0003PropertyAlreadyDefined,
            new SourceLocation(property.GetRoot().SourceFile, property.SourceSpan ?? default),
            $"The '{property.Name}' property is already defined.");

    public void InterfaceMethodAlreadyDefined(InterfaceMethod method)
        => diagnostics.Error(
            DiagnosticId.S0004MethodAlreadyDefined,
            new SourceLocation(method.GetRoot().SourceFile, method.SourceSpan ?? default),
            $"The '{method.Name}' method is already defined.");

    public void PropertyAlreadyDefined(PropertyDeclaration property)
        => diagnostics.Error(
            DiagnosticId.S0003PropertyAlreadyDefined,
            new SourceLocation(property.GetRoot().SourceFile, property.SourceSpan ?? default),
            $"The '{property.Name}' property is already defined.");

    public void MethodAlreadyDefined(MethodDeclaration method)
        => diagnostics.Error(
            DiagnosticId.S0004MethodAlreadyDefined,
            new SourceLocation(method.GetRoot().SourceFile, method.SourceSpan ?? default),
            $"The '{method.Name}' method is already defined.");

    public void ParameterAlreadyDefined(Parameter parameter)
        => diagnostics.Error(
            DiagnosticId.S0005ParameterAlreadyDefined,
            new SourceLocation(parameter.GetRoot().SourceFile, parameter.SourceSpan ?? default),
            $"The '{parameter.Name}' parameter is already defined.");

    public void ParameterAlreadyDefined(ISemanticNode parameter, string name)
        => diagnostics.Error(
            DiagnosticId.S0005ParameterAlreadyDefined,
            new SourceLocation(parameter.GetRoot().SourceFile, parameter.SourceSpan ?? default),
            $"The '{name}' parameter is already defined.");

    public void TypeAlreadyDefined(TypeDeclaration type)
        => diagnostics.Error(
            DiagnosticId.S0006TypeAlreadyDefined,
            new SourceLocation(type.GetRoot().SourceFile, type.SourceSpan ?? default),
            $"The '{type.Name}' type is already defined.");

    public void TypeAlreadyDefined(TypeAliasDeclaration type)
        => diagnostics.Error(
            DiagnosticId.S0006TypeAlreadyDefined,
            new SourceLocation(type.GetRoot().SourceFile, type.SourceSpan ?? default),
            $"The '{type.Name}' type is already defined.");

    public void VariableAlreadyDefined(VariableDeclaration variable)
        => diagnostics.Error(
            DiagnosticId.S0007VariableAlreadyDefined,
            new SourceLocation(variable.GetRoot().SourceFile, variable.SourceSpan ?? default),
            $"The '{variable.Name}' variable is already defined.");
}
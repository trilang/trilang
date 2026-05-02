namespace Trilang.Compilation.Diagnostics;

internal readonly record struct CompilerDiagnosticReporter(DiagnosticCollection Diagnostics)
{
    public void DuplicateDependency(ProjectInfo project, ProjectInfo dependency)
        => Diagnostics.Error(
            DiagnosticId.C0001DuplicateDependency,
            null,
            $"Duplicate dependency detected: '{dependency.Name}' for project '{project.Name}'.");

    public void DuplicateDependency(ProjectInfo project)
        => Diagnostics.Error(
            DiagnosticId.C0001DuplicateDependency,
            null,
            $"Duplicate project detected: '{project.Name}'.");

    public void CyclicDependency()
        => Diagnostics.Error(
            DiagnosticId.C0002CyclicDependency,
            null,
            "Cyclic dependency detected.");
}
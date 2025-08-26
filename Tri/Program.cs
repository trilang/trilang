using CommandLine;
using Tri;
using Trilang.Compilation;

Parser.Default
    .ParseArguments<Options>(args)
    .WithParsed(o =>
    {
        var options = new CompilerOptions(o.OutputPath);

        if (o.Path is not null)
            options.Path = o.Path;

        if (o.OperatingSystem is not null)
            options.OperatingSystem = o.OperatingSystem.Value;

        if (o.Platform is not null)
            options.Platform = o.Platform.Value;

        if (o.PrintIr)
            options.PrintIr = true;

        if (o.Directives is not null)
            foreach (var directive in o.Directives)
                options.AddDirective(directive);

        var compiler = new Compiler();
        compiler.Compile(options);
    });
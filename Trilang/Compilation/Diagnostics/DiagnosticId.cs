namespace Trilang.Compilation.Diagnostics;

public readonly record struct DiagnosticId(DiagnosticCategory Category, int Id)
{
    public static readonly DiagnosticId L0001UnsupportedCharacter = ForLexer(1);
    public static readonly DiagnosticId L0002MissingEndQuoteForStringLiteral = ForLexer(2);

    public static readonly DiagnosticId P0001MissingToken = ForParser(1);
    public static readonly DiagnosticId P0002ExpectedParameter = ForParser(2);
    public static readonly DiagnosticId P0003ExpectedType = ForParser(3);
    public static readonly DiagnosticId P0004ExpectedStatement = ForParser(4);
    public static readonly DiagnosticId P0005ExpectedTypeName = ForParser(5);
    public static readonly DiagnosticId P0006ExpectedInterface = ForParser(6);
    public static readonly DiagnosticId P0007ExpectedMethodName = ForParser(7);
    public static readonly DiagnosticId P0008ExpectedVariableName = ForParser(8);
    public static readonly DiagnosticId P0009ExpectedExpression = ForParser(9);
    public static readonly DiagnosticId P0010ExpectedDeclaration = ForParser(10);
    public static readonly DiagnosticId P0011ExpectedDirectiveName = ForParser(11);
    public static readonly DiagnosticId P0012ExpectedInterfaceParameters = ForParser(12);
    public static readonly DiagnosticId P0013ExpectedIdentifier = ForParser(13);
    public static readonly DiagnosticId P0014ExpectedTypeMember = ForParser(14);

    public static readonly DiagnosticId S0001CyclicTypeAlias = ForSemantic(1);
    public static readonly DiagnosticId S0002FunctionAlreadyDefined = ForSemantic(2);
    public static readonly DiagnosticId S0003PropertyAlreadyDefined = ForSemantic(3);
    public static readonly DiagnosticId S0004MethodAlreadyDefined = ForSemantic(4);
    public static readonly DiagnosticId S0005ParameterAlreadyDefined = ForSemantic(5);
    public static readonly DiagnosticId S0006TypeAlreadyDefined = ForSemantic(6);
    public static readonly DiagnosticId S0007VariableAlreadyDefined = ForSemantic(7);
    public static readonly DiagnosticId S0008UnknownType = ForSemantic(8);
    public static readonly DiagnosticId S0009ReturnTypeMismatch = ForSemantic(9);
    public static readonly DiagnosticId S0010TypeMismatch = ForSemantic(10);
    public static readonly DiagnosticId S0011ExpectedArray = ForSemantic(11);
    public static readonly DiagnosticId S0012ExpectedFunction = ForSemantic(12);
    public static readonly DiagnosticId S0013UnknownMember = ForSemantic(13);
    public static readonly DiagnosticId S0014CantCreateObject = ForSemantic(14);
    public static readonly DiagnosticId S0015IncompatibleUnaryOperator = ForSemantic(15);
    public static readonly DiagnosticId S0016IncompatibleBinaryOperator = ForSemantic(16);
    public static readonly DiagnosticId S0017BreakOutsideLoop = ForSemantic(17);
    public static readonly DiagnosticId S0018ContinueOutsideLoop = ForSemantic(18);
    public static readonly DiagnosticId S0019MemberNotAccessible = ForSemantic(19);
    public static readonly DiagnosticId S0020NotAllPathsReturnValue = ForSemantic(20);
    public static readonly DiagnosticId S0021ThisInStaticMethod = ForSemantic(21);
    public static readonly DiagnosticId S0022ThisOutsideOfType = ForSemantic(22);
    public static readonly DiagnosticId S0023StaticMethodAsInstance = ForSemantic(23);
    public static readonly DiagnosticId S0024InstanceMethodAsStatic = ForSemantic(24);
    public static readonly DiagnosticId S0025VariableUsedBeforeDeclaration = ForSemantic(25);
    public static readonly DiagnosticId S0026MemberIsNotImplemented = ForSemantic(26);
    public static readonly DiagnosticId S0027InterfacePropertyCantBePrivate = ForSemantic(27);

    public static DiagnosticId ForLexer(int id)
        => new DiagnosticId(DiagnosticCategory.Lexer, id);

    public static DiagnosticId ForParser(int id)
        => new DiagnosticId(DiagnosticCategory.Parser, id);

    public static DiagnosticId ForSemantic(int id)
        => new DiagnosticId(DiagnosticCategory.Semantic, id);

    public override string ToString()
    {
        var category = Category switch
        {
            DiagnosticCategory.Lexer => "L",
            DiagnosticCategory.Parser => "P",
            DiagnosticCategory.Semantic => "S",
            _ => throw new ArgumentOutOfRangeException(nameof(Category)),
        };

        return $"{category}{Id:0000}";
    }
}
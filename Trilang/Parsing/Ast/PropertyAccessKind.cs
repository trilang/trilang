namespace Trilang.Parsing.Ast;

[Flags]
public enum PropertyAccessKind
{
    Read = 0x1,
    Write = 0x2,
    ReadWrite = Read | Write,
}
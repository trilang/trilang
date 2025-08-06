using Trilang.IntermediateRepresentation.Instructions;
using Trilang.Metadata;

namespace Trilang.IntermediateRepresentation;

internal class IrBuilder
{
    private readonly Block entryBlock;
    private readonly Dictionary<string, Block> blocks;
    private int registerCounter;
    private Block currentBlock;

    public IrBuilder()
    {
        entryBlock = new Block("entry");
        blocks = [];
        registerCounter = 0;
        currentBlock = entryBlock;
    }

    private Register CreateRegister(ITypeMetadata type)
    {
        if (!type.IsValueType && type is not TypePointerMetadata)
            type = new TypePointerMetadata(type);

        return new Register(registerCounter++, type);
    }

    private Register BinaryInstruction(ITypeMetadata type, BinaryInstructionKind kind, Register left, Register right)
    {
        var register = CreateRegister(type);
        var binaryInstruction = new BinaryOperation(register, kind, left, right);
        currentBlock.AddInstruction(binaryInstruction);

        return register;
    }

    private Register UnaryInstruction(ITypeMetadata type, UnaryInstructionKind kind, Register operand)
    {
        var register = CreateRegister(type);
        var unaryInstruction = new UnaryOperation(register, kind, operand);
        currentBlock.AddInstruction(unaryInstruction);

        return register;
    }

    public Block CreateBlock(string name)
    {
        var block = new Block(name);
        blocks.TryAdd(block.Label, block);

        return block;
    }

    public void UseBlock(Block block)
        => currentBlock = block;

    public Block? FindBlock(string name)
        => blocks.GetValueOrDefault(name);

    public void Branch(Register conditionRegister, Block thenBlock, Block elseBlock)
    {
        var branch = new Branch(conditionRegister, thenBlock.Label, elseBlock.Label);
        currentBlock.AddInstruction(branch);
        currentBlock.AddNext(thenBlock);
        currentBlock.AddNext(elseBlock);
    }

    public void Jump(Block block)
    {
        var jump = new Jump(block.Label);
        currentBlock.AddInstruction(jump);
        currentBlock.AddNext(block);
    }

    public Register LoadConst(ITypeMetadata type, object? value)
    {
        var register = CreateRegister(type);
        var loadInstruction = new LoadConst(register, value);
        currentBlock.AddInstruction(loadInstruction);

        return register;
    }

    public void LoadParameter(string name, ITypeMetadata type, int index)
    {
        var register = CreateRegister(type);
        var loadInstruction = new LoadParameter(register, index);
        currentBlock.AddInstruction(loadInstruction);
        AddDefinition(name, register);
    }

    public Register Move(Register source)
    {
        var register = CreateRegister(source.Type);
        var move = new Move(register, source);
        currentBlock.AddInstruction(move);

        return register;
    }

    public Register Move(Register destination, Register source)
    {
        var move = new Move(destination, source);
        currentBlock.AddInstruction(move);

        return destination;
    }

    public Register NewArray(TypeArrayMetadata type, Register size)
    {
        var register = CreateRegister(type);
        var allocate = new NewArray(register, type, size);
        currentBlock.AddInstruction(allocate);

        return register;
    }

    public Register NewObject(ConstructorMetadata constructor, IReadOnlyList<Register> parameters)
    {
        var register = CreateRegister(constructor.DeclaringType);
        var allocate = new NewObject(register, constructor, parameters);
        currentBlock.AddInstruction(allocate);

        return register;
    }

    public void Return(Register? register = null)
    {
        var ret = new Return(register);
        currentBlock.AddInstruction(ret);
    }

    public Register ArrayElement(Register array, Register index)
    {
        // TODO:
        var pointer = (TypePointerMetadata)array.Type;
        var type = ((TypeArrayMetadata)pointer.Type).ItemMetadata!;

        var register = CreateRegister(type);
        var element = new ArrayElement(register, array, index);
        currentBlock.AddInstruction(element);

        return register;
    }

    public Register Add(ITypeMetadata type, Register left, Register right)
        => BinaryInstruction(type, BinaryInstructionKind.Add, left, right);

    public Register Sub(ITypeMetadata type, Register left, Register right)
        => BinaryInstruction(type, BinaryInstructionKind.Sub, left, right);

    public Register Mul(ITypeMetadata type, Register left, Register right)
        => BinaryInstruction(type, BinaryInstructionKind.Mul, left, right);

    public Register Div(ITypeMetadata type, Register left, Register right)
        => BinaryInstruction(type, BinaryInstructionKind.Div, left, right);

    public Register Mod(ITypeMetadata type, Register left, Register right)
        => BinaryInstruction(type, BinaryInstructionKind.Mod, left, right);

    public Register And(ITypeMetadata type, Register left, Register right)
        => BinaryInstruction(type, BinaryInstructionKind.And, left, right);

    public Register Or(ITypeMetadata type, Register left, Register right)
        => BinaryInstruction(type, BinaryInstructionKind.Or, left, right);

    public Register Xor(ITypeMetadata type, Register left, Register right)
        => BinaryInstruction(type, BinaryInstructionKind.Xor, left, right);

    public Register Eq(ITypeMetadata type, Register left, Register right)
        => BinaryInstruction(type, BinaryInstructionKind.Eq, left, right);

    public Register Ne(ITypeMetadata type, Register left, Register right)
        => BinaryInstruction(type, BinaryInstructionKind.Ne, left, right);

    public Register Lt(ITypeMetadata type, Register left, Register right)
        => BinaryInstruction(type, BinaryInstructionKind.Lt, left, right);

    public Register Le(ITypeMetadata type, Register left, Register right)
        => BinaryInstruction(type, BinaryInstructionKind.Le, left, right);

    public Register Gt(ITypeMetadata type, Register left, Register right)
        => BinaryInstruction(type, BinaryInstructionKind.Gt, left, right);

    public Register Ge(ITypeMetadata type, Register left, Register right)
        => BinaryInstruction(type, BinaryInstructionKind.Ge, left, right);

    public Register Neg(ITypeMetadata type, Register operand)
        => UnaryInstruction(type, UnaryInstructionKind.Neg, operand);

    public Register Not(ITypeMetadata type, Register operand)
        => UnaryInstruction(type, UnaryInstructionKind.Not, operand);

    public Register AddAssignment(string name, Register register)
    {
        currentBlock.AddAssignment(name, register, false);

        return register;
    }

    public Register AddDefinition(string name, Register register)
    {
        currentBlock.AddAssignment(name, register, true);

        return register;
    }

    public IrCode Build()
        => new IrCode(entryBlock) { RegisterCounter = registerCounter };

    public Block CurrentBlock
        => currentBlock;
}
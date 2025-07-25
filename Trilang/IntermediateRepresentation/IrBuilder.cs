using Trilang.IntermediateRepresentation.Instructions;
using Trilang.Metadata;
using Trilang.Parsing.Ast;

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

    private Register CreateRegister()
        => new Register(registerCounter++);

    private Register BinaryInstruction(BinaryInstructionKind kind, Register left, Register right)
    {
        var register = CreateRegister();
        var binaryInstruction = new BinaryInstruction(register, kind, left, right);
        currentBlock.AddInstruction(binaryInstruction);

        return register;
    }

    private Register UnaryInstruction(UnaryInstructionKind kind, Register operand)
    {
        var register = CreateRegister();
        var unaryInstruction = new UnaryInstruction(register, kind, operand);
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
        var branch = new BranchInstruction(conditionRegister, thenBlock.Label, elseBlock.Label);
        currentBlock.AddInstruction(branch);
        currentBlock.AddNext(thenBlock);
        currentBlock.AddNext(elseBlock);
    }

    public void Jump(Block block)
    {
        var jump = new JumpInstruction(block.Label);
        currentBlock.AddInstruction(jump);
        currentBlock.AddNext(block);
    }

    public Register Load(object? value)
    {
        var register = CreateRegister();
        var loadInstruction = new LoadInstruction(register, value);
        currentBlock.AddInstruction(loadInstruction);

        return register;
    }

    private void LoadParameter(string name, int index)
    {
        var register = CreateRegister();
        var loadInstruction = new LoadParameterInstruction(register, index);
        currentBlock.AddInstruction(loadInstruction);
        AddDefinition(name, register);
    }

    public void LoadParameters(IReadOnlyList<ParameterNode> parameters)
    {
        foreach (var (i, parameter) in parameters.Index())
            LoadParameter(parameter.Name, i);
    }

    public Register Move(Register source)
    {
        var register = CreateRegister();
        var move = new MoveInstruction(register, source);
        currentBlock.AddInstruction(move);

        return register;
    }

    public Register Move(Register destination, Register source)
    {
        var move = new MoveInstruction(destination, source);
        currentBlock.AddInstruction(move);

        return destination;
    }

    public Register NewArray(TypeArrayMetadata type, Register size)
    {
        var register = CreateRegister();
        var allocate = new NewArrayInstruction(register, type, size);
        currentBlock.AddInstruction(allocate);

        return register;
    }

    public Register NewObject(ConstructorMetadata constructor, IReadOnlyList<Register> parameters)
    {
        var register = CreateRegister();
        var allocate = new NewObjectInstruction(register, constructor, parameters);
        currentBlock.AddInstruction(allocate);

        return register;
    }

    public void Return(Register? register = null)
    {
        var ret = new ReturnInstruction(register);
        currentBlock.AddInstruction(ret);
    }

    public Register ArrayElement(Register array, Register index)
    {
        var register = CreateRegister();
        var element = new ArrayElement(register, array, index);
        currentBlock.AddInstruction(element);

        return register;
    }

    public Register Add(Register left, Register right)
        => BinaryInstruction(BinaryInstructionKind.Add, left, right);

    public Register Sub(Register left, Register right)
        => BinaryInstruction(BinaryInstructionKind.Sub, left, right);

    public Register Mul(Register left, Register right)
        => BinaryInstruction(BinaryInstructionKind.Mul, left, right);

    public Register Div(Register left, Register right)
        => BinaryInstruction(BinaryInstructionKind.Div, left, right);

    public Register Mod(Register left, Register right)
        => BinaryInstruction(BinaryInstructionKind.Mod, left, right);

    public Register And(Register left, Register right)
        => BinaryInstruction(BinaryInstructionKind.And, left, right);

    public Register Or(Register left, Register right)
        => BinaryInstruction(BinaryInstructionKind.Or, left, right);

    public Register Xor(Register left, Register right)
        => BinaryInstruction(BinaryInstructionKind.Xor, left, right);

    public Register Eq(Register left, Register right)
        => BinaryInstruction(BinaryInstructionKind.Eq, left, right);

    public Register Ne(Register left, Register right)
        => BinaryInstruction(BinaryInstructionKind.Ne, left, right);

    public Register Lt(Register left, Register right)
        => BinaryInstruction(BinaryInstructionKind.Lt, left, right);

    public Register Le(Register left, Register right)
        => BinaryInstruction(BinaryInstructionKind.Le, left, right);

    public Register Gt(Register left, Register right)
        => BinaryInstruction(BinaryInstructionKind.Gt, left, right);

    public Register Ge(Register left, Register right)
        => BinaryInstruction(BinaryInstructionKind.Ge, left, right);

    public Register Neg(Register operand)
        => UnaryInstruction(UnaryInstructionKind.Neg, operand);

    public Register Not(Register operand)
        => UnaryInstruction(UnaryInstructionKind.Not, operand);

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
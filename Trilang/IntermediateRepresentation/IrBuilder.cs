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
        => new Register(registerCounter++, type);

    private ITypeMetadata MapToIrType(ITypeMetadata type)
        => !type.IsValueType ? new TypePointerMetadata(type) : type;

    private Register BinaryInstruction(ITypeMetadata type, BinaryInstructionKind kind, Register left, Register right)
    {
        var register = CreateRegister(MapToIrType(type));
        var binaryInstruction = new BinaryOperation(register, kind, left, right);
        currentBlock.AddInstruction(binaryInstruction);

        return register;
    }

    private Register UnaryInstruction(ITypeMetadata type, UnaryInstructionKind kind, Register operand)
    {
        var register = CreateRegister(MapToIrType(type));
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
        var register = CreateRegister(MapToIrType(type));
        var loadInstruction = new LoadConst(register, value);
        currentBlock.AddInstruction(loadInstruction);

        return register;
    }

    public void LoadParameter(string name, ITypeMetadata type, int index)
    {
        var register = CreateRegister(MapToIrType(type));
        var loadInstruction = new LoadParameter(register, index);
        currentBlock.AddInstruction(loadInstruction);
        AddDefinition(name, register);
    }

    public Register Load(Register source)
    {
        var pointer = (TypePointerMetadata)source.Type;
        var register = CreateRegister(pointer.Type);
        var load = new Load(register, source);
        currentBlock.AddInstruction(load);

        return register;
    }

    private int GetLevel(ITypeMetadata type)
    {
        var level = 0;

        while (type is TypePointerMetadata pointer)
        {
            level++;
            type = pointer.Type;
        }

        return level;
    }

    public Register Deref(Register source, ITypeMetadata expected)
    {
        expected = MapToIrType(expected);

        var sourceLevel = GetLevel(source.Type);
        var expectedLevel = GetLevel(expected);

        if (sourceLevel == expectedLevel)
            return source;

        if (sourceLevel > expectedLevel)
            return Load(source);

        throw new IrException($"Cannot dereference from pointer level {sourceLevel} to higher level {expectedLevel}. Expected level must be less than or equal to source level.");
    }

    public void Store(Register destination, Register source)
    {
        var store = new Store(destination, source);
        currentBlock.AddInstruction(store);
    }

    public Register Move(Register source)
    {
        var register = CreateRegister(source.Type);

        return Move(register, source);
    }

    public Register Move(Register destination, Register source)
    {
        var move = new Move(destination, source);
        currentBlock.AddInstruction(move);

        return destination;
    }

    public void Return(Register? register = null)
    {
        var ret = new Return(register);
        currentBlock.AddInstruction(ret);
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

    public Register GetElementPointer(Register array, Register index)
    {
        var pointer = (TypePointerMetadata)array.Type;
        var type = ((TypeArrayMetadata)pointer.Type).ItemMetadata!;
        var register = CreateRegister(new TypePointerMetadata(type));
        var element = new GetElementPointer(register, array, index);
        currentBlock.AddInstruction(element);

        return register;
    }

    public Register GetMemberPointer(IMetadata member, Register? source = null)
    {
        var resultType = member switch
        {
            FieldMetadata field => MapToIrType(field.Type),
            MethodMetadata method => method.Type,
            ConstructorMetadata constructor => constructor.Type,
            FunctionMetadata function => function.Type,

            // TODO: interface?
            _ => throw new IrException($"Unsupported member type '{member.GetType().Name}'.")
        };

        var register = CreateRegister(new TypePointerMetadata(resultType));
        var getMember = new GetMemberPointer(register, source, member);
        currentBlock.AddInstruction(getMember);

        return register;
    }

    public Register Call(Register function, IReadOnlyList<Register> parameters, bool isStatic)
    {
        var functionType = (FunctionTypeMetadata)function.Type;
        var register = CreateRegister(MapToIrType(functionType.ReturnType));
        var call = new Call(register, function, parameters, isStatic);
        currentBlock.AddInstruction(call);

        return register;
    }

    public Register Alloc(ITypeMetadata type)
    {
        var register = CreateRegister(new TypePointerMetadata(type));
        var allocate = new Alloc(register, type.Layout!.Size);
        currentBlock.AddInstruction(allocate);

        return register;
    }

    public Register ArrayAlloc(TypeArrayMetadata type, Register size)
    {
        var register = CreateRegister(MapToIrType(type));
        var allocate = new ArrayAlloc(register, type.Layout!.Size, type.ItemMetadata!.Layout!.Size, size);
        currentBlock.AddInstruction(allocate);

        return register;
    }

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
namespace Trilang.OutputFormats.Elf;

internal class ElfWriter
{
    private const string ElfHeaderName = "ELF Header";
    private const string ProgramHeaderName = "Program Header";
    private const string PhCodeName = "PH Code";
    private const string PhDataName = "PH Data";
    private const string ContentCodeName = "Content Code";
    private const string ContentDataName = "Content Data";
    private const string ContentShStrTabName = "Content Shstrtab";
    private const string ShNullName = "SH Null";
    private const string ShTextName = "SH Text";
    private const string ShDataName = "SH Text";
    private const string ShShStrTabName = "SH ShStrTab";

    private const ulong BaseAddress = 0x00400000;

    public void Write(ElfOptions options)
    {
        var outputFile = new OutputFile();

        // add null segments to calculate offsets correctly
        outputFile.Add(new NullSegment(ElfHeaderName, ElfHeader.ElfHeaderSize));

        var nullProgramHeader = new NullSegment(ProgramHeaderName, ElfHeader.PhSize);
        outputFile.Add(nullProgramHeader);
        outputFile.Add(new NullSegment(PhCodeName, ElfHeader.PhSize));
        outputFile.Add(new NullSegment(PhDataName, ElfHeader.PhSize));

        // TODO: replace by code
        var contentCode = new DataSegment(ContentCodeName, [
            0x20, 0x00, 0x80, 0xD2, // mov x0, #1
            0xE1, 0x00, 0x00, 0x58, // ldr x1, =msg
            0xC2, 0x01, 0x80, 0xD2, // mov x2, #14
            0x08, 0x08, 0x80, 0xD2, // mov x8, #64
            0x01, 0x00, 0x00, 0xD4, // svc #0

            0x20, 0x00, 0x80, 0xD2, // mov x0, #1
            0xA8, 0x0B, 0x80, 0xD2, // mov x8, #93
            0x01, 0x00, 0x00, 0xD4, // svc #0

            0x10, 0x01, 0x41, 0x00, // TODO: address 0x400110
            0x00, 0x00, 0x00, 0x00,
        ]);
        outputFile.Add(contentCode);

        var contentData = new DataSegment(ContentDataName, [
            .."Hello, World!\n"u8, 0x00
        ]);
        outputFile.Add(contentData);

        var shstrtab = new StringSegment(ContentShStrTabName);
        var shstrtabOffset = shstrtab.Add(".shstrtab");
        var textOffset = shstrtab.Add(".text");
        var dataOffset = shstrtab.Add(".data");
        outputFile.Add(shstrtab);

        // add section headers
        var sectionHeader = SectionHeader.CreateNullSection(ShNullName);
        outputFile.Add(sectionHeader);
        var codeSection = SectionHeader.CreateProgBits(
            segmentName: ShTextName,
            nameOffset: (uint)textOffset,
            address: BaseAddress + (ulong)contentCode.FileOffset,
            offset: (ulong)contentCode.FileOffset,
            size: (ulong)contentCode.FileSize,
            alignment: 8);
        outputFile.Add(codeSection);
        var dataSection = SectionHeader.CreateData(
            segmentName: ShDataName,
            nameOffset: (uint)dataOffset,
            address: BaseAddress + 0x10000 + (ulong)contentData.FileOffset,
            offset: (ulong)contentData.FileOffset,
            size: (ulong)contentData.FileSize,
            alignment: 1);
        outputFile.Add(dataSection);
        outputFile.Add(SectionHeader.CreateShStrTab(
            segmentName: ShShStrTabName,
            nameOffset: (uint)shstrtabOffset,
            offset: (ulong)shstrtab.FileOffset,
            size: (ulong)shstrtab.FileSize));

        // populate null segments
        outputFile.Set(ProgramHeaderName, ProgramHeader.CreateProgramHeader(
            segmentName: ProgramHeaderName,
            offset: (ulong)nullProgramHeader.FileOffset,
            address: BaseAddress + (ulong)nullProgramHeader.FileOffset,
            phFileSize: 3 * ElfHeader.PhSize, // TODO: calculate?
            alignment: 0
        ));
        outputFile.Set(PhCodeName, ProgramHeader.CreateProgramHeaderCode(
            segmentName: PhCodeName,
            offset: 0,
            address: BaseAddress,
            phFileSize: (ulong)(contentCode.FileOffset + contentCode.FileSize),
            alignment: 0x10000 // TODO: calculate?
        ));
        outputFile.Set(PhDataName, ProgramHeader.CreateProgramHeaderData(
            segmentName: PhDataName,
            offset: (ulong)contentData.FileOffset,
            address: dataSection.Address,
            phFileSize: dataSection.Size,
            alignment: 0x10000 // TODO: calculate?
        ));

        var elfHeader = new ElfHeader(
            segmentName: ElfHeaderName,
            endianness: ElfEndianness.Little, // TODO: support big-endian
            instructionSet: options.InstructionSet,
            entryPoint: codeSection.Address,
            programHeaderOffset: (ulong)nullProgramHeader.FileOffset,
            sectionHeaderOffset: (ulong)sectionHeader.FileOffset,
            programHeaderCount: 3, // TODO: calculate?
            sectionHeaderCount: 4, // TODO: calculate?
            sectionNameIndex: 3 // TODO: calculate?
        );
        outputFile.Set(ElfHeaderName, elfHeader);

        using var file = new BinaryStream(
            File.Open(options.OutputPath, FileMode.Create, FileAccess.Write),
            elfHeader.Endianness == ElfEndianness.Little);

        outputFile.WriteTo(file);
    }
}
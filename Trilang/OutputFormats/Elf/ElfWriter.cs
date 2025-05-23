namespace Trilang.OutputFormats.Elf;

internal class ElfWriter
{
    private const string ElfHeaderName = "ELF Header";
    private const string ProgramHeaderName = "Program Header";
    private const string PhCodeName = "PH Code";
    private const string ContentCodeName = "Content Code";
    private const string ContentShStrTabName = "Content Shstrtab";
    private const string ShNullName = "SH Null";
    private const string ShTextName = "SH Text";
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

        // TODO: replace by code
        var contentCode = new DataSegment(ContentCodeName, [
            0x20, 0x00, 0x80, 0xD2, // mov x0, #1
            0xA8, 0x0B, 0x80, 0xD2, // mov x8, #93
            0x01, 0x00, 0x00, 0xD4, // svc #0
        ]);
        outputFile.Add(contentCode);

        var shstrtab = new StringSegment(ContentShStrTabName);
        var shstrtabOffset = shstrtab.Add(".shstrtab");
        var textOffset = shstrtab.Add(".text");
        outputFile.Add(shstrtab);

        // add section headers
        var sectionHeader = SectionHeader.CreateNullSection(ShNullName);
        outputFile.Add(sectionHeader);
        var progBits = SectionHeader.CreateProgBits(
            segmentName: ShTextName,
            nameOffset: (uint)textOffset,
            address: BaseAddress + (ulong)contentCode.FileOffset,
            offset: (ulong)contentCode.FileOffset,
            size: (ulong)contentCode.FileSize,
            alignment: 0);
        outputFile.Add(progBits);
        outputFile.Add(SectionHeader.CreateShStrTab(
            segmentName: ShShStrTabName,
            nameOffset: (uint)shstrtabOffset,
            offset: (ulong)shstrtab.FileOffset,
            size: (ulong)shstrtab.FileSize,
            alignment: 0));

        // populate null segments
        outputFile.Set(ProgramHeaderName, ProgramHeader.CreateProgramHeader(
            segmentName: ProgramHeaderName,
            offset: (ulong)nullProgramHeader.FileOffset,
            address: BaseAddress + (ulong)nullProgramHeader.FileOffset,
            phFileSize: 2 * ElfHeader.PhSize,
            alignment: 0
        ));
        outputFile.Set(PhCodeName, ProgramHeader.CreateProgramHeaderCode(
            segmentName: PhCodeName,
            offset: 0,
            address: BaseAddress,
            phFileSize: (ulong)(contentCode.FileOffset + contentCode.FileSize),
            alignment: 0
        ));

        var elfHeader = new ElfHeader(
            segmentName: ElfHeaderName,
            endianness: ElfEndianness.Little, // TODO: support big-endian
            instructionSet: options.InstructionSet,
            entryPoint: progBits.Address,
            programHeaderOffset: (ulong)nullProgramHeader.FileOffset,
            sectionHeaderOffset: (ulong)sectionHeader.FileOffset,
            programHeaderCount: 2, // TODO: calculate?
            sectionHeaderCount: 3, // TODO: calculate?
            sectionNameIndex: 2 // TODO: calculate?
        );
        outputFile.Set(ElfHeaderName, elfHeader);

        using var file = new BinaryStream(
            File.Open(options.OutputPath, FileMode.Create, FileAccess.Write),
            elfHeader.Endianness == ElfEndianness.Little);

        outputFile.WriteTo(file);
    }
}
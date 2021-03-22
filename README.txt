MIPS DASM is a very simple MIPS disassembler.

=== Passing options
All options with integers must be in decimal.

=== Disassembling
To disassemble specify the size `-s` of how many instructions to disassemble (size = bytes / 4).
Then specify the offset from the file to start disassembling at, for example: -z 1024.

=== Output
The human-readable output will be sent to any specific file (--output option). This output can then be re-assembled with GAS.

=== Labels
Labels as func_xxxx are believed to be functions/routines. Those are from JAL/JALR instructions.
If it is l_xxxx then the label comes from a branching instruction.

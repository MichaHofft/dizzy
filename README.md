# dizzy
Dizzy is a dead slow Z80 emulation environment in Python.

Long term goal is to have a Z80 and a minimal environment hosted in a FPGA. Preferably, this would be an ICE40 or so, which would be enabled for a full Linux/ open source tool chain.

The roadmap is:

| Item | Comment | Status |
| --- | --- | --- |
| Assembler | translate Z80 assembly into binary. Compile CP/M 2.2 code. | Running |
| Dis-assembler | reverse binary into assembler code with auto-generated labelling | Running |
| Soft-CPU | Execute Z80 binary by an interpreter on-the-fly | In progress |
| Soft-environment | Minimal emulated environment to boot CP/M | Pending |
| Optimize | Optimize Soft-CPU as much as possible in order to facilitate FPGA process | Pending |
| FPGA | The hard stuff | Visionary |


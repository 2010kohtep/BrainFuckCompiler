unit &Assembler.Global;

interface

const
  REG_COUNT = 8;

type
  TNumberScale = (nsNo, nsBy2, nsBy4, nsBy8);
  TStrSize = (msByte, msWord, msDWord);
  TCmdPrefix = (cpRegSize   = $66,  // Overrides operand size (eax -> ax)
                cpAddrSize  = $67,  // Overrides address size
                cpWait      = $9B,  // wait
                cpLock      = $F0,  // lock
                cpRep       = $F2,  // rep
                cpRepNZ     = $F3); // repne

  TRegIndex = (rEax, rEcx, rEdx, rEbx, rEsp, rEbp, rEsi, rEdi);

  TAddressingType = (atIndirAddr,   // 00b
                     atBaseAddr8B,  // 01b
                     atBaseAddr32B, // 10b (16B for 16-bit ÎS)
                     atRegisters);  // 11b

  // SIB – Scale Index Base
  // If you specify the same index and base, one register is going to be used.
  TSIBInfo = packed record // *atRegAddr doesn't need this
    Scale: TNumberScale; // 00b
    Index: TRegIndex; // 000b
    Base: TRegIndex; // 000b
  end;

  TRMInfo = packed record
    IsBase: Boolean; // 0b
    B1: Boolean; // 0b
    B2: Boolean; // 0b
  end;

  // "/0" - opcode's expansion due to Reg field in ModR/M byte.
  // "/r" - Registers are used in ModR/M.

  // ref.x86asm.net
  // o (register/opcode field) - RegFrom value (manually sets via TRegIndex)

  TModRMInfo = packed record
    Addr: TAddressingType; // 00b
    FReg: TRegIndex; //000b
    case Integer of // 000b
      1: (SReg: TRegIndex);
      2: (Memory: TRMInfo); // Integer = 101b
    end;

implementation

end.

unit &Assembler.Global;

interface

type
  TNumberScale = (nsNo, nsBy2, nsBy4, nsBy8);
  TAddrSize = (msByte, msWord, msDWord);
  TCmdPrefix = (cpRegSize   = $66,  // Overrides operand size (eax -> ax)
                cpAddrSize  = $67,  // Overrides address size
                cpWait      = $9B,  // wait
                cpLock      = $F0,  // lock
                cpRep       = $F2,  // rep
                cpRepNZ     = $F3); // repne

  TRegIndex =    (rEax,  rEcx,  rEdx,  rEbx,  rEsp,  rEbp,  rEsi,  rEdi);

  TReg64Index = (rRax, rRcx, rRdx, rRbx, rRsp, rRbp, rRsi, rRdi,
                 r8,   r9,   r10,  r11,  r12,  r13,  r14,  r15);

  TAddressingType = (atIndirAddr,   // 00b
                     atBaseAddr8B,  // 01b
                     atBaseAddr32B, // 10b (16B for 16-bit OS)
                     atRegisters);  // 11b

  // SIB – Scale Index Base
  // If you specify the same index and base, one register is going to be used.
  // TAddressingType.atRegisters doesn't need SIB byte.
  TSIBInfo = packed record
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

  TJumpType = (jtJo, jtJno,
               jtJb, jtJnb,
               jtJz, jtJnz,
               jtJa, jtJna,
               jtJs, jtJns,
               jtJp, jtJnp,
               jtJl, jtJnl,
               jtJg, jtJng,
               jtJmp);

  TCondition = (cOverflow, cNotOverflow,
                cBelow, cNotBelow,
                cZero, cNotZero,
                cAbove, cNotAbove,
                cSign, cNotSign,
                cParity, cNotParity,
                cLess, cNotLess,
                cNotGreater, cGreater);

const
  REG_x32_COUNT = SizeOf(TRegIndex);
  REG_x64_COUNT = SizeOf(TReg64Index);

implementation

end.

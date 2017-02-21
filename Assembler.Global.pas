unit &Assembler.Global;

interface

const
  REG_COUNT = 8;

type
  TNumberScale = (nsNo, nsBy2, nsBy4, nsBy8);
  TStrSize = (msByte, msWord, msDWord);
  TCmdPrefix = (cpRegSize   = $66,  // Переопределения размера операнда (eax -> ax)
                cpAddrSize  = $67,  // Переопределения размера адреса
                cpWait      = $9B,  // wait
                cpLock      = $F0,  // lock
                cpRep       = $F2,  // rep
                cpRepNZ     = $F3); // repne

  TRegIndex = (rEax, rEcx, rEdx, rEbx, rEsp, rEbp, rEsi, rEdi);
  TAddressingType = (atIndirAddr,   // 00b
                     atBaseAddr8B,  // 01b
                     atBaseAddr32B, // 10b (16B на 16-разрядной ОС)
                     atRegisters);  // 11b

  // Если указаны одинаковые индекс и база, то будет использован один регистр.
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

  // "/0" - расширения опкода за счёт поля Reg в байте ModR/M.
  // "/r" - указания регистров в ModR/M.

  // ref.x86asm.net
  // o (register/opcode field) - значение RegFrom (ручная установка через TRegIndex)

  TModRMInfo = packed record
    Addr: TAddressingType; // 00b
    FReg: TRegIndex; //000b
    case LongInt of // 000b
      1: (SReg: TRegIndex);
      2: (Memory: TRMInfo); // LongInt = 101b
    end;

implementation

end.

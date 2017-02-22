unit Compiler;

interface

uses
  SysUtils, Buffer, &Assembler.Global;

type
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

  TTarget = (tWin32, tWin64, tLinux);

  TCompiler = object
  strict private
    function InstructionPtr: Pointer;
  strict protected
    FBuffer: TBuffer;

    FTarget: TTarget;

    FSwapOperand: Boolean;
    FForceMemory: Boolean;
  public
    {$REGION 'Properties'}
    property Target: TTarget read FTarget default tWin32;
    property ToMemory: Boolean read FForceMemory write FForceMemory default False;
    property IP: Pointer read InstructionPtr;
    {$ENDREGION}

    {$REGION 'Constructors/Destructors'}
    procedure Create;
    {$ENDREGION}

    {$REGION 'Raw Write Functions'}
    procedure WriteModRM(AddrMod: TAddressingType; Dest, Source: TRegIndex); overload; stdcall;
    procedure WriteModRM(AddrMod: TAddressingType; Dest: TRegIndex; IsBase, B1, B2: Boolean); overload; stdcall;
    procedure WriteSIB(Scale: TNumberScale; Base, Index: TRegIndex);
    procedure WriteBase(Dest, Base, Index: TRegIndex; Scale: TNumberScale; Offset: Integer); stdcall;
    procedure WritePrefix(Prefix: TCmdPrefix);
    {$ENDREGION}

    procedure WriteAnd(Dest, Base, Index: TRegIndex; Scale: TNumberScale = nsNo; Offset: Integer = 0); overload; stdcall;
    procedure WriteAnd(Dest, Source: TRegIndex); overload;
    procedure WriteAnd(Dest: TRegIndex; Value: Integer); overload;

    procedure WriteMov(Dest, Base, Index: TRegIndex; Scale: TNumberScale = nsNo; Offset: Integer = 0); overload; stdcall;
    procedure WriteMov(Dest, Source: TRegIndex); overload;
    procedure WriteMov(Dest: TRegIndex; Value: Integer); overload;

    procedure WriteLea(Dest, Base, Index: TRegIndex; Scale: TNumberScale = nsNo; Offset: Integer = 0); overload; stdcall;
    procedure WriteLea(Dest, Source: TRegIndex; Offset: Integer = 0); overload;
    procedure WriteLea(Dest: TRegIndex; Value: Integer); overload;

    procedure WriteAdd(Dest, Base, Index: TRegIndex; Scale: TNumberScale = nsNo; Offset: Integer = 0); overload; stdcall;
    procedure WriteAdd(Dest: TRegIndex; Value: Integer); overload;
    procedure WriteAdd(Dest, Source: TRegIndex); overload;

    procedure WriteSub(Dest: TRegIndex; Value: Integer); overload;
    procedure WriteSub(Dest, Source: TRegIndex); overload;

    procedure WriteTest(Dest, Source: TRegIndex); overload;
    procedure WriteTest(Dest: TRegIndex; Value: Integer); overload;

    procedure WriteXor(Dest, Source: TRegIndex); overload;
    procedure WriteXor(Dest: TRegIndex; Value: Integer); overload;

    procedure WriteCmp(Dest, Source: TRegIndex); overload;
    procedure WriteCmp(Dest: TRegIndex; Value: Integer); overload;

    procedure WritePush(Source: TRegIndex; Dereference: Boolean = False); overload;
    procedure WritePush(Value: Integer; Dereference: Boolean = False); overload;

    procedure WritePop(Source: TRegIndex); overload;
    procedure WritePop(Addr: Pointer); overload;

    procedure WriteRet(Bytes: Integer = 0);

    procedure WriteNop;

    procedure WriteCall(Addr: Pointer); overload;
    procedure WriteCall(Reg: TRegIndex); overload;

    procedure WriteInt(Interrupt: Integer);

    procedure WriteInc(Reg: TRegIndex);

    procedure WriteDec(Reg: TRegIndex);

    procedure WriteXchg(Dest, Source: TRegIndex); overload;
    procedure WriteXchg(Dest: TRegIndex; Address: Pointer); overload;

    procedure WriteSetCC(Dest: TRegIndex; Condition: TCondition);

    // ...

    procedure WriteAddMem(RegTo, RegFrom: TRegIndex); overload;
    procedure WriteAddMem(RegTo: TRegIndex; Value: Byte); overload;

    procedure WriteIncMem(Reg: TRegIndex);
    procedure WriteDecMem(Reg: TRegIndex);

    procedure WriteSubMem(Reg: TRegIndex; Value: Byte); overload;

    procedure WriteMovMem(RegTo, RegFrom: TRegIndex); overload;
    procedure WriteMovMem(Dest: TRegIndex; Value: Integer); overload;

    procedure WriteJump(Jump: TJumpType; Here: Pointer);

    procedure WriteCmpMem(Reg: TRegIndex; Value: Byte);

    procedure WriteMovS(Prefix: TCmdPrefix; Count: TStrSize); overload;
    procedure WriteMovS(Count: TStrSize); overload;
    procedure WriteStoS(Prefix: TCmdPrefix; Count: TStrSize); overload;
    procedure WriteStoS(Count: TStrSize); overload;

    procedure WriteShr(Reg: TRegIndex; Count: Byte);

    class function IsRel8(Value: Integer): Boolean; static; inline;
    class procedure Swap(var FReg: TRegIndex; var SReg: TRegIndex); static; inline;
  public
    procedure RaiseException(const Text: string); overload;
    procedure RaiseException(const Fmt: string; const Args: array of const); overload;
  end;

implementation

procedure TCompiler.WriteIncMem(Reg: TRegIndex);
begin
  FBuffer.Write<Byte>($FE);

  case Reg of
    rEbp:
    begin
      WriteModRM(atBaseAddr8B, Reg, TRegIndex(0));
      FBuffer.Write<Byte>(0);
    end;

    rEsp:
    begin
      WriteModRM(atBaseAddr8B, Reg, TRegIndex(0));
      WriteSIB(nsNo, Reg, Reg);
      FBuffer.Write<Byte>(0);
    end
  else
    FBuffer.Write<Byte>(Byte(Reg));
  end;
end;

procedure TCompiler.WriteInt(Interrupt: Integer);
begin
  if Interrupt = 3 then
    FBuffer.Write<Byte>($CC)
  else
  begin
    FBuffer.Write<Byte>($CD);
    FBuffer.Write<Byte>(Interrupt);
  end;
end;

procedure TCompiler.WriteJump(Jump: TJumpType; Here: Pointer);
var
  I: Integer;
begin
  if Here = nil then
    I := 0
  else
    I := Cardinal(Here) - Cardinal(@TBytes(FBuffer.Data)[FBuffer.Position]);

  if Jump = jtJmp then
  begin
    if IsRel8(I) then
    begin
      FBuffer.Write<Byte>($EB);
      FBuffer.Write<Byte>(I - 2);
    end
    else
    begin
      FBuffer.Write<Byte>($E9);
      FBuffer.Write<Cardinal>(I - 5);
    end;
  end
  else
  begin
    if IsRel8(I) then
    begin
      FBuffer.Write<Byte>($70 or Byte(Jump));
      FBuffer.Write<Byte>(I - 2);
    end
    else
    begin
      FBuffer.Write<Byte>($0F);
      FBuffer.Write<Byte>($80 or Byte(Jump));
      FBuffer.Write<Cardinal>(I - 6);
    end;
  end;
end;

procedure TCompiler.WriteLea(Dest, Base, Index: TRegIndex; Scale: TNumberScale; Offset: Integer);
begin
  FBuffer.Write<Byte>($8D);

  WriteBase(Dest, Base, Index, Scale, Offset);
end;

procedure TCompiler.WriteLea(Dest: TRegIndex; Value: Integer);
begin
  FBuffer.Write<Byte>($8D);
  WriteModRM(atIndirAddr, TRegIndex(5), Dest);
  FBuffer.Write<Cardinal>(Value);
end;

procedure TCompiler.WriteLea(Dest, Source: TRegIndex; Offset: Integer);
begin
  FBuffer.Write<Byte>($8D);

  case Source of
    rEbp:
    begin
      if not IsRel8(Offset) then
      begin
        WriteModRM(atBaseAddr32B, Source, Dest);
        FBuffer.Write<Cardinal>(Offset);
      end
      else
      begin
        WriteModRM(atBaseAddr8B, Source, Dest);
        FBuffer.Write<Cardinal>(Offset);
      end;
    end;

    rEsp:
    begin
      if not IsRel8(Offset) then
      begin
        WriteModRM(atBaseAddr32B, Source, Dest);
        WriteSIB(nsNo, Source, Source);
        FBuffer.Write<Cardinal>(Offset);
      end
      else
      begin
        WriteModRM(atBaseAddr8B, Source, Dest);
        WriteSIB(nsNo, Source, Source);
        FBuffer.Write<Byte>(Offset);
      end;
    end
    else
    begin
      if Offset = 0 then
        WriteModRM(atIndirAddr, Source, Dest)
      else
      begin
        if IsRel8(Offset) then
        begin
          WriteModRM(atBaseAddr8B, Source, Dest);
          FBuffer.Write<Byte>(Offset);
        end
        else
        begin
          WriteModRM(atBaseAddr32B, Source, Dest);
          FBuffer.Write<Cardinal>(Offset);
        end;
      end;
    end;
  end;
end;

procedure TCompiler.WriteModRM(AddrMod: TAddressingType;
  Dest, Source: TRegIndex);
var
  B: Byte;
begin
  if FForceMemory then
    B := Byte(TAddressingType.atIndirAddr)
  else
    B := Byte(AddrMod);

  FBuffer.Write<Byte>(B shl 6 or Byte(Source) shl 3 or Byte(Dest));
end;

procedure TCompiler.WriteModRM(AddrMod: TAddressingType;
  Dest: TRegIndex; IsBase, B1, B2: Boolean);
begin
  FBuffer.Write<Byte>((Byte(AddrMod) shl 6) or (Byte(Dest) shl 3));
end;

procedure TCompiler.WriteMov(Dest: TRegIndex; Value: Integer);
begin
  FBuffer.Write<Byte>($B8 or Byte(Dest));
  FBuffer.Write<Cardinal>(Value);
end;

procedure TCompiler.WriteMov(Dest, Base, Index: TRegIndex; Scale: TNumberScale;
  Offset: Integer);
begin
  FBuffer.Write<Byte>($8B);

  WriteBase(Dest, Base, Index, Scale, Offset);
end;

procedure TCompiler.WriteMovMem(Dest: TRegIndex; Value: Integer);
begin
  FBuffer.Write<Byte>($C6);
  WriteModRM(atIndirAddr, Dest, TRegIndex(0));
  FBuffer.Write<Byte>(Value);
end;

procedure TCompiler.WriteMovS(Prefix: TCmdPrefix; Count: TStrSize);
begin
  WritePrefix(Prefix);
  WriteMovS(Count);
end;

procedure TCompiler.WriteMovMem(RegTo, RegFrom: TRegIndex);
begin
  FBuffer.Write<Byte>($88);
  WriteModRM(atIndirAddr, RegTo, RegFrom);
end;

procedure TCompiler.WriteMovS(Count: TStrSize);
begin
  case Count of
    msByte: FBuffer.Write<Byte>($A4);
    msWord:
    begin
      WritePrefix(cpAddrSize);
      FBuffer.Write<Byte>($A5);
    end;
    msDWord: FBuffer.Write<Byte>($A5);
  end;
end;

procedure TCompiler.WriteNop;
begin
  FBuffer.Write<Byte>($90);
end;

procedure TCompiler.WriteMov(Dest, Source: TRegIndex);
begin
  FBuffer.Write<Byte>($89);
  WriteModRM(atRegisters, Dest, Source);
end;

procedure TCompiler.WriteDecMem(Reg: TRegIndex);
begin
  FBuffer.Write<Byte>($FE);

  case Reg of
    rEbp:
    begin
      WriteModRM(atBaseAddr8B, Reg, TRegIndex(1));
      FBuffer.Write<Byte>(0);
    end;

    rEsp:
    begin
      WriteModRM(atBaseAddr8B, Reg, TRegIndex(1));
      WriteSIB(nsNo, Reg, Reg);
      FBuffer.Write<Byte>(0);
    end
  else
    FBuffer.Write<Byte>(Byte(Reg) + REG_COUNT);
  end;
end;

procedure TCompiler.WriteInc(Reg: TRegIndex);
begin
  FBuffer.Write<Byte>($40 or Byte(Reg));
end;

procedure TCompiler.RaiseException(const Text: string);
begin
  raise SysUtils.Exception.CreateFmt('%s', [Text]);
end;

procedure TCompiler.Create;
begin
  FSwapOperand := False;
end;

function TCompiler.InstructionPtr: Pointer;
begin
  Result := @TBytes(FBuffer.Data)[FBuffer.Position];
end;

class function TCompiler.IsRel8(Value: Integer): Boolean;
begin
  Result := (Value >= -128) and (Value <= 127);
end;

procedure TCompiler.RaiseException(const Fmt: string; const Args: array of const);
var
  Text: string;
begin
  Text := Format(Fmt, Args);
  RaiseException(Text);
end;

class procedure TCompiler.Swap(var FReg: TRegIndex; var SReg: TRegIndex);
var
  TempReg: TRegIndex;
begin
  TempReg := FReg;
  FReg := SReg;
  SReg := TempReg;
end;

procedure TCompiler.WriteAdd(Dest: TRegIndex; Value: Integer);
begin
  if not IsRel8(Value) then
  begin
    if Dest = rEax then
      FBuffer.Write<Byte>($05) // eax opcode
    else
    begin
      FBuffer.Write<Byte>($81); // default opcode
      WriteModRM(atRegisters, Dest, TRegIndex(0));
    end;

    FBuffer.Write<Cardinal>(Value);
  end
  else
  begin
    FBuffer.Write<Byte>($83); // one-byte opcode
    WriteModRM(atRegisters, Dest, TRegIndex(0));
    FBuffer.Write<Byte>(Value);
  end;
end;

// add [reg], reg
procedure TCompiler.WriteAddMem(RegTo, RegFrom: TRegIndex);
begin
  FBuffer.Write<Byte>($01);

  if RegTo = rEbp then
  begin
    WriteModRM(atBaseAddr8B, RegTo, RegFrom);
    FBuffer.Write<Byte>(0);
  end
  else
  begin
    WriteModRM(atIndirAddr, RegTo, RegFrom);

    if RegTo = rEsp then
      WriteSIB(nsNo, RegTo, RegTo);
  end;
end;

procedure TCompiler.WriteAdd(Dest, Source: TRegIndex);
begin
  FBuffer.Write<Byte>($01);
  WriteModRM(atRegisters, Dest, Source);
end;

procedure TCompiler.WriteAdd(Dest, Base, Index: TRegIndex; Scale: TNumberScale;
  Offset: Integer);
begin
  FBuffer.Write<Byte>($03);

  WriteBase(Dest, Base, Index, Scale, Offset);
end;

procedure TCompiler.WriteAddMem(RegTo: TRegIndex; Value: Byte);
begin
  FBuffer.Write<Byte>($80);
  FBuffer.Write<Byte>(Byte(RegTo));
  FBuffer.Write<Byte>(Value);
end;

procedure TCompiler.WriteAnd(Dest, Source: TRegIndex);
begin
  FBuffer.Write<Byte>($21);
  WriteModRM(atRegisters, Dest, Source);
end;

procedure TCompiler.WriteAnd(Dest: TRegIndex; Value: Integer);
begin
  if IsRel8(Value) then
  begin
    FBuffer.Write<Byte>($82);
    WriteModRM(atRegisters, Dest, TRegIndex(4));
    FBuffer.Write<Byte>(Value);
  end
  else
  begin
    FBuffer.Write<Byte>($81);
    WriteModRM(atRegisters, Dest, TRegIndex(4));
    FBuffer.Write<Integer>(Value);
  end;
end;

procedure TCompiler.WriteAnd(Dest, Base, Index: TRegIndex; Scale: TNumberScale;
  Offset: Integer);
begin
  FBuffer.Write<Byte>($23);

  WriteBase(Dest, Base, Index, Scale, Offset);
end;

procedure TCompiler.WriteBase(Dest, Base, Index: TRegIndex; Scale: TNumberScale;
  Offset: Integer);
begin
  if Index = rEsp then
    Swap(Index, Base);

  if Offset <> 0 then
  begin
    if IsRel8(Offset) then
    begin
      if (Scale = nsNo) and (Index = Base) then
        WriteModRM(atBaseAddr8B, Base, Dest)
      else
      begin
        WriteModRM(atBaseAddr8B, TRegIndex(4), Dest);
        WriteSIB(Scale, Base, Index);
      end;

      FBuffer.Write<Byte>(Offset);
    end
    else
    begin
      if (Scale = nsNo) and (Index = Base) then
        WriteModRM(atBaseAddr32B, Base, Dest)
      else
      begin
        WriteModRM(atBaseAddr32B, TRegIndex(4), Dest);
        WriteSIB(Scale, Base, Index);
      end;

      FBuffer.Write<Cardinal>(Offset);
    end;
  end
  else
  begin
    if (Scale = nsNo) and (Index = Base) then
      WriteModRM(atIndirAddr, Base, Dest)
    else
    begin
      WriteModRM(atIndirAddr, TRegIndex(4), Dest);
      WriteSIB(Scale, Base, Index);
    end;
  end;
end;

procedure TCompiler.WriteCall(Reg: TRegIndex);
begin
  FBuffer.Write<Byte>($FF);
  FBuffer.Write<Byte>($D0 or Byte(Reg));
end;

procedure TCompiler.WriteCmp(Dest, Source: TRegIndex);
begin
  FBuffer.Write<Byte>($39);
  WriteModRM(atRegisters, Dest, Source);
end;

procedure TCompiler.WriteCmp(Dest: TRegIndex; Value: Integer);
begin
  if IsRel8(Value) then
  begin
    FBuffer.Write<Byte>($83);
    WriteModRM(atRegisters, Dest, TRegIndex(7));
    FBuffer.Write<Byte>(Value);
  end
  else
  begin
    FBuffer.Write<Byte>($81);
    WriteModRM(atRegisters, Dest, TRegIndex(7));
    FBuffer.Write<Integer>(Value);
  end;
end;

procedure TCompiler.WriteCmpMem(Reg: TRegIndex; Value: Byte);
begin
  FBuffer.Write<Byte>($80);
  WriteModRM(atIndirAddr, Reg, TRegIndex(7));
  FBuffer.Write<Byte>(Value);
end;

procedure TCompiler.WriteCall(Addr: Pointer);

  function Relative(Func, Addr: Pointer): Pointer; inline;
  begin
    Result := Pointer(Cardinal(Func) - Cardinal(Addr) - 4);
  end;

begin
  FBuffer.Write<Byte>($E8);
  FBuffer.Write<Cardinal>(Cardinal(Relative(Addr, @TBytes(FBuffer.Data)[FBuffer.Position])));
end;

procedure TCompiler.WriteDec(Reg: TRegIndex);
begin
  FBuffer.Write<Byte>($48 or Byte(Reg));
end;

procedure TCompiler.WritePush(Source: TRegIndex; Dereference: Boolean);
begin
  if Dereference then
  begin
    FBuffer.Write<Byte>($FF);

    if Source = rEsp then
    begin
      WriteModRM(atIndirAddr, Source, TRegIndex(6));
      WriteSIB(nsNo, Source, Source);
    end
    else
    if Source = rEbp then
    begin
      WriteModRM(atBaseAddr8B, Source, TRegIndex(6));
      FBuffer.Write<Byte>(0);
    end
    else
      WriteModRM(atIndirAddr, Source, TRegIndex(6));
  end
  else
    FBuffer.Write<Byte>($50 or Byte(Source));
end;

procedure TCompiler.WriteRet(Bytes: Integer);
begin
  if Bytes <> 0 then
  begin
    FBuffer.Write<Byte>($C2);
    FBuffer.Write<Word>(Bytes);
  end
  else
    FBuffer.Write<Byte>($C3);
end;

procedure TCompiler.WriteSetCC(Dest: TRegIndex; Condition: TCondition);
begin
  FBuffer.Write<Byte>($0F);
  FBuffer.Write<Byte>($90 or Byte(Condition));
  WriteModRM(atRegisters, Dest, TRegIndex(3));
end;

procedure TCompiler.WriteShr(Reg: TRegIndex; Count: Byte);
begin
  FBuffer.Write<Byte>($C1);
  WriteModRM(atRegisters, Reg, TRegIndex(5));
  FBuffer.Write<Byte>(Count);
end;

procedure TCompiler.WriteSIB(Scale: TNumberScale; Base, Index: TRegIndex);
begin
  FBuffer.Write<Byte>(Byte(Scale) shl 6 or Byte(Index) shl 3 or Byte(Base));
end;

procedure TCompiler.WriteStoS(Prefix: TCmdPrefix; Count: TStrSize);
begin
  WritePrefix(Prefix);
  WriteStoS(Count);
end;

procedure TCompiler.WriteStoS(Count: TStrSize);
begin
  case Count of
    msByte: FBuffer.Write<Byte>($AA);
    msWord:
    begin
      WritePrefix(cpRegSize);
      FBuffer.Write<Byte>($AB);
    end;
    msDWord: FBuffer.Write<Byte>($AB);
  end;
end;

procedure TCompiler.WriteSub(Dest: TRegIndex; Value: Integer);
begin
  if not IsRel8(Value) then
  begin
    if Dest = rEax then
      FBuffer.Write<Byte>($2D) // eax opcode
    else
    begin
      FBuffer.Write<Byte>($81); // default opcode
      WriteModRM(atRegisters, Dest, TRegIndex(5));
    end;

    FBuffer.Write<Cardinal>(Value);
  end
  else
  begin
    FBuffer.Write<Byte>($83);
    WriteModRM(atRegisters, Dest, TRegIndex(5));
    FBuffer.Write<Byte>(Value);
  end;
end;

procedure TCompiler.WriteSub(Dest, Source: TRegIndex);
begin
  FBuffer.Write<Byte>($29);
  WriteModRM(atRegisters, Dest, Source);
end;

procedure TCompiler.WriteSubMem(Reg: TRegIndex; Value: Byte);
begin
  FBuffer.Write<Byte>($80);
  WriteModRM(atIndirAddr, Reg, TRegIndex(5));
  FBuffer.Write<Byte>(Value);
end;

procedure TCompiler.WriteTest(Dest: TRegIndex; Value: Integer);
begin
  if Dest = rEax then
  begin
    FBuffer.Write<Byte>($A9);
    FBuffer.Write<Cardinal>(Value);
  end
  else
  begin
    FBuffer.Write<Byte>($F7);
    WriteModRM(atRegisters, Dest, TRegIndex(0));
    FBuffer.Write<Cardinal>(Value);
  end;
end;

procedure TCompiler.WriteTest(Dest, Source: TRegIndex);
begin
  FBuffer.Write<Byte>($85);
  WriteModRM(atRegisters, Dest, Source);
end;

procedure TCompiler.WriteXchg(Dest, Source: TRegIndex);
begin
  if Dest = rEax then
    FBuffer.Write<Byte>($90 or Byte(Source))
  else
  begin
    FBuffer.Write<Byte>($87);
    WriteModRM(atRegisters, Dest, Source);
  end;
end;

procedure TCompiler.WriteXchg(Dest: TRegIndex; Address: Pointer);
begin

end;

procedure TCompiler.WriteXor(Dest: TRegIndex; Value: Integer);
begin
  if IsRel8(Value) then
  begin
    FBuffer.Write<Byte>($83);
    WriteModRM(atRegisters, Dest, TRegIndex(6));
    FBuffer.Write<Byte>(Value);
  end
  else
  begin
    FBuffer.Write<Byte>($81);
    WriteModRM(atRegisters, Dest, TRegIndex(6));
    FBuffer.Write<Cardinal>(Value);
  end;
end;

procedure TCompiler.WriteXor(Dest, Source: TRegIndex);
begin
  FBuffer.Write<Byte>($31);
  WriteModRM(atRegisters, Dest, Source);
end;

procedure TCompiler.WritePop(Source: TRegIndex);
begin
  FBuffer.Write<Byte>($58 or Byte(Source));
end;

procedure TCompiler.WritePop(Addr: Pointer);
begin
  FBuffer.Write<Byte>($8F);
  WriteModRM(atIndirAddr, TRegIndex(5), TRegIndex(0));
  FBuffer.Write<Cardinal>(Cardinal(Addr));
end;

procedure TCompiler.WritePrefix(Prefix: TCmdPrefix);
begin
  FBuffer.Write<Byte>(Byte(Prefix));
end;

procedure TCompiler.WritePush(Value: Integer; Dereference: Boolean = False);
begin
  if Dereference then
  begin
    FBuffer.Write<Byte>($FF);
    FBuffer.Write<Byte>($35);
    FBuffer.Write<Cardinal>(Value);
  end
  else
  begin
    if IsRel8(Value) then
    begin
      FBuffer.Write<Byte>($6A);
      FBuffer.Write<Byte>(Value);
    end
    else
    begin
      FBuffer.Write<Byte>($68);
      FBuffer.Write<Cardinal>(Value);
    end;
  end;
end;

end.

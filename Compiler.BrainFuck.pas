unit Compiler.BrainFuck;

interface

uses
  System.SysUtils, &Assembler.Global, Winapi.Windows, Compiler, Compiler.TicksMeter, Stack;

const
  msvcrt = 'msvcrt.dll';

function putchar(C: AnsiChar): Integer; cdecl; external msvcrt;
function getchar: AnsiChar; cdecl; external msvcrt;

const
  CELLS_MIN = 64;
  CELLS_DEF = 16384;

type
  TBrainFuckCompiler = object(TCompiler)
  private
    FInCount: Integer; // Amount of "." in code (needs for stack cleaning in the epilogue)
    FSrc: string; // Source code of compiled file
    FSrcName: string; // Source code's file name without expansion
    FSrcPos: Integer; // Number of the current compiling command

    FCellsReg: TRegIndex; // Register, that consists pointer to cells array
    FCellStart: Integer; // Starting cell
    FInReg: TRegIndex; // Register, that consists pointer at putchar function
    FOutReg: TRegIndex; // Register, hat consists pointer at getchar function

    FLastCmdsData: array[0..15] of Integer;
    FLastCmds: TStack;
  protected
    FOpt: Boolean;
    FCells: Integer;
  public
    property Optimization: Boolean read FOpt write FOpt;
    property CellsCount: Integer read FCells;
    property SourceCode: string read FSrc;

    procedure WriteMemSet(Reg: TRegIndex; Size: Cardinal; Value: Integer);

    procedure WritePrologue; // BrainFuck binary code start (stack and variables initialization)
    procedure WriteEpilogue; // BrainFuck binary code end (data release and exit)
    procedure WriteIn;
    procedure WriteOut;

    procedure Create;
    // Compiler config initialization via command line parameters
    // Maybe I should use contructor?
    procedure Init;

    // Read source code and create binary data on it's basis.
    procedure CompileCode(const Code: string);
    procedure ExecuteCode;

    function CheckLastCode(const Commands: array of Char): Boolean;

    // Brainfuck Compiled Unit
    procedure SaveAsBCU;
  public
    procedure RaiseException(const Text: string); overload;
    procedure RaiseException(const Fmt: string; const Args: array of const); overload;
  end;

implementation

procedure Print(Value: AnsiChar); register;
begin
  putchar(Value);
end;

function Read: AnsiChar; register;
begin
  Result := getchar;
end;

function TBrainFuckCompiler.CheckLastCode(
  const Commands: array of Char): Boolean;
var
  I, Len: Integer;
  C: Char;
begin
  Len := Length(Commands);

  if FLastCmds.Length >= Len then
    for I := 0 to Len - 1 do
    begin
       C := Char(FLastCmds.Get(FLastCmds.Length - Len + I));

       if C <> Commands[I] then
         Exit(False);
    end;

  Result := True;
end;

procedure TBrainFuckCompiler.Create;
begin
  inherited;

  FBuffer.Create;

  FOpt := False;

  FCells := CELLS_DEF; // Cells array size (inits in prologue)
  FOpt := False; // Optimization
  FInCount := 0; // Amount of "." calls

  FCellsReg := rEbx; // Register that consists pointer to byte cells
  FInReg := rEbp; // Register that consists "." function
  FOutReg := rEdi;

  FSrcPos := 0; // Command number that processes by the interpreter
  FCellStart := 0;

  FillChar(FLastCmdsData[0], SizeOf(FLastCmdsData), 0);
  FLastCmds.Create(@FLastCmdsData[0], SizeOf(FLastCmdsData));
end;

procedure TBrainFuckCompiler.ExecuteCode;
type
  TFuckback = procedure; // callback
begin
  if IsDebuggerPresent then
    asm int 3 end;

  TFuckback(FBuffer.Data)();
end;

procedure TBrainFuckCompiler.Init;
var
  Param, Value: string;
  Line: string;
  I, J, L: Integer;
  F: TextFile;
begin
  if ParamCount > 1 then
  begin
    for I := 1 to ParamCount do
    begin
      Param := LowerCase(ParamStr(I));
      Value := LowerCase(ParamStr(Succ(I)));

      if Param = '-file' then
      begin
        FSrcName := ChangeFileExt(Value, '');

        if IsRelativePath(Value) then
          Value := Format('%s\%s', [ExtractFileDir(ParamStr(0)), Value]);

        AssignFile(F, Value);

        {$I-}
        Reset(F);
        if IOResult <> 0 then
          RaiseLastOSError;
        {$I+}

        FSrc := '';

        while not Eof(F) do
        begin
          ReadLn(F, Line);

          J := Pos('#', Line);
          if J <> 0 then
            Line := Copy(Line, 1, J - 1);

          FSrc := FSrc + Line;
        end;

        CloseFile(F);
      end;

      if Param = '-o' then
        FOpt := True;

      if Param = '-cells' then
      begin
        if (not TryStrToInt(Value, L)) or (L < CELLS_MIN) then
          FCells := CELLS_DEF
        else
          FCells := L;
      end;

      if Param = '-begin' then
      begin
        if Value = 'center' then
          FCellStart := FCells div 2
        else
        if Value = 'left' then
          FCellStart := 0
        else
        if Value = 'right' then
          FCellStart := FCells
        else
          FCellStart := StrToIntDef(Value, 0);
      end;
    end;
  end;
end;

function Relative(FAddr, SAddr: Pointer): Pointer; inline;
begin
  Result := Pointer(Integer(FAddr) - Integer(SAddr) - 5);
end;

procedure TBrainFuckCompiler.CompileCode(const Code: string);
var
  JumpAddr, P2: Pointer;
  P: PChar;
  C: Char;
  I: Integer;

  ArrStack: TStack;
  ArrData: array[0..255] of Integer;

  Cells: array of Byte;
  CellsPtr: Integer;

  Ticks: TTicksMeter;
begin
  Ticks.Start;

  // Create function prologue (stack, registers, ...).
  WritePrologue;
  // Write pointer to "Code" first symbol for easier way to work with it
  P := PChar(Code);
  // Create a stack array of addresses. Needs to work with cycles.
  ArrStack.Create(@ArrData[0], SizeOf(ArrData));

  SetLength(Cells, FCells);
  CellsPtr := FCellStart;

  while P^ <> #0 do
  begin
    {$REGION 'No Optimization'}
    if not FOpt then
    begin
      C := P^;

      case C of
        '>': WriteInc(FCellsReg);
        '<': WriteDec(FCellsReg);
        '+': WriteIncMem(FCellsReg);
        '-': WriteDecMem(FCellsReg);
        '.': WriteIn;
        ',': WriteOut;
        '[': RaiseException('"No optimization" mode does not support loops.');
        ']':
          if ArrStack.Length = 0 then
            RaiseException('Loop is not initialized by "%s" command.', ['[']);
        '@': Break;
      end;
    end
    else
    {$ENDREGION}
    begin
      Inc(FSrcPos);

      C := P^;

      // Commands that can be compressed into a single operand
      if CharInSet(C, ['>', '<', '+', '-']) then
      begin
        I := 0;
        repeat
          Inc(P);

          // Skip symbols that do not affect anything
          while CharInSet(P^, [' ', #9, #10, #13]) do Inc(P);

          Inc(I);
        until (P^ <> C) or (P^ = #0);

        case C of
          '>':
          begin
            FLastCmds.Push(Integer(C));
            if I > 1 then
            begin
              WriteAdd(FCellsReg, I);
              Inc(CellsPtr, I);
            end
            else
            begin
              WriteInc(FCellsReg);
              Inc(CellsPtr);
            end;
          end;

          '<':
          if I > 1 then
          begin
            WriteSub(FCellsReg, I);
            Dec(CellsPtr, I);
          end
          else
          begin
            WriteDec(FCellsReg);
            Dec(CellsPtr);
          end;

          '+':
          if I > 1 then
          begin
            WriteAddMem(FCellsReg, I);
            Inc(Cells[CellsPtr], I);
          end
          else
          begin
            WriteIncMem(FCellsReg);
            Inc(Cells[CellsPtr]);
          end;

          '-':
          if I > 1 then
          begin
            WriteSubMem(FCellsReg, I);
            Dec(Cells[CellsPtr], I);
          end
          else
          begin
            WriteDecMem(FCellsReg);
            Dec(Cells[CellsPtr]);
          end;
        end;

        FLastCmds.Push(Integer(C));
        Continue;
      end;

      case C of
        '.': WriteIn;
        ',': WriteOut;
        '[':
        begin
          WriteCmpMem(FCellsReg, 0);

          { Create "jz" instruction manually }
          FBuffer.Write<Byte>($0F);
          FBuffer.Write<Byte>($80 or (4));
          FBuffer.Write<Cardinal>(0);

          ArrStack.Push(IP);
        end;
        ']':
        begin
          if ArrStack.Length = 0 then
            RaiseException('Loop is not initialized by "%s" command.', ['[']);

          P2 := Pointer(ArrStack.Pop);

          WriteCmpMem(FCellsReg, 0);
          WriteJump(jtJnz, Pointer(Integer(P2)));

          JumpAddr := Relative(IP, Pointer(Integer(P2) - 5));

          PPointer(Integer(P2) - 4)^ := JumpAddr;
        end;
      end;
    end;

    FLastCmds.Push(Integer(C));

    if CheckLastCode(['[', '-', ']']) then
    begin
      FBuffer.Seek(-16); // replace this with something more appropriate

      if Byte(Cells[CellsPtr] + 1) <> 0 then
        WriteMovMem(FCellsReg, 0);
    end;

    Inc(P);
  end;

  Ticks.Stop;

  WriteLn('Compiled successfully. Code size: ', FBuffer.Position, ' bytes.');
  WriteLn(Ticks.ToString);
  WriteLn;

  WriteEpilogue;
end;

procedure TBrainFuckCompiler.RaiseException(const Text: string);
begin
  inherited RaiseException('[Fatal Error] %s(%d): %s', [FSrcName, FSrcPos, Text]);
end;

procedure TBrainFuckCompiler.RaiseException(const Fmt: string;
  const Args: array of const);
var
  Text: string;
begin
  Text := Format(Fmt, Args);
  Text := Format('[Fatal Error] %s(%d): %s', [FSrcName, FSrcPos, Text]);
  inherited RaiseException(Text);
end;

procedure TBrainFuckCompiler.SaveAsBCU;
var
  S: string;
begin
  S := Format('%s.bcu', [FSrcName]);
  FBuffer.SaveToFile(S);
end;

procedure TBrainFuckCompiler.WriteEpilogue;
begin
  WriteAdd(rEsp, FCells {+ FInCount * 4});

  if FOpt then
  begin
    WritePop(FInReg);
    WritePop(FOutReg);
  end;

 WritePop(FCellsReg);

 WriteRet;
end;

procedure TBrainFuckCompiler.WriteIn;
begin
  // mov eax, [edi]
  FBuffer.Write<Byte>($8B);
  FBuffer.Write<Byte>(Byte(FCellsReg));

  // push eax // WriteToStack: PInteger(edi)^
  WritePush(rEax);

  if FOpt then
  begin
    // call ebx
    WriteCall(FInReg);
    WritePop(rEcx);
  end
  else
  begin
    // P := GetProcAddress(GetModuleHandle(msvcrt), 'putchar');
    WriteCall(@Print);
  end;

  Inc(FInCount);
end;

procedure TBrainFuckCompiler.WriteMemSet(Reg: TRegIndex; Size: Cardinal; Value: Integer);
var
  Divisible: Boolean;
begin
  if Size <= 0 then
    Exit;

  // Reserve edi register
  WritePush(rEdi);

  //  A little optimization. If we need to set register as 0, we do it via "xor" command
  if Value = 0 then
    WriteXor(rEax, rEax)
  else
    WriteMov(rEax, Value);

  // We need "lea" here to avoud spoil the saved value in edi
  WriteLea(rEdi, Reg, 4);
  // Write amount of cells for zeroing
  WriteMov(rEcx, Size);
  // Divide by 4, because we are going to use stosd for zeroing
  WriteShr(rEcx, 2);

  // Divisible - is a variable, that contains result of checking of the
  // possibility of division by four.
  Divisible := Size mod 4 = 0;
  if not Divisible then
  begin
    WriteMov(rEdx, rEcx);
    WriteAnd(rEdx, 3);
  end;

  WriteStoS(cpRepNZ, msDWord);

  if not Divisible then
  begin
    WriteMov(rEcx, rEdx);
    WriteStoS(cpRepNZ, msByte);
  end;

  WritePop(rEdi);
end;

procedure TBrainFuckCompiler.WriteOut;
begin
  WriteCall(FOutReg);
  FBuffer.Write<Byte>($88);
  FBuffer.Write<Byte>(Byte(FCellsReg));
end;

procedure TBrainFuckCompiler.WritePrologue;
begin
  // Reserve for pointer at cells
  WritePush(FCellsReg);

  if FOpt then
  begin
    WritePush(FOutReg);
    WritePush(FInReg);
    WriteMov(FInReg, Cardinal(@putchar)); // reg = @printf
    WriteMov(FOutReg, Cardinal(@getchar));
  end;

  // Create CellsCount cells
  WriteSub(rEsp, FCells);
  // Fill cells with 0
  WriteMemSet(rEsp, FCells, 0);

  // Write pointer at memory cells
  WriteLea(FCellsReg, rEsp, FCellStart);
end;

end.

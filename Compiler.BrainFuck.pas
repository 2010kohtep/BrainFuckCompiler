unit Compiler.BrainFuck;

{$WARN WIDECHAR_REDUCED OFF} // Because CharInSet() is too slow for me

interface

uses
  System.SysUtils, Compiler, &Assembler.Global, Winapi.Windows, TicksMeter, Stack;

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
    FInReg: TRegIndex; // Register, that consists pointer at getchar function
    FOutReg: TRegIndex; // Register, that consists pointer at putchar function

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

    procedure LoadFromFile(FileName: string);

    // Read source code and create binary data on it's basis.
    procedure CompileCode;
    procedure ExecuteCode;

    function CheckCode(const Cmds: string): Boolean;

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

function TBrainFuckCompiler.CheckCode(const Cmds: string): Boolean;
begin
  Result := StrLIComp(PChar(@FSrc[FSrcPos + 1]), PChar(Cmds), Length(Cmds)) = 0;
end;

procedure TBrainFuckCompiler.Create;
begin
  inherited;

  // I believe that 1048576 is enough bytes for compiled code
  FBuffer.Create(1024 * 1024);

  FOpt := False;

  FCells := CELLS_DEF; // Cells array size (inits in prologue)
  FOpt := False; // Optimization
  FInCount := 0; // Amount of "." calls

  FCellsReg := rEbx; // Register that consists pointer to byte cells
  FInReg := rEbp; // Register that consists "." function
  FOutReg := rEdi; // Register that consists "," function

  FSrcPos := 0; // Command number that processes by the interpreter
  FCellStart := 0;

  FillChar(FLastCmdsData[0], SizeOf(FLastCmdsData), 0);
  FLastCmds.Create(@FLastCmdsData[0], SizeOf(FLastCmdsData));
end;

procedure TBrainFuckCompiler.ExecuteCode;
type
  TFuckback = procedure; // callback
begin
  TFuckback(FBuffer.Data)();
end;

procedure TBrainFuckCompiler.Init;
var
  Value: string;
begin
  if FindCmdLineSwitch('F', Value) then
    LoadFromFile(Value);

  if FindCmdLineSwitch('O') then
    FOpt := True;

  if FindCmdLineSwitch('C', Value) then
  begin
    FCells := StrToIntDef(Value, CELLS_DEF);

    if FCells < CELLS_MIN then
      FCells := CELLS_DEF;
  end;

  if FindCmdLineSwitch('B', Value) then
  begin
    if StrIComp(PChar(Value), 'CENTER') = 0 then
      FCellStart := FCells div 2
    else
    if StrIComp(PChar(Value), 'LEFT') = 0 then
      FCellStart := 0
    else
    if StrIComp(PChar(Value), 'RIGHT') = 0 then
      FCellStart := FCells
    else
      FCellStart := StrToIntDef(Value, 0);
  end;

  if FindCmdLineSwitch('T', Value) then
  begin
    if StrIComp(PChar(Value), 'WIN32') = 0 then
      FTarget := tWin32
    else
    if StrIComp(PChar(Value), 'WIN64') = 0 then
      FTarget := tWin64
    else
    if StrIComp(PChar(Value), 'LINUX') = 0 then
      FTarget := tLinux
    else
      raise Exception.Create('TBrainFuckCompiler.Init: Unknown target.');
  end;
end;

procedure TBrainFuckCompiler.LoadFromFile(FileName: string);
var
  Line: string;
  F: TextFile;
  J: Integer;
begin
  FSrcName := ChangeFileExt(FileName, '');

  if IsRelativePath(FileName) then
    FileName := Format('%s\%s', [ExtractFileDir(ParamStr(0)), FileName]);

  AssignFile(F, FileName);

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

function Relative(FAddr, SAddr: Pointer): Pointer; inline;
begin
  Result := Pointer(Integer(FAddr) - Integer(SAddr) - 5);
end;

procedure TBrainFuckCompiler.CompileCode;
var
  JmpBegin: Pointer;
  P: PChar;
  C: Char;
  CmdCount: Integer;

  LoopStack: TStack;
  LoopData: array[0..255] of Integer;

  Cells: array of Byte;
  CellsPtr: Integer;

  Ticks: TTicksMeter;
begin
  Ticks.Start;

  // Create function prologue (stack, registers, ...).
  WritePrologue;
  // Write pointer to "Code" first symbol for easier way to work with it
  P := PChar(FSrc);
  // Create a stack array of addresses. Needs to work with cycles.
  LoopStack.Create(@LoopData[0], SizeOf(LoopData));

  SetLength(Cells, FCells);
  CellsPtr := FCellStart;

  if not FOpt then
  begin
    while P^ <> #0 do
    begin
      C := P^;

      case C of
        '>': WriteInc(FCellsReg);
        '<': WriteDec(FCellsReg);
        '+': WriteIncMem(FCellsReg);
        '-': WriteDecMem(FCellsReg);
        '.': WriteIn;
        ',': WriteOut;
        '[':
        begin
          WriteCmpMem(FCellsReg, 0);
          WriteJump(jtJz, Pointer($AAAAAAAA));
          LoopStack.Push<Pointer>(IP);
        end;
        ']':
        begin
          if LoopStack.Length = 0 then
            RaiseException('Loop is not initialized by "%s" command.', ['[']);

          JmpBegin := Pointer(LoopStack.Pop);

          if not (Char(FLastCmds.GetLast) in ['+', '-']) then
            WriteCmpMem(FCellsReg, 0);

          WriteJump(jtJnz, JmpBegin);
          PPointer(@PByte(JmpBegin)[-4])^ := Relative(IP, @PByte(JmpBegin)[-5]);
        end;
      end;

      Inc(P);
    end;
  end
  else
  begin
    while P^ <> #0 do
    begin
      if CheckCode('[-]') or CheckCode('[+]') then
      begin
        WriteMovMem(FCellsReg, 0);

        FLastCmds.Push<Char>('[');
        FLastCmds.Push<Char>(P[1]);
        FLastCmds.Push<Char>(']');

        Cells[CellsPtr] := 0;

        Inc(P, 3);
        Inc(FSrcPos, 3);
        Continue;
      end;

      C := P^;

//      if StrLIComp(P, '//', 2) = 0 then
//      begin
//        P2 := StrScan(P, #10);
//
//        Inc(FSrcPos, Integer(P2 - P));
//        P := P2 + 1;
//      end;

      // Commands that can be compressed into a single operand
      if C in ['>', '<', '+', '-'] then
      begin
        CmdCount := 0;

        repeat
          Inc(P);

          // Skip symbols that do not affect anything
          while (P^ in [' ', #9, #10, #13]) do Inc(P);

          Inc(CmdCount);
        until (P^ <> C) or (P^ = #0);

        Inc(FSrcPos, CmdCount);

        {$REGION 'Optimizable commands'}
        case C of
          '>':
          begin
            FLastCmds.Push<Char>(C);
            if CmdCount > 1 then
            begin
              WriteAdd(FCellsReg, CmdCount);
              Inc(CellsPtr, CmdCount);
            end
            else
            begin
              WriteInc(FCellsReg);
              Inc(CellsPtr);
            end;
          end;

          '<':
          if CmdCount > 1 then
          begin
            WriteSub(FCellsReg, CmdCount);
            Dec(CellsPtr, CmdCount);
          end
          else
          begin
            WriteDec(FCellsReg);
            Dec(CellsPtr);
          end;

          '+':
          if CmdCount > 1 then
          begin
            WriteAddMem(FCellsReg, CmdCount);
            Inc(Cells[CellsPtr], CmdCount);
          end
          else
          begin
            WriteIncMem(FCellsReg);
            Inc(Cells[CellsPtr]);
          end;

          '-':
          if CmdCount > 1 then
          begin
            WriteSubMem(FCellsReg, CmdCount);
            Dec(Cells[CellsPtr], CmdCount);
          end
          else
          begin
            WriteDecMem(FCellsReg);
            Dec(Cells[CellsPtr]);
          end;
        end;

        FLastCmds.Push<Char>(C);
        Continue;
      end;

      Inc(FSrcPos);

      case C of
        '.': WriteIn;
        ',': WriteOut;
        '[':
        begin
//          if Cells[CellsPtr] = 0 then
//          begin
//            P2 := StrScan(P, ']');
//            if P2 = nil then
//              RaiseException('Cannot find end loop.');
//
//            Inc(FSrcPos, Integer(P2 - P));
//            P := P2 + 1;
//
//            Continue;
//          end;

          // We don't need to create "cmp" instruction because "add", "sub", "inc"
          // and "dec" ones are already set ZF before
          if not (Char(FLastCmds.GetLast) in ['+', '-']) then
            WriteCmpMem(FCellsReg, 0);

          // Create dummy jump which will be contain skip loop address
          WriteJump(jtJz, Pointer($AAAAAAAA));

          LoopStack.Push<Pointer>(IP);
        end;

        ']':
        begin
          if LoopStack.Length = 0 then
            RaiseException('Loop is not initialized by "%s" command.', ['[']);

          JmpBegin := Pointer(LoopStack.Pop);

          if not (Char(FLastCmds.GetLast) in ['+', '-']) then
            WriteCmpMem(FCellsReg, 0);

          WriteJump(jtJnz, JmpBegin);
          PPointer(@PByte(JmpBegin)[-4])^ := Relative(IP, @PByte(JmpBegin)[-5]);
        end;
      end;

      FLastCmds.Push<Char>(C);

      Inc(P);
    end;
  end;

  Ticks.Stop;

  WriteLn('Compiled successfully. Code size: ', FBuffer.Position, ' bytes.');
  WriteLn(Ticks.ToString);
  WriteLn;

  WriteEpilogue;
end;

procedure TBrainFuckCompiler.RaiseException(const Text: string);
begin
  inherited RaiseException('[Fatal Error] %s(%d): %s', [FSrcName, Succ(FSrcPos), Text]);
end;

procedure TBrainFuckCompiler.RaiseException(const Fmt: string;
  const Args: array of const);
var
  Text: string;
begin
  Text := Format(Fmt, Args);
  Text := Format('[Fatal Error] %s(%d): %s', [FSrcName, Succ(FSrcPos), Text]);
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
  WriteAdd(rEsp, FCells);

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
  WriteMov(rEax, FCellsReg, FCellsReg);
  WritePush(rEax);

  if FOpt then
    WriteCall(FInReg)
  else
    WriteCall(@Print);

  WritePop(rEcx);

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
  // If we are under debugger, then we need to create int3 instruction right
  // before compiled brainfuck code.
  if IsDebuggerPresent then
    WriteInt(3);

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

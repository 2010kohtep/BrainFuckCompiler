unit Compiler.BrainFuck;

interface

uses
  System.SysUtils, &Assembler.Global, Winapi.Windows, Compiler, Stack;

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
    FInCount: Integer; // ���-�� "." � ���� (����� ��� ������� ����� � �������)
    FSrc: string; // �������� ��� �������������� �����
    FSrcName: string; // ��� ����� ��������� ���� ��� ����������
    FSrcPos: Integer; // ����� ������� ������� �� ����� ����������

    FCellsReg: TRegIndex; // �������, � ������� �������� ��������� �� ������ �����
    FCellStart: Integer; // ������, � ������� ������� ������
    FInReg: TRegIndex; // �������, � ������� �������� ��������� �� ������� putchar
    FOutReg: TRegIndex; // �������, � ������� �������� ��������� �� ������� getchar

    FIgnoreUnknownCmds: Boolean;

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

    procedure WritePrologue; // ������ ���� BrainFuck (������������� ����� � ����������)
    procedure WriteEpilogue; // ��������� ������ ���� (������������ ������ � �����)
    procedure WriteIn;
    procedure WriteOut;

    procedure Create;
    // ������������� �������� ���������� ����� ��������� �������
    // �����, ������� �� �� ��������������� constructor?
    procedure Init;

    // ������ ��������� ���� � �������� �������� ������ �� ��� ������
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

  FCells := CELLS_DEF; // ������ ������� ����� (������������� ���������� � �������)
  FOpt := False; // �����������
  FInCount := 0; // ���������� ������� "."

  FCellsReg := rEbx; // ������� �������� ��������� �� ������ �����
  FInReg := rEbp; // ������� �������� ������� "."
  FOutReg := rEdi;

  FSrcPos := 0; // ����� �������������� ��������������� �������
  FCellStart := 0;
  FIgnoreUnknownCmds := False;

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

      if Param = '-i' then
        FIgnoreUnknownCmds := True;
    end;
  end;
end;

function Relative(FAddr, SAddr: Pointer): Pointer;
begin
  Result := Pointer(Integer(FAddr) - Integer(SAddr) - 5);
end;

function RDTSC: Int64;
asm
  rdtsc
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

  Start: Int64;
begin
  Start := RDTSC;

  // �������� ������� ������� (������������� �����, ���������, ...).
  WritePrologue;
  // �������� ��������� �� ������ ������ "Code" ��� ����� ������� ������ � ���������.
  P := PChar(Code);
  // �������� ��������� ������� ��� �������. ��������� ��� ������ � �������.
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
        '[':
        begin
        end;
        ']':
        begin
          if ArrStack.Length = 0 then
            RaiseException('Loop is not initialized by "%s" command.', ['[']);
        end;
        '@': Break;
        ' ', #9, #10, #13: ;
      else
        if FIgnoreUnknownCmds then
          WriteLn('[DEBUG] Ignored unknown command: "', C, '".')
        else
          RaiseException('Unknown syntax command: "%s".', [C]);
      end;
    end
    else
    {$ENDREGION}
    begin
      Inc(FSrcPos);

      C := P^;

      // �������, ������� ����� ����� � ���� �������
      if CharInSet(C, ['>', '<', '+', '-']) then
      begin
        I := 0;
        repeat
          Inc(P);

          // ���������� �������, �� �������� �� �� ���
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

          { ������ �������� ���������� "jz" }
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

  WriteLn('Compiled successfully. Code size: ', FBuffer.Position, ' bytes.');
  WriteLn('Cycles spent: ', RDTSC - Start, '.');
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

{$REGION '������ ��������'}
//  WriteXor(rEax, rEax);
//  WriteMov(rEcx, Size);
//  WriteMov(rEdx, Reg);
//  Addr := IP;
//
//  WriteMovMem(rEdx, rEax);
//  WriteInc(rEdx);
//  WriteDec(rEcx);
//  WriteJump(jtJnz, Addr);
{$ENDREGION}

  // �������������� �������� edi
  WritePush(rEdi);

  // ��������� �����������. ���� ����� ���������� 0 � �������, �� ������ ��� �
  // ������� ������� xor.
  if Value = 0 then
    WriteXor(rEax, rEax)
  else
    WriteMov(rEax, Value);

  // Lea ����� ��� ����, ����� �� ��������� ���������� �������� �������� edi.
  WriteLea(rEdi, Reg, 4);
  // �������� ���������� ����� ��� ���������
  WriteMov(rEcx, Size);
  // ��������� �� 4, ��� ��� �� ����� ������������ stosd ��� ���������.
  WriteShr(rEcx, 2);

  // Divisible - ��� ����������, ������� ������ � ���� ��������� �������� ����������
  // Size �� ��������� ������. ���� ��� �� ������, �� ����� �������� ���������
  // ���������� �����.
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
  // ��������������� ��� ��������� �� ������ ������
  WritePush(FCellsReg);

  if FOpt then
  begin
    WritePush(FOutReg);
    WritePush(FInReg);
    WriteMov(FInReg, Cardinal(@putchar)); // reg = @printf
    WriteMov(FOutReg, Cardinal(@getchar));
  end;

  // ������� CellsCount ����� ��� ������ BrainFuck
  WriteSub(rEsp, FCells);
  // ��������� ������ ������
  WriteMemSet(rEsp, FCells, 0);

  // �������� ��������� �� ������ ������
  // WriteMov(FCellsReg, rEsp);
  WriteLea(FCellsReg, rEsp, FCellStart);
end;

end.

program bfc;

{$M 16777216, 16777216}

{$APPTYPE CONSOLE}

{$REGION 'Minimise Size'}

  (* WEAKLINKRTTI:
       �� ��������� - OFF
       ������ ������ �� ������� - ������ �� ���������� � �������� ���,
       ������� RTTI �� ����� ����� � �������� ������ � �������� �������. *)

  {$WEAKLINKRTTI ON}

  (* ������������ ���������� ����������� RTTI ����������, ������ ������������
     ��� ������� � ������� - ��� ��������� ��� RTTI ����� � ��������� ��������
     ����������� RTTI ����������. *)
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

  (* �������� ������ .reloc, ������� �� ����� ��� ����������� ������. *)

  {$SETPEFLAGS 1}

{$ENDREGION}

uses
  Winapi.Windows,
  System.SysUtils,
  &Assembler.Global,
  Console.Final,

  Compiler,
  Compiler.BrainFuck,
  Compiler.TicksMeter,

  Utils,
  Stack,
  Buffer;

var
  Compiler: TBrainFuckCompiler;

procedure Init;
label
  A;
begin
  Compiler.Create;

  WriteLn('Brainfuck Compiler Version 1.0.0');
  WriteLn;

  if ParamCount = 0 then
  begin;
A:  WriteLn('Syntax: bfc [options]');
    WriteLn;
    PrintSwitchValue('-file <str>', 'Source file');
    PrintSwitchValue('-target <str>', 'Target platform (win32, win64, linux)');
    PrintSwitchValue('-cells <int>', 'Elements count');
    PrintSwitchValue('-begin <int>', 'Start cell position');
    PrintSwitchValue('-o', 'Optimization');
    ReadLn;
    Exit;
  end
  else
  begin
    Compiler.Init;

    if Compiler.SourceCode = '' then
      goto A;
  end;

  Compiler.CompileCode(Compiler.SourceCode);
  Compiler.ExecuteCode;
end;

begin
  Init;
end.

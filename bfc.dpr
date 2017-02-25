program bfc;

{$M 16777216, 16777216}

{$APPTYPE CONSOLE}

(* WEAKLINKRTTI:
     Default - OFF
     It affects only the linking - methods are not included in the binary code,
     that's why RTTI can not find and include methods in realtime. *)
{$WEAKLINKRTTI ON}

(*  Controls the amount of extended RTTI information, which generated
    for classes and records - it disables all RTTI options and disables creation
    of extended RTTI information. *)
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

(* Remove .reloc section, which is not needed for executable files. *)
{$SETPEFLAGS 1}

uses
  Winapi.Windows,
  System.SysUtils,
  &Assembler.Global,
  Console.Final,

  Compiler,
  Compiler.BrainFuck,

  TicksMeter,
  Utils,
  Stack,
  Buffer;

const
  PROJNAME = 'Brainfuck Compiler';
  PROJVER = 'Indev';

var
  Compiler: TBrainFuckCompiler;

procedure Init;
begin
  // To do list:

  // - Cell offset to avoid use add/sub/inc/dec

  Compiler.Create;

  SetConsoleTitle(PROJNAME);
  WriteLn(PROJNAME, ' version ', PROJVER);
  WriteLn('Copyright (c) 2017 Alexander B.');
  WriteLn;

  if ParamCount = 0 then
  begin
    WriteLn('Syntax: bfc [options]');
    WriteLn;
    PrintSwitchValue('-F <str>', 'Source file.');
    PrintSwitchValue('-T <str>', 'Target platform (win32, win64, linux).');
    PrintSwitchValue('-C <int>', 'Cells count.');
    PrintSwitchValue('-B <int>', 'Start cell position.');
    PrintSwitchValue('-O', 'Enable code optimization.');
    Exit;
  end
  else
  begin
    Compiler.Init;

    if Compiler.SourceCode = '' then
    begin
      WriteLn('Cannot continue, source code is not loaded.');
      Exit;
    end;
  end;

  Compiler.CompileCode;
  Compiler.ExecuteCode;
end;

begin
  Init;
end.

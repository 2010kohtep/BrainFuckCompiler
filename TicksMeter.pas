unit TicksMeter;

interface

uses
  System.SysUtils;

type
  TTicksMeter = object
  strict private
    FStartTime: Int64;
    FSpentTime: Int64;

    class function RDTSC: Int64; static;
  public
    property Spent: Int64 read FSpentTime;

    procedure Start; inline;
    procedure Stop; inline;

    function ToString: string; inline;
  end;

implementation

{ TTicksMeter }

class function TTicksMeter.RDTSC: Int64;
asm
  rdtsc
end;

procedure TTicksMeter.Start;
begin
  FStartTime := RDTSC;
end;

procedure TTicksMeter.Stop;
begin
  FSpentTime := RDTSC - FStartTime;
end;

function TTicksMeter.ToString: string;
begin
  Result := Format('Cycles spent: %d.', [FSpentTime]);
end;

end.

unit Utils;

interface

procedure PrintSwitchValue(const Switch, Value: string); inline;

implementation

procedure PrintSwitchValue(const Switch, Value: string); inline;
begin
  WriteLn(#9, Switch, #9, Value);
end;

end.

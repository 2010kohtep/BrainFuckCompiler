unit Stack;

interface

type
  TIntegers = array of Integer;

  TStack = object
  protected
    FStack: Pointer;
    FSize: Integer;
    FLength: Integer;
  public
    property Length: Integer read FLength;

    procedure Create(Buffer: Pointer; Size: Integer);

    procedure Push(Value: Integer); overload;
    procedure Push(Value: Pointer); overload;
    function Pop: Integer;

    function Get(I: Integer): Integer;
  end;

implementation

procedure TStack.Create(Buffer: Pointer; Size: Integer);
begin
  FStack := Buffer;
  FSize := Size;
  FLength := 0;
end;

function TStack.Get(I: Integer): Integer;
begin
  Result := TIntegers(FStack)[I];
end;

function TStack.Pop: Integer;
begin
  if (FStack <> nil) and (FLength > 0) then
  begin
    Dec(FLength);
    Result := TIntegers(FStack)[FLength];
  end
  else
    Result := -1;
end;

procedure TStack.Push(Value: Pointer);
begin
  Push(Integer(Value));
end;

procedure TStack.Push(Value: Integer);
begin
  if FStack <> nil then
  begin
    if FLength = FSize div SizeOf(Integer) then
    begin
      Move(TIntegers(FStack)[1], TIntegers(FStack)[0], FLength * SizeOf(Integer) - SizeOf(Integer));
      TIntegers(FStack)[FLength - 1] := Value;
    end
    else
    begin
      TIntegers(FStack)[FLength] := Value;
      Inc(FLength);
    end;
  end;
end;

end.

unit Stack;

interface

uses
  System.SysUtils;

type
  TIntegers = array of Integer;

  TStack = object
  strict protected
    FStack: Pointer;
    FSize: Integer;
    FLength: Integer;
  public
    property Length: Integer read FLength;

    procedure Create(Buffer: Pointer; Size: Integer);

    procedure Push<T>(A: T); overload;
    function Pop: Integer;

    function Get(I: Integer): Integer;
  end;

implementation

procedure TStack.Create(Buffer: Pointer; Size: Integer);
begin
  if (Buffer = nil) or (Size <= 0) then
    raise Exception.Create('TStack.Create: Invalid arguments.');

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

procedure TStack.Push<T>(A: T);
var
  Value: Integer;
begin
  if SizeOf(A) > 4 then
    raise Exception.Create('TStack.Push: Data with size more than 4 bytes is not supported.')
  else
  begin
    Value := PInteger(@A)^;

    case SizeOf(A) of
      4: ;
      3: Value := Value and $FFFFFF;
      2: Value := Value and $FFFF;
      1: Value := Value and $FF;
    end;

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
end;

end.

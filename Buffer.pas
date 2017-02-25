unit Buffer;

interface

uses
  System.AnsiStrings, System.SysUtils;

const
  BUF_MINSIZE = 64;
  BUF_REALLOCSIZE = 64;

type
  TBuffer = object
  protected
    FAutoExtention: Boolean;
    FData: PByte;
    FSize: Integer;
    FOffset: Integer;
  public
    property Data: PByte read FData;
    property Position: Integer read FOffset;

    constructor Create(Size: Integer);
    destructor Destroy;

    procedure Seek(Size: Integer);
    procedure CheckBounds(IncomingSize: Integer);

    procedure Write(const A; Size: Integer); overload;
    procedure Write<T>(A: T); overload;

    procedure SaveToFile(const FileName: string);
  end;

implementation

{ TBuffer }

procedure TBuffer.CheckBounds(IncomingSize: Integer);
begin
  if FOffset + IncomingSize > FSize then
  begin
    if FAutoExtention then
    begin
      Inc(FSize, IncomingSize + BUF_REALLOCSIZE);
      ReallocMem(FData, FSize);
    end
    else
      raise Exception.Create('TBuffer: Overflowed.');
  end;
end;

constructor TBuffer.Create(Size: Integer);
begin
  if Size = 0 then
  begin
    GetMem(FData, BUF_MINSIZE);
    FSize := BUF_MINSIZE;
    FAutoExtention := True;
  end
  else
  begin
    GetMem(FData, Size);
    FSize := Size;
    FAutoExtention := False;
  end;

  FOffset := 0;
end;

destructor TBuffer.Destroy;
begin
  FreeMem(FData);
end;

procedure TBuffer.SaveToFile(const FileName: string);
var
  F: File;
begin
  if (FData = nil) or (FOffset <= 0) then
    Exit;

  AssignFile(F, FileName);
  ReWrite(F, 1);
  BlockWrite(F, FData^, FOffset);
  CloseFile(F);
end;

procedure TBuffer.Seek(Size: Integer);
begin
  Inc(FOffset, Size);
end;

procedure TBuffer.Write(const A; Size: Integer);
begin
  Move(A, PByte(Integer(FData) + FOffset)^, Size);
  Inc(FOffset, Size);
end;

procedure TBuffer.Write<T>(A: T);
var
  Len: Integer;
  PValue: Pointer;
begin
  PValue := PPointer(@A)^;

  if IsManagedType(A) then // interface, string or dynamic array
  begin
    Len := PInteger(Integer(PValue) - SizeOf(Integer))^;
    CheckBounds(Len);
    Write(PValue^, Len);
  end
  else
  if TypeInfo(T) = TypeInfo(PAnsiChar) then
  begin
    Len := System.AnsiStrings.StrLen(PAnsiChar(PValue));
    CheckBounds(Len);
    Write(PValue^, Len);
  end
  else
  if TypeInfo(T) = TypeInfo(PWideChar) then
  begin
    Len := StrLen(PWideChar(PValue));
    CheckBounds(Len);
    Write(PValue^, Len);
  end
  else
  begin
    CheckBounds(SizeOf(A));
    Write(A, SizeOf(A));
  end;
end;

end.

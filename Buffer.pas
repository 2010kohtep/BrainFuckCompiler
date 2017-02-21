unit Buffer;

interface

const
  BUF_MINSIZE = 64;
  BUF_REALLOCSIZE = 64;

type
  TBytes = array of Byte;

  TBuffer = object
  protected
    FData: Pointer;
    FSize: Integer;
    FOffset: Integer;
  public
    property Data: Pointer read FData;
    property Position: Integer read FOffset;

    constructor Create;
    destructor Destroy;

    procedure Seek(Size: Integer);

    procedure Write(const A; Size: Integer); overload;
    procedure Write<T>(A: T); overload;

    procedure SaveToFile(const FileName: string);
  end;

implementation

{ TBuffer }

constructor TBuffer.Create;
begin
  GetMem(FData, BUF_MINSIZE);
  FSize := BUF_MINSIZE;
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
begin
  if IsManagedType(A) then // interface, string or dynamic array
  begin
    Len := PInteger(Integer(PPointer(@A)^) - SizeOf(Integer))^;

    if FOffset + Len > FSize then
    begin
      Inc(FSize, Len + BUF_REALLOCSIZE);
      ReallocMem(FData, FSize);
    end;

    Write(PPointer(PPointer(@A)^)^, Len);
  end
  else
  begin
    if FOffset + SizeOf(A) > FSize then
    begin
      Inc(FSize, SizeOf(A) + BUF_REALLOCSIZE);
      ReallocMem(FData, FSize);
    end;

    Write(A, SizeOf(T));
  end;
end;

end.
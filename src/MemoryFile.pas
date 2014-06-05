unit MemoryFile;

interface

uses
    Classes;

type
    tMemoryFile = class(tStream)
    strict private
        FPosition: int64;
        FSize: int64;
        Block: array of pointer;
        BlockSize: int64;

        procedure SetPosition(NewPosition: int64);
        procedure SetSize(NewSize: int64);
        procedure AdjustSizeTo(NewSize: int64);
        procedure InternalRead(var Buf; Len: int64);

    public
        constructor Create; overload;
        constructor Create(ABlockSize: int64); overload;
        destructor Destroy; override;
        property Position: int64 read FPosition write SetPosition;
        function Eof: boolean;
        function Read(var Buffer; Count: Longint): Longint; override;
        procedure Write(var Buf; Len: int64);
        property Size: int64 read FSize write SetSize;
        function GetByte(Position: int64):byte;
    end;


implementation

uses
    MemoryPool,
    SysUtils;

type
    tBlock = array [0 .. MAXINT-1] of byte;
    pBlock = ^tBlock;


procedure tMemoryFile.AdjustSizeTo(NewSize: int64);
// Only ever increases the size upwards
var
    BlocksNeeded: int64;
    OldHighBlock: int64;
    i: integer;
begin
    if NewSize > FSize then
    begin
        BlocksNeeded := (NewSize + BlockSize - 1) div BlockSize;
        if (BlocksNeeded - 1) > High(Block) then
        begin
            OldHighBlock := High(Block);
            SetLength(Block, BlocksNeeded);
            for i := OldHighBlock + 1 to High(Block) do
            begin
                // WriteLn('Allocating block #', i);
                Block[i] := MemPool(BlockSize).Alloc;
            end;
        end;
        FSize := NewSize;
    end;
end;


constructor tMemoryFile.Create;
begin
    Create(1024*1024); // Default BlockSize is 1mb
end;


constructor tMemoryFile.Create(ABlockSize: int64);
begin
    inherited Create;

    FPosition := 0;
    FSize := 0;
    BlockSize:=ABlockSize;
    SetLength(Block, 0);
end;


destructor tMemoryFile.Destroy;
var
    i: integer;
begin
    if Block <> nil then
    begin
        for i := 0 to High(Block) do
        begin
            // WriteLn('Releasing block #', i);
            MemPool(BlockSize).Release(Block[i]);
        end;
        SetLength(Block, 0);
    end;

    inherited;
end;


function tMemoryFile.Eof: boolean;
begin
    Result := FPosition >= FSize;
end;


function tMemoryFile.GetByte(Position: int64): byte;
begin
    if Position>=FSize then
    begin
        raise ERangeError.Create('Read exceeds file-size.');
        exit; // Not sure if "exit" is really necessary. Can't hurt though. :)
    end;

    Result:=tBlock(Block[Position div BlockSize]^)[Position mod BlockSize];
end;


procedure tMemoryFile.SetPosition(NewPosition: int64);
begin
    if NewPosition >= 0 then FPosition := NewPosition;
end;


procedure tMemoryFile.SetSize(NewSize: int64);
begin
    FSize:=NewSize;
end;


function tMemoryFile.Read(var Buffer; Count: Longint): Longint;
begin
    InternalRead(Buffer,Count);
    Result:=Count;
end;


procedure tMemoryFile.InternalRead(var Buf; Len: int64);
var
    ThisBytes: int64;
    ThisOffset: int64;
    p: pointer;
begin
    if Len <= 0 then exit;

    if (FPosition + Len) > FSize then
    begin
        raise ERangeError.Create('Read exceeds file-size.');
        exit; // Not sure if "exit" is really necessary. Can't hurt though. :)
    end;

    p := @Buf;
    // WriteLn('p=', NativeInt(p));
    while Len > 0 do
    begin
        ThisOffset := FPosition mod BlockSize;
        ThisBytes := Len;
        if (BlockSize - ThisOffset) < ThisBytes then ThisBytes := BlockSize - ThisOffset;

        // WriteLn('Reading ', ThisBytes, ' bytes of block #', FPosition div BlockSize, ' with offset ', ThisOffset,
        // ' to @', NativeInt(p));
        Move(tBlock(Block[FPosition div BlockSize]^)[ThisOffset], p^, ThisBytes);

        Dec(Len, ThisBytes);
        Inc(FPosition, ThisBytes);
        Inc(NativeInt(p), ThisBytes);
    end;

end;


procedure tMemoryFile.Write(var Buf; Len: int64);
var
    ThisBytes: int64;
    ThisOffset: int64;
    p: pointer;
begin
    if Len <= 0 then exit;

    // If necessary adjust FSize and allocate more memory
    AdjustSizeTo(FPosition + Len);

    p := @Buf;
    // WriteLn('p=', NativeInt(p));
    while Len > 0 do
    begin
        // WriteLn('FPosition=',FPosition);
        ThisOffset := FPosition mod BlockSize;
        ThisBytes := Len;
        if (BlockSize - ThisOffset) < ThisBytes then ThisBytes := BlockSize - ThisOffset;

        // WriteLn('Writing ', ThisBytes, ' bytes of block #', FPosition div BlockSize, ' with offset ', ThisOffset,
        // ' from @', NativeInt(p));
        Move(p^, tBlock(Block[FPosition div BlockSize]^)[ThisOffset], ThisBytes);

        Dec(Len, ThisBytes);
        Inc(FPosition, ThisBytes);
        Inc(NativeInt(p), ThisBytes);
    end;
end;

end.

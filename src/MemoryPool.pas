unit MemoryPool;

interface

uses
    GlobalTypes,
    SyncObjs;

type
    tMemoryPool = class
        constructor Create(const BlockSize: int32);
        destructor Destroy; override;
        function Alloc: pointer;
        procedure Release(p: pointer);
        function BlockCount: integer;

    strict private
        FBlockSize: int32;
        Block: array of pointer;
        BlockUsed: array of boolean;
        CritSec: tCriticalSection;
    end;


function MemPool(const BlockSize: int32): tMemoryPool;


implementation

uses
    SysUtils,
    Logging;

type
    tMemPool = record
        BlockSize: integer;
        ThisMemoryPool: tMemoryPool;
    end;

var
    Pools: array of tMemPool;
    PoolCS: tCriticalSection;


function MemPool(const BlockSize: int32): tMemoryPool;
var
    i: integer;
begin
    PoolCS.Enter;
    for i := 0 to High(Pools) do
    begin
        if Pools[i].BlockSize = BlockSize then
        begin
            Result := Pools[i].ThisMemoryPool;
            PoolCS.Leave;
            exit;
        end;
    end;

    try
        SetLength(Pools, High(Pools) + 2);
        Pools[High(Pools)].BlockSize := BlockSize;
        Pools[High(Pools)].ThisMemoryPool := tMemoryPool.Create(BlockSize);
        Result := Pools[High(Pools)].ThisMemoryPool;
    finally
        PoolCS.Leave;
    end;
end;


function tMemoryPool.Alloc: pointer;
var
    i: integer;
begin
    CritSec.Enter;
    for i := 0 to High(Block) do
    begin
        if not BlockUsed[i] then
        begin
            BlockUsed[i] := true;
            Result := Block[i];
            // WriteLn('Alloc: reused ',NativeInt(Result));
            CritSec.Leave;
            exit;
        end;
    end;

    SetLength(Block, High(Block) + 2);
    SetLength(BlockUsed, High(BlockUsed) + 2);
    BlockUsed[High(BlockUsed)] := true;
    GetMem(Block[High(Block)], FBlockSize);
    Result := Block[High(Block)];
    // WriteLn('Alloc: new ',NativeInt(Result));
    // LogMsg('robot.log','Alloc block #'+IntToStr(High(Block)+1)+' for blocksize '+IntToStr(FBlockSize));
    CritSec.Leave;
end;


procedure tMemoryPool.Release(p: pointer);
var
    i: integer;
begin
    // WriteLn('Release: ',NativeInt(p));
    CritSec.Enter;
    for i := 0 to High(Block) do
    begin
        if Block[i] = p then
        begin
            BlockUsed[i] := false;
            CritSec.Leave;
            exit;
        end;
    end;

    CritSec.Leave;
    LogMsg('robot.log', 'Release failed for blocksize=' + IntToStr(FBlockSize));
    raise ERangeError.Create('Trying to release unknown memory-block');
end;


function tMemoryPool.BlockCount: integer;
begin
    Result := High(Block) + 1;
end;


constructor tMemoryPool.Create(const BlockSize: int32);
begin
    FBlockSize := BlockSize;
    SetLength(Block, 0);
    SetLength(BlockUsed, 0);
    CritSec := tCriticalSection.Create;
end;


destructor tMemoryPool.Destroy;
var
    i: integer;
begin
    for i := 0 to High(Block) do
        FreeMem(Block[i]);

    CritSec.Free;

    inherited;
end;


initialization

begin
    PoolCS := tCriticalSection.Create;
end;

end.

// ---------------------------------------------------------------------------------------
// Copyright(c) 2025 Jens Kallup
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy of this
// software and associated documentation files(the "Software"), to deal in the Software
// without restriction, including without limitation the rights to use, copy, modify,
// merge, publish, distribute, sublicense, and /or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to the following
// conditions :
// 
// The above copyright notice and this permission notice shall be included in all copies
// or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
// CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
// OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// ---------------------------------------------------------------------------------------

{$if defined(codeh) and defined(codei)}
{$FATAL interface and implementation at the same time not possible}
{$endif}

{$ifdef codeh}

type
  TObject = class;
  IUnknown = interface;
  TClass  = class of tobject;
  PClass  = ^TClass;     
  TextFile = Text;

{ ****************************************
  TInterfaceEntry
  **************************************** }

  // This enumerate is found both in the rtl and compiler. Do not change the order of the fields.
  TInterfaceEntryType = (
    etStandard,
    etVirtualMethodResult,
    etStaticMethodResult,
    etFieldValue,
    etVirtualMethodClass,
    etStaticMethodClass,
    etFieldValueClass
  );

  PInterfaceEntry = ^TInterfaceEntry;
  TInterfaceEntry = record
  private
    function GetIID: PGUID; inline;
    function GetIIDStr: pshortstring; inline;
  public
    property IID: PGUID read GetIID;
    property IIDStr: pshortstring read GetIIDStr;
  public
    IIDRef: ^PGUID; { if assigned(IID) then Com else Corba}
    VTable: Pointer;
    case integer of
      1: (
        IOffset: sizeuint;
      );
      2: (
        IOffsetAsCodePtr: CodePointer;
        IIDStrRef: ^pshortstring; { never nil. Com: upper(GuidToString(IID^)) }
        IType: TInterfaceEntryType;
      );
  end;

  PInterfaceTable = ^TInterfaceTable;
  TInterfaceTable = record
    EntryCount: sizeuint;
    Entries: array[0..0] of TInterfaceEntry;
  end;

  { to access the message table from outside }
  PMsgStrTable = ^TMsgStrTable;
  TMsgStrTable = record
    name: pshortstring;
    method: codepointer;
  end;

  pstringmessagetable = ^tstringmessagetable;
  TStringMessageTable = record
    count: longint;
    msgstrtable: array[0..0] of tmsgstrtable;
  end;

{ ****************************************
  TVmt
  **************************************** }

  PVmt  = ^TVmt;
  PPVmt = ^PVmt;
  TVmt  = record
     vInstanceSize:      SizeInt;
     vInstanceSize2:     SizeInt;
     vParentRef:         PPVmt;
     vClassName:         PShortString;
     vDynamicTable:      Pointer;
     vMethodTable:       Pointer;
     vFieldTable:        Pointer;
     vTypeInfo:          Pointer;
     vInitTable:         Pointer;
     vAutoTable:         Pointer;
     vIntfTable:         Pointer;
     vMsgStrPtr:         Pointer;
     vDestroy:           CodePointer;
     vNewInstance:       CodePointer;
     vFreeInstance:      CodePointer;
     vSafeCallException: CodePointer;
     vDefaultHandler:    CodePointer;
     vAfterConstruction: CodePointer;
     vBeforeDestruction: CodePointer;
     vDefaultHandlerStr: CodePointer;
     vDispatch:          CodePointer;
     vDispatchStr:       CodePointer;
     vEquals:            CodePointer;
     vGetHashCode:       CodePointer;
     vToString:          CodePointer;
  private
    function GetvParent: PVmt; inline;
  public
    property vParent: PVmt read GetvParent;
  end;

{ ****************************************
  TObject
  **************************************** }

  TObject = class
  private
  public
    constructor Create;
    { the virtual procedures must be in THAT order }
    destructor Destroy; virtual;
    class function NewInstance: TObject; virtual;
    procedure FreeInstance; virtual;
    function SafeCallException(exceptobject: TObject; exceptaddr: codepointer): HRESULT; virtual;
    procedure Free;
    class function InitInstance(Instance: Pointer): TObject;
    class function ClassType: TClass;
    class function ClassInfo: Pointer;
    class function ClassName: ShortString;
    class function ClassParent: TClass;
    class function InstanceSize: SizeInt;
    class function InheritsFrom(aclass: TClass): Boolean;
    procedure AfterConstruction; virtual;
    procedure BeforeDestruction; virtual;
    { interface functions }
    function GetInterface(const iid: TGUID; out obj): boolean;
    function GetInterfaceWeak(const iid: TGUID; out obj): boolean; // equal to GetInterface but the interface returned is not referenced
    class function GetInterfaceEntry(const iid: TGUID): PInterfaceEntry;
  end;

{ ****************************************
  TInterfacedObject
  **************************************** }

const
  { for safe as operator support }
  IObjectInstance: TGUID = '{D91C9AF4-3C93-420F-A303-BF5BA82BFD23}';

type
  IUnknown = interface
    ['{00000000-0000-0000-C000-000000000046}']
    function QueryInterface(constref iid: TGUID; out obj): LongInt; stdcall;
    function _AddRef: LongInt; stdcall;
    function _Release: LongInt; stdcall;
  end;
  IInterface = IUnknown;

const
  { IInterface }
  S_OK          = 0;
  S_FALSE       = 1;
  E_NOINTERFACE = HRESULT($80004002);
  E_UNEXPECTED  = HRESULT($8000FFFF);
  E_NOTIMPL     = HRESULT($80004001);

type
  TInterfacedObject = class(TObject, IUnknown)
  protected
     FRefCount: LongInt;
     FDestroyCount: LongInt;
     { implement methods of IUnknown }
     function QueryInterface(constref iid: TGUID; out obj): LongInt; stdcall;
     function _AddRef: LongInt; stdcall;
     function _Release: LongInt; stdcall;
   public
     destructor Destroy; override;
     procedure AfterConstruction; override;
     procedure BeforeDestruction; override;
     class function NewInstance: TObject; override;
     property RefCount: LongInt read FRefCount;
  end;
  TInterfacedClass = class of TInterfacedObject;

{ ****************************************
  TMethod
  **************************************** }

  PMethod = ^TMethod;
  TMethod = record
    Code: CodePointer;
    Data: Pointer;
  public
  class operator =(const aLeft, aRight: TMethod): Boolean; inline;
    class operator <>(const aLeft, aRight: TMethod): Boolean; inline;
    class operator >(const aLeft, aRight: TMethod): Boolean; inline;
    class operator >=(const aLeft, aRight: TMethod): Boolean; inline;
    class operator <(const aLeft, aRight: TMethod): Boolean; inline;
    class operator <=(const aLeft, aRight: TMethod): Boolean; inline;
  end;

{ ****************************************
  funcs
  **************************************** }

procedure fpc_intf_assign(var d: pointer; const s: pointer); compilerproc;
function fpc_class_as_intf(const s: pointer; const iid: TGUID): IInterface; compilerproc;
function fpc_intf_as_class(const s: pointer; const aclass: TClass): pointer; compilerproc;
function fpc_intf_cast(const s: pointer; const iid: TGUID): IInterface; compilerproc;
procedure fpc_intf_decr_ref(var i: pointer); compilerproc;
procedure fpc_intf_incr_ref(i: pointer); compilerproc;

{$endif}

{$ifdef codei}

{ ****************************************
  TVmt
  **************************************** }

function TInterfaceEntry.GetIID: PGUID;
begin
  {$ifdef VER3_0}
  GetIID := IIDRef;
  {$else VER3_0}
  if Assigned(IIDRef) then GetIID := IIDRef^
  else
    GetIID := nil;
  {$endif VER3_0}
end;

function TInterfaceEntry.GetIIDStr: PShortString;
begin
  {$ifdef VER3_0}
  GetIIDStr := IIDStrRef;
  {$else VER3_0}
  if Assigned(IIDStrRef) then GetIIDStr := IIDStrRef^
  else
    GetIIDStr := nil;
  {$endif VER3_0}
end;

{ ****************************************
  TVmt
  **************************************** }

function TVmt.GetvParent: PVMT;
begin
  if vParentRef <> nil then result := vParentRef^ else result := nil;
end;

{ ****************************************
  TObject
  **************************************** }

constructor TObject.Create;
begin
end;

destructor TObject.Destroy;
begin          
  //writeln('TObject.Destroy | ', ClassName, ClassName, ' | memsize = ', MemSize(pointer(self)), ' | ptr = ', ptruint(pointer(self)));
end;

class function TObject.NewInstance: TObject;
var
   p: pointer;
begin
  GetMem(p, InstanceSize);
  //writeln('TObject.NewInstance | ', TClass(self).ClassName, ' | memsize = ', MemSize(p), ' | ptr = ', ptruint(p));
  if p <> nil then InitInstance(p);
  result := TObject(p);
end;

procedure TObject.FreeInstance;
begin
  FreeMem(Pointer(Self));
end;

function TObject.SafeCallException(exceptobject: TObject; exceptaddr: codepointer): HRESULT;
begin
  result := 1;
end;

procedure TObject.Free;
begin                     
  //writeln('TObject.Free | ', ClassName, ClassName, ' | memsize = ', MemSize(pointer(self)), ' | ptr = ', ptruint(pointer(self)));
  if self <> nil then self.Destroy;
end;

procedure InitInterfacePointers(objclass: tclass;instance : pointer);
var
  ovmt: PVmt;
  i: longint;
  intftable: PInterfaceTable;
  Res: PInterfaceEntry;
begin
  ovmt := PVmt(objclass);
  while assigned(ovmt) and {$ifdef VER3_0}(ovmt^.vIntfTable <> @emptyintf){$else}assigned(ovmt^.vIntfTable){$endif} do
    begin
      intftable:=ovmt^.vIntfTable;
      {$ifdef VER3_0}
      if assigned(intftable) then
      {$endif VER3_0}
      begin
        i:=intftable^.EntryCount;
        Res:=@intftable^.Entries[0];
        while i>0 do begin
          if Res^.IType = etStandard then
            ppointer(@(pbyte(instance)[Res^.IOffset]))^:=
              pointer(Res^.VTable);
          inc(Res);
          dec(i);
        end;
      end;
      ovmt:=ovmt^.vParent;
    end;
end;

class function TObject.InitInstance(Instance: Pointer): TObject;
var
  vmt: PVmt;
begin
  FillChar(Instance^, InstanceSize, 0);
  PPointer(Instance)^ := Pointer(self);

  if PVmt(self)^.vIntfTable <> nil then
    InitInterfacePointers(self, Instance);

  //vmt := PVmt(self);
  //if vmt <> nil then begin
  //  if vmt^.vInitTable <> nil then begin
  //    // ???
  //  end;
  //end;

  result := TObject(Instance);
end;

class function TObject.ClassType: TClass;
begin
  result := TClass(Pointer(Self));
end;

class function TObject.ClassInfo: Pointer;
begin
  result := PVmt(Self)^.vTypeInfo;
end;

class function TObject.ClassName: ShortString;
begin
  result := PVmt(Self)^.vClassName^;
end;

class function TObject.ClassParent: TClass;
begin
  result := TClass(PVmt(Self)^.vParent);
end;

class function TObject.InstanceSize: SizeInt;
begin
  result := PVmt(Self)^.vInstanceSize;
end;

class function TObject.InheritsFrom(aclass: TClass): Boolean;
var
  vmt: PVmt;
begin
  if aclass = nil then exit(false);
  vmt := PVmt(Self);
  while (vmt <> nil) and (vmt <> PVmt(aclass)) do vmt := vmt^.vParent;
  result := vmt = PVmt(aclass);
end;

procedure TObject.AfterConstruction;
begin
end;

procedure TObject.BeforeDestruction;
begin
end;

// replace it later to more common place?
function IsGUIDEqual(const guid1, guid2: TGUID): boolean;
begin
  result:=
  (guid1.D1=guid2.D1) and
  (PDWORD(@guid1.D2)^=PDWORD(@guid2.D2)^) and
  (PDWORD(@guid1.D4[0])^=PDWORD(@guid2.D4[0])^) and
  (PDWORD(@guid1.D4[4])^=PDWORD(@guid2.D4[4])^);
  //result := CompareByte(guid1, guid2, sizeof(TGUID)) = 0;
end;

type
  TInterfaceGetter = procedure(out Obj) of object;
  TClassGetter = function: TObject of object;

function GetInterfaceByEntry(Instance: pointer; IEntry: PInterfaceEntry; out obj): boolean;
var
  Getter: TMethod;
begin
  Pointer(Obj) := nil;
  Getter.Data := Instance;
  if Assigned(IEntry) and Assigned(Instance) then case IEntry^.IType of
      etStandard:
        Pointer(Obj) := pbyte(instance) + IEntry^.IOffset;
      etFieldValue, etFieldValueClass:
        Pointer(obj) := PPointer(pbyte(Instance) + IEntry^.IOffset)^;
      etVirtualMethodResult:
      begin
        Getter.code := PCodePointer(pbyte(PPointer(Instance)^) + IEntry^.IOffset)^;
        TInterfaceGetter(Getter)(obj);
      end;
      etVirtualMethodClass:
      begin
        Getter.code := PCodePointer(pbyte(PPointer(Instance)^) + IEntry^.IOffset)^;
        TObject(obj) := TClassGetter(Getter)();
      end;
      etStaticMethodResult:
      begin
        Getter.code := IEntry^.IOffsetAsCodePtr;
        TInterfaceGetter(Getter)(obj);
      end;
      etStaticMethodClass:
      begin
        Getter.code := IEntry^.IOffsetAsCodePtr;
        TObject(obj) := TClassGetter(Getter)();
      end;
    end;
  result := assigned(pointer(obj));
end;

function TObject.GetInterface(const iid: TGUID; out obj): boolean;
var
  IEntry: PInterfaceEntry;
  Instance: TObject;
begin
  if IsGUIDEqual(IObjectInstance, iid) then begin
    TObject(Obj) := Self;
    Result := true;
    Exit;
  end;

  Instance := self;
  repeat
    IEntry := Instance.GetInterfaceEntry(iid);
    result := GetInterfaceByEntry(Instance, IEntry, obj);

    if (not result) or
      (IEntry^.IType in [etStandard, etFieldValue,
      etStaticMethodResult, etVirtualMethodResult]) then Break;

    { if interface is implemented by a class-type property or field, continue search }
    Instance := TObject(obj);
  until false;

  { Getter function will normally AddRef, so adding another reference here will cause memleak. }
  if result and (IEntry^.IType in [etStandard, etFieldValue]) then IInterface(obj)._AddRef;
end;

function TObject.GetInterfaceWeak(const iid: TGUID; out obj): boolean;
var
  IEntry: PInterfaceEntry;
  Instance: TObject;
begin
  if IsGUIDEqual(IObjectInstance, iid) then begin
    TObject(Obj) := Self;
    Result := true;
    Exit;
  end;

  Instance := self;
  repeat
    IEntry := Instance.GetInterfaceEntry(iid);
    result := GetInterfaceByEntry(Instance, IEntry, obj);

    if (not result) or
      (IEntry^.IType in [etStandard, etFieldValue,
      etStaticMethodResult, etVirtualMethodResult]) then Break;

    { if interface is implemented by a class-type property or field, continue search }
    Instance := TObject(obj);
  until false;

  { Getter function will normally AddRef, so we have to release it, else the ref is not weak. }
  if result and not (IEntry^.IType in [etStandard, etFieldValue]) then IInterface(obj)._Release;
end;

class function TObject.GetInterfaceEntry(const iid: TGUID): PInterfaceEntry;
var
  i: longint;
  intftable: PInterfaceTable;
  ovmt: PVmt;
begin
  ovmt := PVmt(Self);
  while Assigned(ovmt) and Assigned(ovmt^.vIntftable) do begin
    intftable := ovmt^.vIntfTable;
    for i := 0 to intftable^.EntryCount-1 do begin
      result := @intftable^.Entries[i];
      if assigned(Result^.iid) and IsGUIDEqual(Result^.iid^, iid) then Exit;
    end;
    ovmt := ovmt^.vParent;
  end;
  result := nil;
end;

{ ****************************************
  TInterfacedObject
  **************************************** }

function TInterfacedObject.QueryInterface(constref iid: TGUID; out obj): LongInt; stdcall;
begin
  if GetInterface(iid, obj) then result := S_OK else result := LongInt(E_NOINTERFACE);
end;

function TInterfacedObject._AddRef: LongInt; stdcall;
begin
  // @@todo: InterlockedDecrement/Increment
  inc(FRefCount);
  result := FRefCount;
end;

function TInterfacedObject._Release: LongInt; stdcall;
begin
  // @@todo: InterlockedDecrement/Increment
  dec(FRefCount);
  result := FRefCount;
  if result = 0 then begin
    // @@todo: that too
    inc(FDestroyCount);
    if FDestroyCount = 1 then begin
      if copy(ClassName, 1, 9) = '$Capturer' then begin
        // we cant destroy capturer object, it crashes
        //writeln('@@todo: fix this');
        exit;
      end;

      self.Destroy;
    end;
  end;
end;

destructor TInterfacedObject.Destroy;
begin
  FRefCount := 0;
  FDestroyCount := 0;
  inherited Destroy;
end;

procedure TInterfacedObject.AfterConstruction;
begin
  // @@todo: thread safety
  // dec by 1 on purpose
  dec(FRefCount);
end;

procedure TInterfacedObject.BeforeDestruction;
begin
  if FRefCount <> 0 then HandleError(204);
end;

class function TInterfacedObject.NewInstance: TObject;
begin
  result := inherited NewInstance;
  if result <> nil then TInterfacedObject(result).FRefCount := 1;
end;

{ ****************************************
  TMethod
  **************************************** }

class operator TMethod.=(const aLeft, aRight: TMethod): Boolean; inline;
begin
  result := (aLeft.Data = aRight.Data) and (aLeft.Code = aRight.Code);
end;

class operator TMethod.<>(const aLeft, aRight: TMethod): Boolean; inline;
begin
  result := (aLeft.Data <> aRight.Data) or (aLeft.Code <> aRight.Code);
end;

class operator TMethod.>(const aLeft, aRight: TMethod): Boolean; inline;
begin
  result := (PtrUInt(aLeft.Data) > PtrUInt(aRight.Data)) or
            ((aLeft.Data = aRight.Data) and (PtrUInt(aLeft.Code) > PtrUInt(aRight.Code)));
end;

class operator TMethod.>=(const aLeft, aRight: TMethod): Boolean; inline;
begin
  result := (aLeft > aRight) or (aLeft = aRight);
end;

class operator TMethod.<(const aLeft, aRight: TMethod): Boolean; inline;
begin
  Result := (PtrUInt(aLeft.Data) < PtrUInt(aRight.Data)) or
            ((aLeft.Data = aRight.Data) and (PtrUInt(aLeft.Code) < PtrUInt(aRight.Code)));
end;

class operator TMethod.<=(const aLeft, aRight: TMethod): Boolean; inline;
begin
  result := (aLeft < aRight) or (aLeft = aRight);
end;

{ ****************************************
  funcs
  **************************************** }

procedure fpc_intf_assign(var d: pointer; const s: pointer); [public, alias: 'FPC_INTF_ASSIGN']; compilerproc;
begin
  if s <> nil then IUnknown(s)._AddRef;
  if d <> nil then IUnknown(d)._Release;
  d := s;
end;

function fpc_class_as_intf(const s: pointer; const iid: TGUID): IInterface; compilerproc;
var
  tmpi: pointer; // _AddRef before _Release
  tmpi2: pointer; // weak!
begin
  if s = nil then exit(nil);
  tmpi := nil;
  tmpi2 := nil;
  if not ((TObject(s).GetInterfaceWeak(IUnknown, tmpi2) and (IUnknown(tmpi2).QueryInterface(iid, tmpi) = S_OK)) or TObject(s).GetInterface(iid, tmpi)) then
    HandleError(219);
  result := nil;
  pointer(result) := tmpi;
end;

function fpc_intf_as_class(const s: pointer; const aclass: TClass): pointer; [public, alias: 'FPC_INTF_AS_CLASS']; compilerproc;
var
  tmpo: tobject;
begin
  if s = nil then exit(nil);
  if not ((IUnknown(s).QueryInterface(IObjectInstance, tmpo) = S_OK) and tmpo.InheritsFrom(aclass)) then
    HandleError(219);
  result := tmpo;
end;

function fpc_intf_cast(const s: pointer; const iid: TGUID): IInterface; [public, alias: 'FPC_INTF_CAST']; compilerproc;
begin
  msgdebug('@@todo: fpc_intf_cast');
end;

procedure fpc_intf_decr_ref(var i: pointer); compilerproc; [public, alias: 'FPC_INTF_DECR_REF'];
begin
  if i <> nil then begin
    IUnknown(i)._Release;
    i := nil;
  end;
end;

//procedure intf_decr_ref(var i: pointer); [external name 'FPC_INTF_DECR_REF'];

procedure fpc_intf_incr_ref(i: pointer); compilerproc; [public, alias: 'FPC_INTF_INCR_REF'];
begin
  if i <> nil then IUnknown(i)._AddRef;
end;

//procedure intf_incr_ref(i: pointer); [external name 'FPC_INTF_INCR_REF'];

{$endif}


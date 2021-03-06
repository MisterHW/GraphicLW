unit DynamicFunctions;

interface
uses
  windows; // MathParser;


// whatever calling convention you choose, be sure to setup the correct
// stack frame or register contents!
// you need to call Tx86gen.subroutine 

type TVar1SingleProc    = procedure( var s : single ); stdcall;
type TVar1SingleObjProc = procedure(sender: TObject; var s : single ); stdcall;
type TVar2SingleProc    = procedure( var s1, s2 : single ); stdcall;
type TVoidProc          = procedure; stdcall;


type
  TAsmCommand = record
    cmd, op1, op2 : string[32];
  end;

  TSymbolReference = record
    identifier: string[32];
    ReferenceType: Cardinal; // SOMBOLREF_x
    ptr: Array [0..1] of Pointer;
  end;

  TAsmCode = record
    commands  : Array of TAsmCommand;
    references: Array of TSymbolReference;
  end;



const
  ERR_X86GEN_NO_ERROR        = $0;
  ERR_X86GEN_INVALID_CODE    = $1;
  ERR_X86GEN_STACK_VIOLATION = $2;
  ERR_X86GEN_WRONG_OPTYPE    = $4;
  ERR_X86GEN_UNKNOWN_COMMAND = $8;

  SYMBOLREF_PTYPE             = $0; // ptr[0] is the address of the referenced value,
                                    // e.g. ptr[0] := @x;
  SYMBOLREF_GLOBALPROCEDURE   = $1; // ptr[0] is the procedural pointer,
                                    // e.g. ptr[0] := @SomeProcedure;
  SYMBOLREF_PROCEDUREOFOBJECT = $2; // ptr[1] is the class procedure pointer, ptr[0] is the instance pointer (mandatory),
                                    // e.g. ptr[0] := Addr(Dummy1); ptr[1] := Addr(TDummy.ObjectProcedure);


type Tx86gen = class
    constructor Create();
    destructor Destroy();
  private
    _subroutine : Pointer;
    buffer : Pointer;
    buffersize: Cardinal;
    _errstat    : Cardinal;

  public
    // procedure InsertProcedureReference(Name: string; Address: Pointer; Convention: TCallConvention);
    procedure translate(var src: TAsmCode);
    property subroutine : Pointer read _subroutine;
    property buildstatus: Cardinal read _errstat;
end;



implementation

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////



function MakeExecutableMemory(codedata:Pointer; size: integer):Pointer;
var
  oldprotect: DWord;
  byteswritten: Cardinal;
begin
  result := VirtualAlloc(nil,4096,MEM_COMMIT, PAGE_READWRITE);
  WriteProcessMemory(GetCurrentProcess,result,codedata,size,byteswritten);
  VirtualProtect(result,4096,PAGE_EXECUTE_READ,@oldprotect);
end;



procedure DisposeExecutableMemory(mem: Pointer);
begin
  VirtualFree(mem,4096,MEM_DECOMMIT);
end;


/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////




constructor Tx86gen.Create();
begin
  inherited Create();
  buffersize := 4096;
  getmem(buffer, buffersize);
  _subroutine := MakeExecutableMemory(buffer,buffersize);
end;

destructor Tx86gen.Destroy();
begin
  DisposeExecutableMemory(_subroutine);
  freemem(buffer,buffersize);
  inherited Destroy();
end;


procedure Tx86gen.translate(var src: TAsmCode);
begin
  // translate to x86 commands

end;


end.

unit fw_utils;

interface





// http://delphi.jmrds.com/?q=node/44 Cifrar

uses fw_system_types,fw_vm_types,fw_vm_constants;

type

    p_node=^T_node;
    T_node=record
         kind:cardinal;                  // Tipo de nodo
         pos:integer;                   // Posicion en la que se encuentra el nodo
         len:integer;                   // Longitud del texto
         hash:cardinal;                 // Hash
         prev,next:p_node;              // Anterior y posterior
         parent,first:p_node;           // Padre y primer hijo
         data:p_node;                   // Dato asociado
         value:longint;                 // Valor numerico
         number:cardinal;               // Usado en polimorfismo si tenemos mas de un elemento igual
         text:pchar;
         end;


     t_HelperProcessUnknown = function (helper:pointer;p:pRt_Var):pRt_Var;


// Utilidades generica
function _Error(v:pRt_Var;k:pchar):cardinal;
function _Ok1(v:pRt_Var;k:cardinal):cardinal;
function _Ok2(v:pRt_Var;k:cardinal;p:pointer):cardinal;

procedure s_alert(s:string);
function  s_hash(p:string):cardinal;
function  s_hashp(p:pchar):cardinal;
function  s_f2s(k1:cardinal;v1:longint):string;
function  s_s2f(s:string;var k1:Cardinal;var v1:longint):boolean;
function  s_p2s(p:pointer):string;
function  s_s2i(s:string):integer;
function  s_pc2i(s:pchar):integer;    // Igual que la anterior pero el error da -1
function  s_i2s(i:integer):string;
function  s_s2i2(s:string;var i:integer):boolean;
function  s_i2h(q:cardinal):string;
function  s_lcase(s:pchar):pchar;
function  s_pcharlen(const s:pchar):cardinal;
function  s_pcharCopy(Dest: PChar; Source: PChar): PChar;
function  s_pcharCopyL(Dest: PChar; Source: PChar):cardinal;
function  p_Inc(p:pointer;k:longint):pointer;
function  s_PcharInc(p:pchar;k:longint):pchar;
function  p_Minus(p1,p2:pointer):cardinal;
function  s_tick:cardinal;
function  s_charpos(const aSource:pchar;const C: Char;EndPos : Integer) : Integer;
function  s_wherechar(p:pchar;c:char;var s:pchar):pchar;
function  s_trimpchar(r:pchar):pchar;
function  s_trimleftpchar(r:pchar):pchar;
function  s_trimleftpchar_until(r,f:pchar):pchar;
function  s_incpchar(p:pchar):pchar;
function  s_ext(p:pchar):pchar;



function  s_Spaces(i:longint):string;
function  s_Left(const s:string;j:integer):string;
function  s_rpad(const s:string;i:longint):string;
function  s_addOver(i1,i2:longint;var res:longint):boolean;  // Suma con indicacion de Overflow
function  s_trim(s:string):string;
function  s_LoadFile(s:pchar):pchar;
function  s_CloseFile(p:pchar):pchar;
function  s_LoadIfFile(p:pchar):pchar;
function  s_CanBeFileName(p:pchar):boolean;
function  s_split(s:string;var s1,s2:string;c:char):boolean;
function  s_splitpchar(p:pchar;c:char):pchar;
function  s_fileexists(p:pchar):boolean;



procedure s_LockRead(l:pinteger);           // Acquire Read lock, yield on spin
procedure s_LockWrite(l:pinteger);          // Acquire write lock, yield on spin
procedure s_UnlockRead(l:pinteger);         // Release read lock
procedure s_UnlockWrite(l:pinteger);        // Release write lock


// Utilidades especificas de la VM
function s_isFixedValue(k:cardinal):boolean;        // Si    valor & opt_Mask1Constant (cardinal($0000FFF0)) = 0 se trata de una constante (none,integer,pchar,boolean) o un real
function s_isFloat(k:cardinal):boolean;              // Si    valor & opt_Mask2Real <> opt_Mask2Real se trata de un real
function s_isNumeric(k:cardinal):boolean;
function s_isNumNan(k:cardinal):boolean;
function s_isChar(k:cardinal):boolean;

// Gestion de bloques de codigo
function block_Get(c:prt_oCode;num_block:cardinal):pRt_oBlock;
function block_name(b:pRt_oBlock):pchar;
function block_filename(b:pRt_oBlock):pchar;
function block_params(b:pRt_oBlock):string;
function block_VarsToCapture(b:pRt_oBlock):cardinal;
function block_ExternCapturedVarsUsed(b:pRt_oBlock):cardinal;


// Gestion de instrucciones
function opc_inc(o:pRt_oOpcode):pRt_oOpcode;
function opc_incV(o:pointer;n:longint):pRt_oOpcode;

// Gestion simple de objetos

// Devuelve el hostdata y el hostcontrol de un objeto
function o_2host(c:pRt_Context;o,r1,r2:pRt_Var):boolean;


// Gestion de memoria
function m_alloc(size:cardinal):pointer;
function m_allocz(size:cardinal):pointer;
function m_free(p:pointer):pointer;
function m_free_what(p:pointer):pointer;

function m_pchar(src:pchar):pchar;
function m_pchar2(const Source:string): PChar;
function m_memorycopy(Destination: Pointer; Source: Pointer; Length:cardinal):pointer;
procedure m_Move(const Source; var Dest; Count: Integer);

function m_realloc(p:pointer;size:cardinal):pointer;
function m_reallocz(p:pointer;size:cardinal):pointer;
function m_leak:boolean;
function m_report:string;

// Gestion de variables
function v_kind(v:pRt_Var):cardinal;
function v_kind2(v1,v2:pRt_Var;k1,k2:cardinal):boolean;
function v_kind1st(v:pRt_Var):cardinal;                     // V_Kind extendido es como v_kind pero ademas contempla el caso de parametros
function v_value(v:prt_Var):pointer;
function v_valuec(v:prt_Var):cardinal;
function v_valuei(v:prt_Var):longint;
function v_val1st(v:pRt_Var):pointer;     // V_Kind extendido es como v_kind pero ademas contempla el caso de parametros
function v_Set(v:pRt_Var;k:cardinal;p:pointer):pointer;     inline;
function v_Pchar(v:pRt_Var;p:pchar):pointer;     inline;
function v_True(v:pRt_Var):pointer;     inline;
function v_False(v:pRt_Var):pointer;     inline;
function v_Bool(v:pRt_Var;b:boolean):pRt_Var; inline;
function v_BoolB(v:pRt_Var;b:boolean):boolean; inline;
function v_SetP(v:pRt_Var;k:cardinal;p:pointer):pointer;    inline;
function v_None(v:pRt_Var):pointer;                         inline;
function v_Unknown(v:pRt_Var):pointer;
function v_GetIfKind(v:pRt_Var;k:cardinal):pointer;
function v_inc(v:pRt_Var):prt_var;
function v_incN(v:pRt_Var;n:cardinal):pointer;
function v_copy(dest,src:pRt_Var):pRt_Var;
function v_copys(dest,src:pRt_Var):pRt_Var;
function v_copy_debug(dest,src:pRt_Var):pRt_Var;
function v_ValIf(p:pRt_Var;k:cardinal):pointer;
function v_v2string(v:prt_Var):string;
function V_ToString(where,what:pRt_Var):string;
function v_2pchar(v:prt_var):pchar;
function v_params(p:pRt_Var;const v: array of const):boolean;
function v_isChar(p:pRt_Var):boolean;
function v_2context(v:prt_Var):pRt_Context;






function v_valueNrd(v:pRt_Var;n:cardinal):pointer;     // V_Kind extendido es como v_kind pero ademas contempla el caso de parametros
function v_IsTrue(v:prt_Var):cardinal;


// Gestion de variables de tipo Opt_Vlist
function vars_create(size:cardinal):pRt_Var;
function vars_createSingle(v:pRt_Var):pRt_Var;
function vars_free(v:pRt_Var):pRt_Var;
function vars_get(l:pRt_Var;index:cardinal):pRt_Var;
function vars_count(l:pRt_Var):cardinal;
function vars_purge(p:pRt_Var;item:cardinal):pRt_Var;
function vars_clone(p:pRt_Var):pRt_Var;
function vars_delete(p:pRt_Var;i:cardinal):pRt_Var;
function vars_copy(dest,src:pRt_Var;count:cardinal):pRt_Var;

// Lista de valores
function l_create(item_size:cardinal;initial_size:cardinal):pointer;
function l_resize(l0:pointer;new_size:cardinal):boolean;
function l_free(l:pointer):pointer;
function l_push(l:pointer;value:pointer):cardinal;
function l_pushi(l:pointer;value:longint):cardinal;
function l_pushp(l:pointer;value:pointer):pointer;
function l_pushalloc(l:pointer):pointer;
function l_count(l:pointer):cardinal;


function l_pushNotNull(l:pointer;value:pointer):boolean;
function l_get(l:pointer;index:cardinal):pointer;
function l_geti(p:pointer;index:cardinal):integer;
function l_set(l:pointer;index:cardinal;value:pointer):pointer;
function l_pointer(l:pointer;index:cardinal):pointer;



function l_pop(l:pointer):pointer;
function l_top(l:pointer):pointer;

// Lista de strings
function strl_create:pointer;
function strl_push(l:pointer;s:string):pointer;
function strl_pop(l:pointer):string;
function strl_free(l:pointer):pointer;
function strl_get(l:pointer;i:cardinal):string;
function strl_join(l:pointer;sep:string):string;

// Helper. Un helper implementa cosas comunes, como el diccionario o la funcion de debug
function  helper_create(owner:prt_Var;dictionary_size:cardinal):pointer;
function  helper_free(h:pointer):pointer;
function  helper_GetOwner(h:pointer):pRt_Var;
function  helper_GetName(h:pointer;p:cardinal;name:pRt_Var):boolean;
function  helper_GetValueFromIndex(h:pointer;index:cardinal;value:pRt_Var):boolean;
function  helper_GetValueFromName(h:pointer;name:pchar;value:pRt_Var):boolean;
function  helper_SetValueFromName(h:pointer;name:pchar;value:pRt_Var):boolean;
function  helper_SetDebug(h:pointer;debug:TFW_Function):boolean;
function  helper_GetDebug(h:pointer):TFW_Function;
procedure helper_Process(h:pointer;p:pRt_Var;data:pointer;callback:t_HelperProcessUnknown);
function  helper_debug(helper:pointer;msg:string):boolean;

// Lista de variables
function  vl_create(initial_size:cardinal):pRt_VarList;
function  vl_createU(initial_size:cardinal):pRt_VarList;
function  vl_deinit(l:pRt_VarList):pointer;
function  vl_init(v:pRt_VarList;initial_size:cardinal):pRt_VarList;
function  vl_get(l:pRt_VarList;index:cardinal):pRt_Var;  // Obtiene una variable de una lista, si no existe la crea
function  vl_push(l:pRt_VarList;v:pRt_Var):pRt_Var;
function  vl_pushVal(l:pRt_VarList;kind:cardinal;v:pointer):pRt_Var;
function  vl_pop(l:pRt_VarList;v:pRt_Var):pRt_Var;
function  vl_free(l:pRt_VarList):pointer;


// Nodos
function  n_AddAsChildP(p,c:p_node):p_node;
function  n_Create(parent:p_node;kind:cardinal):p_node;
function  n_LastBrother(p:p_node):p_node;
function  n_Free(p:p_node):p_node;
function  n_CreateT(parent:p_node;kind:cardinal;s:string):p_node;
function  n_Text(n:p_node;s:string):pchar;
function  n_AddAsChild(p,c:p_node):p_node;
function  n_RemoveFromParent(a:p_node):p_node;
function  n_FindChildHash(p:p_node;k:cardinal):p_node;
function  n_ValuePP(p:p_node):longint;
function  n_NumberPP(p:p_node):longint;
function  n_AddAfter(p,c:p_node):p_node;
function  n_FindChildText(p:p_node;k:string):p_node;
procedure n_ClearChildren(p:p_node);
function  n_top(n:p_node):p_node;
function  n_kind(p:p_node):cardinal;
function  n_kindIs(p:p_node;k:cardinal):boolean;
function  n_next(c:p_node):p_node;
function  n_first(c:p_node):p_node;
function  n_parent(n:p_node):p_node;
function  n_ChildCount(p:p_node):integer;

// Implementa un buffer de memoria
function  b_create(size:cardinal):pointer;
function  b_writeint(p:pointer;v:longint):cardinal;
function  b_writepointer(p:pointer;v:pointer):cardinal;
function  b_writebyte(p:pointer;v:byte):cardinal;
function  b_writestring(p:pointer;text:string):cardinal;
function  b_writepchar(p:pointer;text:pchar):cardinal;
function  b_writeBlock(p:pointer;block:pchar;size:cardinal):cardinal;
function  b_Free(p:pointer):pointer;
function  b_readstring(p:pointer;pos:cardinal):pchar;
function  b_readint(p:pointer;pos:cardinal):longint;
function  b_inc(p:pointer;pos:cardinal):longint;
function  b_setint(p:pointer;pos:cardinal;v:longint):cardinal;
function  b_pointer(p:pointer;pos:cardinal):pointer;
function  b_pointercur(p:pointer):pointer;
function  b_SetCount(p:pointer;k:cardinal):cardinal;
function  b_Count(p:pointer):cardinal;
function  b_pad(p:pointer;k:longint):longint;
function  b_Size(p:pointer):cardinal;

// Lectura de archivos
function  file_open(name:string):pointer;
function  file_close(h:pointer):pointer;
function  file_read(f:pointer;buffer:pointer;size:cardinal):longint;

// Flags
function  f_CheckFlags(flags:cardinal;v1:longint):boolean;


// Float
{
Al igual que JaegerMonkey y usamos un Quirk de  IEEE/754 Double Precision Numbers

Implementamos NaN boxing

The first bit is the sign bit (purple), and the next eleven (yellow) are all exponent bits. If
all of the exponent bits are 1s, then the number is a NaN, unless all of the remaining
bits (the blue ones) are 0s. If all of the blue bits in this diagram were 0, the value would
be either positive or negative infinity. We distinguish the values we�re using for type
tags from other NaNs by marking the first 16 bits as 1s. In practice, all hardware and
standard libraries produce a single canonical NaN value, so we�re free to use all of the
other values for our own purposes. This technique is called NaN boxing
}

function  s_FloatFromString(s:string;var k1:cardinal;var v1:longint):boolean;
function  s_FloatToString(k1:cardinal;v1:longint):string;
procedure s_IntToFloat(i:longint;var k1:cardinal;var v1:longint);
procedure s_DoubleToFloat(d:double;var k1:cardinal;var v1:longint);
procedure s_DoubleToVar(d:double;p:pRt_Var);
function  s_FloatToDouble(k1:cardinal;v1:longint):double;
function  s_NumberToDouble(k1:cardinal;v1:longint):double;




function  p_check(var p:pRt_Var):cardinal;



// Implementa un pool de objetos, es muy rapida. De momento no puede crecer
function  pl_release(p:pointer):pointer;                        // Libera un elemento del pool (devuelve NIL si OK)
function  pl_get(pl:pointer):pointer;                           // Saca un elemento del pool y lo marca como ocupado (NIL si no hay ninguno libre)
function  pl_free(pl:pointer):pointer;                          // Libera el pool y devuelve nil
function  pl_create(count,item_size:cardinal):pointer;          // Hay Count elementos de tama�o item_Size
function  pl_item(pl:pointer;item:cardinal):pointer;            // Obtiene un item


// Implementa un Stream
{function str_create(size:cardinal):pointer;
function str_Size(str:pointer):cardinal;
function str_Write(str:pointer;Buffer:pchar; Count:cardinal):longint;
function str_Read(str:pointer;Buffer:pointer; count:cardinal):longint;    // Por implementacion siempre devuelve >0
function str_Destroy(str:pointer):pointer;
function str_memory(str:pointer;value:longint):pointer;
function str_Clear(str:pointer):cardinal;
function str_Filecreate(name:string):pointer;
}

procedure DLog(s:string);

// Implementa un splitter
function  split_create(p:pchar;c:char):pointer;
function  split_count(sp:pointer):cardinal;
function  split_get(sp:pointer;c:cardinal):pchar;
function  split_destroy(sp:pointer):pointer;


// Dentro del contexto
function  ctx_NewString(c:pRt_Var;p:pchar;r:pRt_Var):boolean;
function  ctx_NewProperty(c,o:pRt_Var;name:pchar;value:prt_Var):boolean;
function  ctx_NewPropertyStr(c,o:pRt_Var;name,value:pchar):boolean;
function  ctx_NewObject(c,res:pRt_Var):boolean;



type


      THClass_Name=function (o:pRt_Object):pchar;                                                               // Obtiene el nombre de la clase de un Objeto



var opcodes:array [0..255] of TOpcode_Func;
    VAR_NONE:TRt_Var=(kind:opt_None;v_c:0);
    internal_log:tFw_Function=nil;
    flags_values:array[0..2,false..true] of cardinal;
    HClass_Name:THClass_Name;



implementation

type
      TRt_Name=record name:pchar;value:tRt_Var;hash:cardinal;end;
      pRt_Name=^tRt_name;


      pRt_Pool=^tRt_Pool;
      tRt_PoolItem=record pool:pRt_Pool; data:record end;end;
      pRt_PoolItem=^tRt_PoolItem;

      tRt_Pool=record
                  count,item_size:cardinal;     // N� de elementos del pool y tama�o de cada uno
                  items:pRt_PoolItem;           // Zona de memoria con los elementos
                  stack:ppointer;               // Base Stack con las direcciones de los elementos
                  tstack:ppointer;              // Top del Stack anterior
                  end;

      pRt_Stream=^tRt_Stream;
      tRt_StreamCleanUp=function(p:pRt_Stream):boolean;
      tRt_StreamRead=   function (str:pointer;Buffer:pointer; count:cardinal):longint;    // Por implementacion siempre devuelve >0
      tRt_StreamWrite=  function (str:pointer;Buffer:pchar; Count:cardinal):cardinal;

       // Implementa una lista generica
       t_list=record count,size,item_size,lock:cardinal;data:p_pointer;end;
      p_list=^t_list;

      tRt_Helper=record owner:tRt_Var;names:p_list;debug:TFW_Function;end;
      prt_Helper=^tRt_Helper;


      t_Buffer=record size,count:cardinal;thread_safe:boolean;mutex:integer;data:pchar;end;
      p_Buffer=^t_Buffer;



      tRt_Stream=record
                  fPosition, fSize, fCapacity: cardinal;
                  data:pointer;
                  str_clean:tRt_StreamCleanUp;
                  str_read: tRt_StreamRead;
                  str_write:tRt_StreamWrite;
                  Memory: pointer;
                  end;


      tRt_Splitter=record
                str:pchar;
                count:cardinal;
                items:ppchar;
                end;
      pRt_Splitter=^tRt_Splitter;


      pRt_StrItem=^tRt_StrItem;
      tRt_StrItem=record
                str:string;
                next:pRt_StrItem;
                end;


const V_SIZE=SizeOf(TRt_Var);
      KK_NULLPCHAR:pchar='[NULL PCHAR]';
      kernel32 ='kernel32.dll';
      user32   ='user32.dll';
      GENERIC_READ           = LongWord($80000000);
      FILE_SHARE_READ        = $00000001;
      OPEN_EXISTING          = 3;
      FILE_ATTRIBUTE_NORMAL  = $00000080;
      FILE_ATTRIBUTE_ARCHIVE = $00000020;
      FILE_ATTRIBUTE_BAD     = $FFFFFFFF;

      FILE_BEGIN             = 0;
      HEAP_ZERO_MEMORY       = $00000008;
      seek_FromEnd           =5;
      seek_FromBeggining     =4;
      seek_FromCurrent       =3;


var memory_count:longint=0;          // Memoria reservada con malloc menos memoria reservada con Free
    memory_peak:longint=0;          // Maximo valor de memory_used
    blocks_count:longint=0;          // Llamadas a malloc menos llamadas a Free
    malloc_count:longint=0;
    free_count:longint=0;
    my_heap:longint=0;

// http://code.google.com/p/scalemm/
// http://code.google.com/p/asmprofiler/w/list
// http://andremussche.blogspot.com/
// http://code.google.com/p/jazz-sdk/

function MessageBox(h:cardinal;p1,p2:PChar;uType:cardinal):Integer;                                               stdcall; external user32   name 'MessageBoxA';
function GetFileAttributes(lpFileName: PChar): cardinal;                                                          stdcall; external kernel32 name 'GetFileAttributesA';
function CreateFile(filename:pchar;ac,sh:cardinal;lp:pointer;d1,d2,t:cardinal):integer;                           stdcall; external kernel32 name 'CreateFileA';
function CloseHandle(hObject:longint): LongBool;                                                                  stdcall; external kernel32 name 'CloseHandle';
function GetFileSize(hFile:longint;lpFileSizeHigh:Pointer):LongWord;                                              stdcall; external kernel32 name 'GetFileSize';
function SetFilePointer(h:integer;d:Longint;dh:Pointer;m:longword):longword;                                      stdcall; external kernel32 name 'SetFilePointer';
function ReadFile(h:integer;var Buffer;nBytes:LongWord;var nRead:LongWORD;lpo:pointer):LongBool;                  stdcall; external kernel32 name 'ReadFile';
function GetProcessHeap: THandle;                                                                                 stdcall; external kernel32 name 'GetProcessHeap';
function HeapAlloc(hHeap: THandle; dwFlags, dwBytes: cardinal): Pointer;                                          stdcall; external kernel32 name 'HeapAlloc';
function HeapFree(hHeap: THandle; dwFlags:cardinal;lpMem:Pointer):LongBool;                                       stdcall; external kernel32 name 'HeapFree';
function HeapReAlloc(hHeap: THandle; dwFlags:cardinal;lpMem: Pointer; dwBytes:cardinal): Pointer;                 stdcall; external kernel32 name 'HeapReAlloc';
function SwitchToThread:LongBool;                                                                                 stdcall; external kernel32 name 'SwitchToThread';
function GetTickCount: cardinal;                                                                                  stdcall; external kernel32 name 'GetTickCount';
function HeapSize(hHeap: THandle; dwFlags:LongWord; lpMem: Pointer):LongWord;                                     stdcall; external kernel32 name 'HeapSize';
function GetLastError: longword;                                                                                  stdcall; external kernel32 name 'GetLastError';
function HeapCreate(flOptions, dwInitialSize, dwMaximumSize:LongWord): THandle; stdcall;                          stdcall; external kernel32 name 'HeapCreate';
function HeapDestroy(hHeap: THandle):LongBool; stdcall;                                                           stdcall; external kernel32 name 'HeapDestroy';

function  s_tick:cardinal;
begin
  result:=GetTickCount;
end;

function  s_fileexists(p:pchar):boolean;
var f:LongWord;
begin
 f:= GetFileAttributes(p);
 result:=(f<>FILE_ATTRIBUTE_BAD) and ((f and FILE_ATTRIBUTE_ARCHIVE)=FILE_ATTRIBUTE_ARCHIVE);    // Solo existe si no falla y ademas es archivo
end;

function  s_ext(p:pchar):pchar;
begin
  result:=nil;while p^<>#0 do begin if p^='.' then result:=p;inc(p);end;
end;

function  s_wherechar(p:pchar;c:char;var s:pchar):pchar;
var cr:boolean;
begin
  cr:=false;
  while (p^<>#0) and (p^<>c) do begin
          if not cr then if p^<' ' then cr:=true;
          if cr and (p^>' ') then begin s:=p;cr:=false;end;

          inc(p);
          result:=p;
          end;
end;

function  s_PcharInc(p:pchar;k:longint):pchar;
begin
  inc(p,k);
  result:=p;
end;


function s_charpos(const aSource:pchar;const C: Char;EndPos : Integer) : Integer;
begin
  asm
      mov result, 0
      PUSH  EDI
      mov EDI, aSource
      mov ECX, EndPos
      mov AL, C
  @Loop:
      cmp Al, [EDI]
      jz @Found
      inc EDI
      dec ECX
      jnz @Loop
      jmp @NotFound
      @Found:
      sub EDI, aSource
      inc EDI
      mov Result, EDI
      @NotFound:
      POP EDI
      end;
end;

function s_CloseFile(p:pchar):pchar;
begin
  result:=m_free(p);
end;

function s_LoadFile(s:pchar):pchar;
var iFileHandle: Integer;
    iFileLength,lastSize: LongWord;
    buffer:pointer;
begin
  result:=nil;
  iFileHandle:=CreateFile(s,GENERIC_READ,FILE_SHARE_READ,nil, OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);if iFileHandle=0 then exit;
  iFileLength := GetFileSize(iFileHandle,@lastSize);
  if iFileLength>0 then begin
            SetFilePointer(iFileHandle,0,nil,FILE_BEGIN);                                                                       // LLevamos al archivo al inicio
            buffer:=m_allocZ(iFileLength+2);                                                                                    // Incrementamos en +2
            if ReadFile(iFileHandle,buffer^,iFileLength,lastSize, nil) then result:=buffer else result:=m_Free(buffer);
            end;
  // Cerramos el archivo
  CloseHandle(iFileHandle);
end;

function  file_open(name:string):pointer;
var i:Integer;
begin
  if name='' then begin result:=nil;exit;end;
  i:=CreateFile(pchar(name),GENERIC_READ,FILE_SHARE_READ,nil, OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
  SetFilePointer(i,0,nil,FILE_BEGIN);                                                                       // LLevamos al archivo al inicio
  result:=pointer(i);
end;

function  file_read(f:pointer;buffer:pointer;size:cardinal):longint;
var readed:LongWord;
begin
   if ReadFile(longint(f),buffer^,size,readed,nil) then result:=readed else result:=-1;
end;

function  file_close(h:pointer):pointer;
begin
  if h<>nil then CloseHandle(longint(h));
  result:=nil;
end;


function  s_ValidCharInFileName(c:char):boolean;
begin
  result:=(c>=' ') and (not (c in ['?','<','>','*']));
end;

function  s_CanBeFileName(p:pchar):boolean;
var k:cardinal;
begin
  result:=true;k:=0;
  while (p^<>#0) and result do begin result:=(k<260) and s_ValidCharInFileName(p^);inc(p);inc(k);end;
end;

function  s_lcase(s:pchar):pchar;
begin
  result:=s;if s=nil then exit;
  while s^<>#0 do begin
    if (s^>='A') and (s^<='Z') then s^:=char(byte(s^)+32);
    inc(s);
    end;
end;

function  s_incpchar(p:pchar):pchar;
begin
  inc(p);result:=p;
end;

function  s_LoadIfFile(p:pchar):pchar;
begin
  if not s_CanBeFileName(p) then begin result:=nil;exit;end;
  result:=s_LoadFile(p);
end;


function m_leak:boolean;
begin
  result:=(blocks_count<>0) or (memory_count<>0);
end;

function m_report:string;
begin
  result:='#Blocks: '+s_i2s(blocks_count)+'   #Memory: '+s_i2s(memory_count)
          +'    #Malloc: '+s_i2s(malloc_count)+'   #Free: '+s_i2s(free_count)
          +'    Peak memory use: '+s_i2s(memory_peak);

end;

function m_alloc(size:cardinal):pointer;
begin
 if size>0 then begin
        //result:=HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY,size);
        result:=HeapAlloc(my_Heap, HEAP_ZERO_MEMORY,size);
        malloc_count:=malloc_count+1;
        blocks_count:=blocks_count+1;
        //memory_count:=memory_count+HeapSize(GetProcessHeap,0,result);
        memory_count:=memory_count+HeapSize(my_Heap,0,result);
        if memory_count>memory_peak then memory_peak:=memory_count;
        end else result:=nil;
end;

function m_allocz(size:cardinal):pointer;
begin
  result:=m_alloc(size);
end;

function m_free(p:pointer):pointer;
var l:longint;
begin
  if p<>nil then begin
        free_count:=free_count+1;
        blocks_count:=blocks_count-1;
        //memory_count:=memory_count-HeapSize(GetProcessHeap,0,p);
        //if not HeapFree(GetProcessHeap,0,p) then s_alert('Error in HeapFree: '+s_i2s(GetLastError));
        memory_count:=memory_count-HeapSize(my_Heap,0,p);
        if not HeapFree(my_Heap,0,p) then s_alert('Error in HeapFree: '+s_i2s(GetLastError));
        end;
  result:=nil;
end;

function m_realloc(p:pointer;size:cardinal):pointer;
begin
 if (p=nil)  then begin result:=m_alloc(size);exit;end;
 if (size=0) then begin result:=m_free(p);exit;end;

 //memory_count:=memory_count-HeapSize(GetProcessHeap,0,p);
 //result:=HeapReAlloc(GetProcessHeap,HEAP_ZERO_MEMORY,p,size);
 //if result<>nil then memory_count:=memory_count+HeapSize(GetProcessHeap,0,result);

  memory_count:=memory_count-HeapSize(my_Heap,0,p);
 result:=HeapReAlloc(my_Heap,HEAP_ZERO_MEMORY,p,size);
 if result<>nil then memory_count:=memory_count+HeapSize(my_Heap,0,result);

end;

function m_reallocz(p:pointer;size:cardinal):pointer;
begin
 result:=m_realloc(p,size);
end;

function  m_memorycopy(Destination: Pointer; Source: Pointer; Length:cardinal):pointer;
begin
  if destination=nil then destination:=m_alloc(length);
  Move(Source^, Destination^, Length);
  result:=destination;
end;


{
function m_alloc(size:cardinal):pointer;
begin
  if size>0 then begin
        result:=GetMemory(size);
        inc(uso_malloc);
        if uso_malloc>max_malloc then max_malloc:=uso_malloc;
        end else result:=nil;
end;

function m_free(p:pointer):pointer;
begin
  if p<>nil then begin
        pointer(FreeMemory(p));
        dec(uso_malloc);
        end;
  result:=nil;
end;

function m_realloc(p:pointer;size:cardinal):pointer;
begin
  result:=ReallocMemory(p,size);
  if (p=nil) and (size>0)  then inc(uso_malloc);
  if (p<>nil) and (size=0) then dec(uso_malloc);
end;

function m_allocz(size:cardinal):pointer;
var p:^pointer;
begin
  result:=m_alloc(size);
  p:=result;FillChar(p^,size,#0);
end;
}

function m_free_what(p:pointer):pointer;
begin
  m_Free(p);
  result:=p;
end;







procedure m_Move(const Source; var Dest; Count: Integer);
asm
      cmp eax, edx
      je  @exit

      push esi
      push edi

      mov esi, eax
      mov edi, edx
      shr ecx, 2
      jz  @MoveRest

   @LongLoop:
      mov eax, [esi]
      mov [edi], eax
      dec ecx
      jz  @MoveRest4
      mov eax, [esi + 4]
      mov [edi + 4], eax
      add esi, 8
      add edi, 8
      dec ecx
      jne @LongLoop
      jmp @MoveRest

   @MoveRest4:
      add esi, 4
      add edi, 4

   @moveRest:
      mov ecx, Count
      and ecx, 3
      rep movsb

      pop edi
      pop esi
   @exit:
end;


function  s_split(s:string;var s1,s2:string;c:char):boolean;
var i:longint;
begin
  i:=s_charpos(pchar(s),c,length(s));
  result:=i>0;
  if result then begin s1:=s_trim(Copy(s1,1,i-1));s2:=s_trim(copy(s1,i+1,length(s)));end
            else s1:=s_trim(s);s2:='';

end;

function  s_trimleftpchar_until(r,f:pchar):pchar;
begin
 result:=r;while (result^<=' ') and (result^<>#0) and (result<>f) do inc(result);
end;

function  s_trimleftpchar(r:pchar):pchar;
begin
 result:=r;while (result^<=' ') and (result^<>#0) do inc(result);
end;

function  s_trimpchar(r:pchar):pchar;
begin
 result:=r;while (result^<=' ') and (result^<>#0) do inc(result);if result^=#0 then exit;   // Trim Left
 r:=result;while (r^<>#0) do inc(r);                                                        // Go to End
 while (r^<=' ') and (r<>result) do begin r^:=#0;dec(r);end;                                // Trim Right
end;


function  s_splitpchar(p:pchar;c:char):pchar;
var p1:pchar;
begin
  result:=p;if (c=#0) or (result=nil) then exit;
  while (result^<>c) and (result^<>#0) do inc(result);
  if result^=c then begin result^:=#0;inc(result);end;
end;



function  s_trim(s:string):string;
var i:longint;
begin
  result:='';if length(s)=0 then exit;
  for i:=1 to length(s) do if s[i]>' ' then result:=result+s[i];
end;

procedure s_alert(s:string);
begin
 MessageBox(0,PChar(s),'VM Message',0);
end;

procedure s_LockRead(l:pinteger);           // Acquire Read lock, yield on spin
ASM
        MOV EDX, l                           // EDX es la direccion donde tenemos el LOCK
  @@Back:
        MOV EAX, [ EDX ]                     // Copiamos el contenido del LOCK en EAX
  @@Retry:
        TEST EAX, EAX                        // Comparamos el contenido del LOCK consigo mismo
        JS @@Spin                            // Si el FLAG de SIGNO se activa (es decir el LOCK no esta escribiendo) saltamos a @@SPIN
        LEA ECX, [ EAX + 1 ]                 // EXC=[EAX+1]
   LOCK CMPXCHG [ EDX ], ECX                 // Guardamos el nuevo valor
        JNZ @@Retry                          // Si no pudimos (pej otro thread escribiendo/leyendo) volvemos a empezar
        RET
  @@Spin:
        PUSH EDX
        CALL SwitchToThread
        POP EDX
        JMP @@Back
  END;

procedure s_LockWrite(l:pinteger);          // Acquire write lock, yield on spin
  ASM
        MOV EDX, l
  @@Back:
        MOV EAX, [ EDX ]
        TEST EAX, EAX
        JNZ @@Spin
        MOV ECX, -1
   LOCK CMPXCHG [ EDX ], ECX
        JZ  @@Done
  @@Spin:
        //PUSH EDX                  ASI ES MAS AGRESIVO !!!!
        //CALL SwitchToThread
        //POP EDX
        JMP @@Back
  @@Done:
  END;

procedure s_UnlockRead(l:pinteger);           // Release read lock
ASM
   LOCK DEC [l]
END;

procedure s_UnlockWrite(l:pinteger);          // Release write lock
ASM
   LOCK INC [l]
END;


function v_2pchar(v:prt_var):pchar;
begin
 if v=nil                     then result:=nil else
 if v.kind=opt_ConstPchar     then result:=v.v_pc else
 if v.kind=opT_ExtPChar       then result:=pchar(@(pRt_ExPchar(v.v_p).data)) else result:=nil;
end;



function v_v2string(v:prt_Var):string;
var j:cardinal;
begin
  if v=nil then begin result:='-- NO VAR--';exit;end;

  if (v.kind=opt_ConstPchar) or (v.kind=opT_ExtPChar) then begin result:=string(v_2pchar(v));exit;end;
  if v.kind=opT_Integer     then begin result:=s_i2s(v.v_i);exit;end;
  if v.kind=opT_Object      then begin result:='[object ';if assigned(HClass_Name) then result:=result+' '+HClass_Name(v.v_p);result:=result+' @'+s_p2s(v.v_p)+']';exit;end;
  if v.kind=opt_HostData    then begin result:='[h_data  @'+s_p2s(v.v_p)+']';exit;end;
  if v.kind=opt_Runner      then begin result:='[runner  @'+s_p2s(v.v_p)+']';exit;end;
  if v.kind=opt_ExternF     then begin result:='[externf @'+s_p2s(v.v_p)+']';exit;end;
  if v.kind=opt_Context     then begin result:='[context @'+s_p2s(v.v_p)+']';exit;end;
  if v.kind=opt_Boolean     then begin if v.v_c=0 then result:='true' else result:='false';exit;end;
  if v.kind=opt_VarList     then begin
               j:=v.v_c;result:='';inc(v);
               while j>0 do begin result:=result+v_v2string(v)+',';inc(v);dec(j);end;
               if result<>'' then result:=s_left(result,Length(result)-1);
               exit;
               end;

  result:='['+vm_t2str(v.kind)+']';
end;


function v_2context(v:prt_Var):pRt_Context;
begin
  if v=nil then result:=nil else
  if v.kind=opt_Context then result:=v.v_ct else
  if v.kind=opt_Runner  then result:=v.v_r.context else result:=nil;
end;

function V_ToString(where,what:pRt_Var):string;
var l,r:tRt_Var;
    c:pRt_Context;
begin
  if what=nil then begin result:='[Can''t convert nil to string]';exit;end;
  if what.kind=opt_Object then begin
              c:=v_2context(where);v_pchar(@l,'toString');
              if c<>nil then if c.lib_run(what,@l,nil,@r)=FW_OK then begin result:=v_ToString(where,@r);exit;end;
              end;
  result:=v_v2string(what);
end;


function v_IsTrue(v:prt_Var):cardinal;
begin
  if v.kind=opt_ConstPchar then begin if string(v.v_pc)<>''                             then result:=FW_OK else result:=FW_FAIL;exit;end;
  if v.kind=opT_ExtPChar   then begin if string(pchar(@(pRt_ExPchar(v.v_p).data)))<>''  then result:=FW_OK else result:=FW_FAIL;exit;end;
  if v.kind=opT_Integer    then begin if v.v_i<>0                                       then result:=FW_OK else result:=FW_FAIL;exit;end;
  if v.kind=opt_Object     then begin if v.v_p<>nil                                     then result:=FW_OK else result:=FW_FAIL;exit;end;
  if v.kind=opt_Boolean    then begin if v.v_i<>0                                       then result:=FW_OK else result:=FW_FAIL;exit;end;
  if v.kind=opt_Nan        then begin result:=FW_FAIL;exit;end;
  if v.kind=opt_None       then begin result:=FW_FAIL;exit;end;
  if s_isFloat(v.kind)    then begin if s_FloatToDouble(v.kind,v.v_i)<>0              then result:=FW_OK else result:=FW_FAIL;exit;end;
  result:=FW_HALT;
end;


function  s_hashp(p:pchar):cardinal;         // Calcula un Hash
var x: cardinal;
begin
  result:=0;if p=nil then exit;
  while (p^<>#0) do begin
          Result:=(Result shl 4)+ord(p^);x:=Result and $F0000000;
          if (x<>0) then begin result:=result xor (x shr 24);result:=result and (not x);end;
          inc(p);
          end;
end;

function s_hash(p:string):cardinal;
var j,i,x: cardinal;
    b:byte;
begin
  result:=0;if p='' then exit;
  j:=length(p);i:=1;
  repeat
      b:=ord(p[i]);
      if b<>0 then begin
            Result := (Result shl 4)+b;x:=Result and $F0000000;
            if (x<>0) then begin Result:=Result xor (x shr 24);Result:=Result and (not x);end;
            end;
      i:=i+1;
      until (i>j);
end;

//  (from qstrings (C) Andrew N. Driazgov ):
{
function Q_StrHashKey(const S: string): LongWord;
asm
TEST EAX,EAX
JE @@qt
LEA EDX,[EAX-1]
MOV ECX,[EAX-4]
XOR EAX,EAX
TEST ECX,ECX
JE @@qt
PUSH ESI
PUSH EBX
@@lp: MOV ESI,EAX
SHL EAX,5
MOVZX EBX,BYTE PTR [EDX+ECX]
ADD EAX,ESI
ADD EAX,EBX
DEC ECX
JNE @@lp
POP EBX
POP ESI
@@qt:
end;
}


function s_pcharCopy(Dest: PChar; Source: PChar): PChar;
begin
  if source=nil then result:=nil
                else begin result:=dest;while source^<>#0 do begin dest^:=source^;inc(dest);inc(source);end;dest^:=#0;end;
end;

function  s_pcharCopyL(Dest: PChar; Source: PChar):cardinal;
begin
  result:=0;
  if (source<>nil) and (dest<>nil) then begin
          while source^<>#0 do begin dest^:=source^;inc(dest);inc(source);inc(result);end;dest^:=#0;
          end;
end;


function  m_pchar2(const Source:string): PChar;
begin
  result:=m_allocz(length(source)+1);
  s_PcharCopy(Result,pchar(source));
end;

function  m_pchar(src:pchar):pchar;
var i:longint;
begin
  if src=nil then begin result:=nil;exit;end;
  i:=s_pcharlen(src);i:=i+1;
  result:=m_alloc(i);
  s_pcharCopy(result,src);
end;



function  s_pcharlen(const s:pchar):cardinal;   // Comparamos DWORDS es un FastCase
var P, PStr: PChar;
    I, J: Integer;
begin
  if s=nil then   begin result:=0;exit;end;
  if s^=#0 then   begin result:=0;exit;end;
  if s[1]=#0 then begin result:=1;exit;end;
  if s[2]=#0 then begin result:=2;exit;end;
  if s[3]=#0 then begin result:=3;exit;end;
  P:=Pointer(s);PStr:=P;
  P:=Pointer(Integer(P) and -4);
  repeat
      Inc(P, 4);
      I:=PInteger(P)^;
      J:= I-$01010101;
      I:=not(I);
      I:=I and J;
      until (I and $80808080)<>0;
 Result:=P-PStr;
 if I and $80=0 then if I and $8000 <> 0 then Inc(Result) else if I and $800000 <> 0 then Inc(Result, 2) else Inc(Result, 3)
end;

function s_i2s(i:integer):string;
begin
  Str(i,Result);
end;

function  s_i2h(q:cardinal):string;
var   r:cardinal;
begin
  result:= '';
  while q>0 do begin
      r:=q mod 16;
      if r>=10 then result:=chr(55+r)+result else result:=s_i2s(r)+result;
      q:=q div 16;
      end;
  if result='' then result:='0';
end;


function  s_s2i(s:string):integer;
var E: Integer;
begin
  Val(S, Result, E);
  if E<>0 then result:=0; // Esto quiere decir un error de conversion
end;

function  s_pc2i(s:pchar):integer;
var E: Integer;
begin
  if s=nil then result:=-1
           else begin
                  Val(S, Result, E);
                  if E<>0 then result:=-1; // Esto quiere decir un error de conversion
                end;
end;

function  s_s2i2(s:string;var i:integer):boolean;
var E: Integer;
begin
  Val(S, i, E);
  result:=E=0;      // Si true NO hay error
end;

function  s_p2s(p:pointer):string;
begin
  result:=s_i2s(cardinal(p));
end;

function s_s2f(s:string;var k1:Cardinal;var v1:longint):boolean;
var k:double;
    err:integer;
    p1:^longint;
begin
  Val(s,k,err);
  result:=err=0;if err<>0 then exit;
  p1:=@k;
  v1:=p1^;inc(p1);k1:=p1^;
end;


function  s_f2s(k1:cardinal;v1:longint):string;
var k:^double;
    e1,e2:extended;
    l:array[0..1] of longint;
begin
  l[0]:=v1;l[1]:=k1;
  k:=@l[0];
  e1:=int(k^);e2:=Frac(k^)*100;
  result:=s_i2s(round(e1))+'.'+s_i2s(round(e2*10) div 10);
end;



function  s_rpad(const s:string;i:longint):string;
begin
 result:=s_left(s+s_Spaces(i),i);
end;

function  s_Left(const s:string;j:integer):string;
begin
  result:=copy(s,1,j);
end;

function  s_Spaces(i:longint):string;
begin
 if i<=0 then begin result:='';exit;end;
 Setlength(result,i);
 while (i>0) do begin result[i]:=' ';i:=i-1;end;
end;

function s_addOver(i1,i2:longint;var res:longint):boolean;
begin
   asm                                                     // Hacemos una suma con acarreo
    mov   eax,i1
    mov   ecx,i2
    add   eax,ecx
    seto   result
    mov   res,eax
end;
end;



function l_count(l:pointer):cardinal;
begin
  result:=p_list(l).count;
end;



// Item_Size es en Bytes
function l_create(item_size,initial_size:cardinal):pointer;
var l:p_list;
begin
  if item_size<SizeOf(pointer) then item_size:=SizeOf(pointer);           // El tama�o menor es SizeOF(Pointer)
  while (item_size mod SizeOf(pointer))<>0 do item_size:=item_size+1;     // Nos aseguramos que el tama�o del item sea multiplo del tama�o del puntero.
  l:=m_allocz(SizeOf(t_list));                                       // Generamos la estructura base
  l.item_size:=item_size div SizeOf(pointer);                        // Internamente se guarda en unidades de SizeOf(pointer)
  l.data:=m_alloc(initial_size*item_size*SizeOf(pointer));           // Alloc de los bytes que necesitamos
  l.size:=initial_size;                                              // Count y Size se miden en unidades de item_size
  l.count:=0;
  result:=l;
end;

function l_delta(l:p_list):cardinal;
begin
  if l.size<8 then result:=8 else if l.size<16 then result:=16 else result:=l.size*2;
end;


function l_getaddress(l:p_list;index:cardinal):pointer;
var p:p_pointer;
begin
  if (l<>nil) and (index<l.count) then begin p:=l.data;inc(p,index*l.item_size);result:=p;end else result:=nil;
end;

function l_getaddress2(l:p_list;index:cardinal):pointer;
var p:p_pointer;
begin
  if index<l.size then begin p:=l.data;inc(p,index*l.item_size);result:=p;end else result:=nil;
end;

// l_new -> devuelve a un puntero a una zona de tama�o Item_size dentro de la lista
function l_new(l:p_list):pointer;
begin
  if l.count=l.size then begin
        l.size:=l_delta(l);
        l.data:=m_reallocz(l.data,l.size*l.item_size*SizeOf(pointer));
        end;
  l.count:=l.count+1;
  result:=l_getaddress(l,l.count-1);
end;

function l_push(l:pointer;value:pointer):cardinal;
var p:p_pointer;
begin
  p:=l_new(l);
  p^:=value;
  result:=p_list(l).count-1;
end;


function l_free(l:pointer):pointer;
begin
  if l<>nil then begin m_free(p_list(l).data);m_free(l);end;
  result:=nil;
end;



function l_resize(l0:pointer;new_size:cardinal):boolean;
var l:p_list;
begin
  l:=l0;
  if l.size<new_size then   begin l.size:=new_size;l.data:=m_realloc(l.data,l.size*l.item_size*SizeOf(pointer));result:=true;end else result:=false;
end;

function l_pushNotNull(l:pointer;value:pointer):boolean;
begin
  result:=value<>nil;
  if result then l_push(l,value);
end;


function l_pushi(l:pointer;value:longint):cardinal;
begin
  result:=l_push(l,pointer(value));
end;


function l_pushp(l:pointer;value:pointer):pointer;
var p:p_pointer;
begin
  p:=l_new(l);
  p^:=value;
  result:=value;
end;

function l_pushalloc(l:pointer):pointer;
begin
  result:=l_new(l);
end;

function l_pop(l:pointer):pointer;
var p:p_pointer;
begin
  p:=l_getaddress(l,p_list(l).count-1);
  if p<>nil then begin result:=p^;dec(p_list(l).count);end else result:=nil;
end;

function l_top(l:pointer):pointer;
var p:p_pointer;
begin
  p:=l_getaddress(l,p_list(l).count-1);
  if p<>nil then result:=p^ else result:=nil;
end;

function l_get(l:pointer;index:cardinal):pointer;
var p:p_pointer;
begin
  p:=l_getaddress2(l,index);
  if p<>nil then result:=p^ else result:=nil;
end;

function l_pointer(l:pointer;index:cardinal):pointer;
begin
  result:=l_getaddress2(l,index);
end;



function  l_geti(p:pointer;index:cardinal):integer;
begin
  result:=longint(l_get(p,index));
end;


function l_set(l:pointer;index:cardinal;value:pointer):pointer;
var p:p_pointer;
begin
  p:=l_getaddress2(l,index);
  if p<>nil then begin p^:=value;result:=value;end else result:=nil;
end;


function helper_create(owner:prt_Var;dictionary_size:cardinal):pointer;
var h:pRt_Helper;
begin
  h:=m_allocz(SizeOf(tRt_Helper));
  v_copy(@h.owner,owner);
  h.names:=l_create(SizeOf(tRt_Name),dictionary_size);
  result:=h;
end;


function helper_free(h:pointer):pointer;
var i:cardinal;
    h1:pRt_Helper;
begin
  h1:=h;i:=0;
  while (i<l_count(h1.names)) do begin
          m_Free(pRt_Name(l_getaddress(h1.names,i)).name);
          i:=i+1;
          end;
  l_free(h1.names);
  result:=m_free(h1);
end;


function lnames_add(l:p_list;name:pchar;value:pRt_Var):pRt_Name;
var h:cardinal;
begin
  h:=s_hashp(name);if h=0 then begin result:=nil;exit;end;
  result:=l_new(l);
  result.name:=m_pchar(name);
  result.hash:=h;
  result.value.kind:=value.kind;
  result.value.v_p:=value.v_p;
end;

function lnames_find(l:p_list;name:pchar):pRt_Name;
var k,i:cardinal;
    p:pRt_Name;
begin
    result:=nil;      if (l=nil) or (name=nil) then exit;
    k:=s_hashp(name); if k=0 then exit;
    i:=l_count(l);p:=l_getaddress(l,0);
    while (i>0) and (result=nil) do if p.hash=k then result:=p else begin inc(p);dec(i);end;
end;




function helper_GetName(h:pointer;p:cardinal;name:pRt_Var):boolean;
var n:pRt_Name;
begin
  n:=l_getaddress(pRt_Helper(h).names,p);result:=n<>nil;
  if result then v_Set(name,opt_ConstPchar,n.name) else v_Set(name,opt_None,nil);
end;


function  helper_GetValueFromIndex(h:pointer;index:cardinal;value:pRt_Var):boolean;
var n:pRt_Name;
begin
  n:=l_getaddress(pRt_Helper(h).names,index);result:=n<>nil;
  if result then v_Copy(value,@n.value) else v_Set(value,opt_None,nil);
end;

function helper_GetValueFromName(h:pointer;name:pchar;value:pRt_Var):boolean;
var n:pRt_Name;
begin
  result:=false;if (h=nil) or (value=nil) or (name=nil) then exit;
  n:=lnames_find(pRt_Helper(h).names,name);
  result:=n<>nil;
  if result then v_Copy(value,@n.value) else v_Set(value,opt_None,nil);;
end;

function helper_SetValueFromName(h:pointer;name:pchar;value:pRt_Var):boolean;
var n:pRt_Name;
begin
  result:=false;if (h=nil) or (value=nil) or (name=nil) then exit;
  n:=lnames_find(pRt_Helper(h).names,name);if n=nil then n:=lnames_add(pRt_Helper(h).names,name,value);
  result:=n<>nil;
end;

function helper_SetDebug(h:pointer;debug:TFW_Function):boolean;
begin
  pRt_Helper(h).debug:=debug;
  result:=assigned(debug);
end;

function helper_GetDebug(h:pointer):TFW_Function;
begin
   if h=nil then result:=nil else result:=pRt_Helper(h).debug;
end;

function  helper_GetOwner(h:pointer):pRt_Var;
begin
   if h=nil then result:=nil else result:=@pRt_Helper(h).owner;
end;


// Procesa los parametros del Helper. Si
procedure helper_Process(h:pointer;p:pRt_Var;data:pointer;callback:t_HelperProcessUnknown);
var np:cardinal;
    p1,fin:pRt_Var;
    h1:prt_Helper;
    pc:pchar;
begin
  np:=p_check(p);if (np=0) then exit;fin:=v_incN(p,np);
  h1:=h;
  while (p<>nil) and (p<>fin) do begin
        p1:=p;
        if p.kind=opt_Debug then begin h1.debug:=p.v_p;helper_debug(h,'Debug @ '+s_p2s(p.v_p));inc(p);end else
        if p.kind=opt_Name  then begin pc:=p.v_pc;inc(p);
                                       helper_SetValueFromName(h1,pc,p);
                                       helper_debug(h,'Names ['+pc+'] is '+vm_v2str(p));
                                       inc(p);
                                       end else
        if assigned(callback) then p:=callback(h,p) else inc(p);
        if p1=p then p:=nil;     // Para evitar bucles infinitos
        end;
end;


function  helper_debug(helper:pointer;msg:string):boolean;
var v2,vr:tRt_Var;
    w:array[0..10] of tRt_Var;
    h:pRt_Helper;
begin
  h:=helper;result:=(h<>nil) and (Assigned(h.debug));if not result then exit;
  v2.kind:=opt_Info;v2.v_p:=nil;v_params(@w,[opt_ConstPchar,pchar(msg)]);
  result:=h.debug(@h.owner,@v2,@w,@vr)=FW_OK;
end;



function v_params(p:pRt_Var;const v: array of const):boolean;
var i,j:cardinal;
begin
  j:=high(v)-low(v)+1;                          // Esta es la longitud del array recibido
  result:=j mod 2=0;if not result then exit;    // Que debe de ser par
  i:=j div 2;                                   // Este es el n� de parametros que tenemos
  j:=low(v);                                    // Y este es el primer parametro
  p.kind:=opt_VarList;p.v_c:=i;                 // Iniciamos el opt_VarList
  while (i>0) do begin                          // Y vamos copiando los parametros
      inc(p);
      p.kind:=v[j].VInteger;inc(j);
      p.v_p:= v[j].VPointer;inc(j);
      i:=i-1;
      end;
end;

function v_isChar(p:pRt_Var):boolean;
begin
  if p=nil then result:=false else result:=s_isChar(p.kind);
end;


function v_kind(v:pRt_Var):cardinal;
begin
  if v=nil then result:=opt_None else result:=v.kind;
end;

function v_value(v:prt_Var):pointer;
begin
  if v=nil then result:=nil else result:=v.v_p;
end;

function v_valuec(v:prt_Var):cardinal;
begin
  if v=nil then result:=0 else result:=v.v_c;
end;

function v_valuei(v:prt_Var):longint;
begin
  if v=nil then result:=0 else result:=v.v_i;
end;

function v_incN(v:pRt_Var;n:cardinal):pointer;
begin
  if v<>nil then begin inc(v,n);result:=v;end else result:=nil;
end;

function v_inc(v:pRt_Var):prt_var;
begin
  if v<>nil then begin inc(v);result:=v;end else result:=nil;
end;

function v_copy(dest,src:pRt_Var):pRt_Var;
begin
  if dest<>nil then begin
        if src<>nil then begin dest.kind:=src.kind;dest.v_p:=src.v_p;end else dest.kind:=opt_None;
        end;
  result:=dest;
end;

function v_copys(dest,src:pRt_Var):pRt_Var;
begin
  if dest<>nil then begin
        if src<>nil then begin dest.kind:=src.kind;dest.v_p:=src.v_p;end else dest.kind:=opt_None;
        end;
  result:=src;
end;

function v_copy_debug(dest,src:pRt_Var):pRt_Var;
begin
  if dest<>nil then begin
        if src<>nil then begin dest.kind:=src.kind;dest.v_p:=src.v_p;s_alert('%V_Copy COPIED');end else begin dest.kind:=opt_None;s_alert('%V_Copy SRC=NIL');end;
        end else s_alert('%V_Copy SRDEST=NIL');
  result:=dest;
end;

function v_ValIf(p:pRt_Var;k:cardinal):pointer;
begin
  if p=nil then result:=nil
           else if p.kind=k then result:=p.v_p else result:=nil;
end;

function v_kind1st(v:pRt_Var):cardinal;     // V_Kind extendido es como v_kind pero ademas contempla el caso de parametros
begin
  if v=nil then result:=opt_None else result:=v.kind;
  if result=opt_VarList then result:=v_kind1st(v_inc(v));
end;

function v_val1st(v:pRt_Var):pointer;     // V_Kind extendido es como v_kind pero ademas contempla el caso de parametros
begin
  if v=nil then result:=nil
           else if v.kind<>opt_VarList then result:=v.v_p else result:=v_val1st(v_inc(v));
end;

function v_kind2(v1,v2:pRt_Var;k1,k2:cardinal):boolean;
begin
  result:=(v_kind1st(v1)=k1) and (v_kind1st(v2)=k2);
end;


function v_valueNrd(v:pRt_Var;n:cardinal):pointer;     // V_Kind extendido es como v_kind pero ademas contempla el caso de parametros
begin
  if v=nil then begin result:=nil;exit;end;
  if (v.kind=opt_VarList) then begin inc(v,n+1);result:=v.v_p;end
                          else if n=0 then result:=v.v_p else result:=nil;

end;

function v_GetIfKind(v:pRt_Var;k:cardinal):pointer;
begin
  if v_kind(v)=k then result:=v.v_p else result:=nil;
end;

function v_Set(v:pRt_Var;k:cardinal;p:pointer):pointer;
begin
  if v<>nil then begin v.kind:=k;v.v_p:=p;end;
  result:=v;
end;

function v_Pchar(v:pRt_Var;p:pchar):pointer;     inline;
begin
  if v<>nil then begin v.kind:=opt_ConstPchar;v.v_p:=p;end;
  result:=v;
end;


function v_True(v:pRt_Var):pointer;     inline;
begin
  if v<>nil then begin v.kind:=opt_Boolean;v.v_c:=1;end;
  result:=v;
end;

function v_Bool(v:pRt_Var;b:boolean):pRt_Var; inline;
begin
  if b then v_True(v) else v_False(v);
  result:=v;
end;

function v_BoolB(v:pRt_Var;b:boolean):boolean; inline;
begin
  if b then v_True(v) else v_False(v);
  result:=b;
end;


function v_False(v:pRt_Var):pointer;     inline;
begin
  if v<>nil then begin v.kind:=opt_Boolean;v.v_c:=0;end;
  result:=v;
end;


function v_SetP(v:pRt_Var;k:cardinal;p:pointer):pointer;
begin
  if v<>nil then begin v.kind:=k;v.v_p:=p;end;
  result:=p;
end;

function v_Unknown(v:pRt_Var):pointer;
begin
  if v<>nil then begin v.kind:=opt_Unknown;v.v_c:=0;end;
  result:=v;
end;

function v_none(v:pRt_Var):pointer;
begin
  if v<>nil then begin v.kind:=opt_None;v.v_p:=nil;end;
  result:=v;
end;


function vars_create(size:cardinal):pRt_Var;
var v:pRt_Var;
begin
  result:=m_alloc((size+1)*V_SIZE);
  result.kind:=opt_VarList;result.v_c:=size;
  v:=result;
  while size>0 do begin inc(v);v.kind:=opT_None;size:=size-1;end;
end;

function vars_createSingle(v:pRt_Var):pRt_Var;
var p:pRt_Var;
begin
  result:=vars_create(1);p:=result;
  inc(p);
  if v<>nil then begin p.kind:=v.kind;p.v_p:=v.v_p;end
            else begin p.kind:=opt_None;p.v_p:=nil;end;
end;

function vars_clone(p:pRt_Var):pRt_Var;
var i:cardinal;
    p1:pRt_Var;
begin
  if p=nil then begin result:=nil;exit;end;
  result:=vars_create(p.v_c);p1:=result;i:=p.v_c;
  while i>0 do begin inc(p);inc(p1);p1.kind:=p.kind;p1.v_p:=p.v_p;dec(i);end;
end;

function vars_delete(p:pRt_Var;i:cardinal):pRt_Var;
begin
  result:=p;if p=nil then exit;
  if i>=p.v_c then exit;
  inc(p,i+1);
  while i<p.v_c-1 do begin p:=v_copy(p,v_Inc(p));inc(i);end;
  dec(result.v_c);
end;

function vars_copy(dest,src:pRt_Var;count:cardinal):pRt_Var;
begin
  result:=dest;if (dest=nil) or (src=nil) then exit;
  while (count>0) do begin dest.kind:=src.kind;dest.v_p:=src.v_p;inc(dest);inc(src);dec(count);end;
end;


function vars_free(v:pRt_Var):pRt_Var;
begin
  result:=m_free(v);
end;

function vars_count(l:pRt_Var):cardinal;
begin
  if l.kind<>opt_VarList then result:=0 else result:=l.v_c;
end;


function vars_get(l:pRt_Var;index:cardinal):pRt_Var;
begin
  if l=nil then begin result:=nil;exit;end;
  if l.kind<>opt_VarList then begin if index=0 then result:=l else result:=nil;exit;end;
  if index<l.v_c then begin result:=l;inc(result,index+1);end else result:=nil;
end;

function vars_purge(p:pRt_Var;item:cardinal):pRt_Var;
var s:pRt_Var;
    i,j:cardinal;
begin
  if p.kind<>opt_VarList then begin result:=nil;exit;end;               // Solo funciona con opt_VarList
  if item>=p.v_c then begin result:=nil;exit;end;                       // Si item>=count no podemos purgar
  i:=0;j:=p.v_c;s:=vars_Create(j-1);result:=s;inc(s);inc(p);            // Creamos una nueva lista de tama�o el mismo-1
  while (i<j) do begin                                                  // Recorremos todos los valores de p
      if i<>item then begin v_copy(s,p);inc(s);end;                     // Si no se trata de item lo copiamos y avanzamos s
      inc(p);inc(i);                                                    // Avanzamos p,i
      end;
end;


function vl_init(v:pRt_VarList;initial_size:cardinal):pRt_VarList;
var p:pRt_Var;
begin
  if initial_size<8 then initial_size:=8;      // El tama�o minimo de lista es de 8 variables
  v.count:=0;v.size:=initial_size;
  v.list:=m_alloc(initial_size*V_SIZE);
  p:=v.list;
  while initial_size>0 do begin p.kind:=opT_None;initial_size:=initial_size-1;inc(p);end;
  result:=v;
end;

function  vl_deinit(l:pRt_VarList):pointer;
begin
  result:=m_free(l.list);
end;

function  vl_free(l:pRt_VarList):pointer;
begin
  if l<>nil then begin m_free(l.list);m_free(l);end;
  result:=nil;
end;

function  vl_create(initial_size:cardinal):pRt_VarList;
begin
  result:=vl_init(m_alloc(SizeOf(tRt_VarList)),initial_size);
end;

function  vl_createU(initial_size:cardinal):pRt_VarList;
var p:pRt_Var;
begin
  result:=vl_init(m_alloc(SizeOf(tRt_VarList)),initial_size);
  if initial_size>0 then begin
          p:=result.list;
          while (initial_size>0) do begin p.kind:=opt_None;p.v_c:=0;inc(p);dec(initial_size);end;
          end;
end;


function  vl_get(l:pRt_VarList;index:cardinal):pRt_Var;  // Obtiene una variable de una lista, si no existe la crea
var nc,i:cardinal;
begin
  nc:=index+1;
  if nc>l.size then begin                                                    // Si no los tenemos hacemos un resize
        l.size:=2*nc;                                                         // Duplicamos el tama�o
        l.list:=m_realloc(l.list,l.size*V_SIZE);
        result:=l.list;inc(result,l.count);
        for i:=l.count to l.size do begin result.kind:=opT_None;inc(result);end;
        l.count:=nc;
        end;
  result:=l.list;inc(result,index);
end;

function  vl_pushVal(l:pRt_VarList;kind:cardinal;v:pointer):pRt_Var;
begin
  result:=vl_get(l,l.count);
  result.kind:=kind;
  result.v_p:=v;
end;

function  vl_push(l:pRt_VarList;v:pRt_Var):pRt_Var;
begin
  result:=v_copy(vl_get(l,l.count),v);
end;

function  vl_pop(l:pRt_VarList;v:pRt_Var):pRt_Var;
begin
  if l.count=0 then result:=nil
               else begin l.count:=l.count-1;result:=v_copy(v,vl_get(l,l.count));end;
end;




function  p_Inc(p:pointer;k:longint):pointer;
begin
  result:=pointer(pchar(p)+k);
end;

function  p_Minus(p1,p2:pointer):cardinal;
begin
  result:=cardinal(pchar(p2)-pchar(p1));
end;


function block_Get(c:prt_oCode;num_block:cardinal):pRt_oBlock;
begin
  if c=nil then begin result:=nil;exit;end;
  if num_block>=c.num_blocks then begin result:=nil;exit;end;
  result:=p_Inc(c,c.blocks);
  inc(result,num_block);
end;

function block_name(b:pRt_oBlock):pchar;
begin
  //result:=p_Inc(p_Inc(b.oCode,b.oCode.strings),b.name);
  if b=nil then result:=KK_NULLPCHAR
           else result:=@(prt_ExPchar(p_Inc(b.oCode,b.oCode.strings+b.name)).data);

end;

function block_VarsToCapture(b:pRt_oBlock):cardinal;
begin
  result:=b.vars_captures;
end;

function block_ExternCapturedVarsUsed(b:pRt_oBlock):cardinal;
begin
  result:=b.vars_extern_captured;
end;

procedure Init;
var i:longint;
begin
  for i:=0 to 255 do opcodes[i]:=nil;
end;


function opc_inc(o:pRt_oOpcode):pRt_oOpcode;
begin
  if o<>nil then inc(o);
  result:=o;
end;

function opc_incV(o:pointer;n:longint):pRt_oOpcode;
begin
  if o=nil then result:=nil else begin result:=o;inc(result,n);end;
end;

function o_2host(c:pRt_Context;o,r1,r2:pRt_Var):boolean;
var k:tRt_Var;
begin
  result:=(v_kind(o)=opt_Object) and (c<>nil);if not result then exit;
  if r1<>nil then begin  v_Set(@k,opt_HostData,POINTER(0));   c.lib_get(o,@k,nil,r1);end;
  if r2<>nil then begin  v_Set(@k,opt_HostControl,pointer(0));c.lib_get(o,@k,nil,r2);end;
end;





procedure DLog(s:string);
var v:tRt_Var;
begin
  if assigned(internal_log) then begin
          v.kind:=opt_ConstPchar;v.v_pc:=pchar(s);
          internal_log(nil,nil,@v,nil);
    end;
end;


function f_CheckFlags(flags:cardinal;v1:longint):boolean;
begin
  case v1 of
    opF_Eq:       result:=(flags and 1)=1; // (flags and 001)=001
    opF_NEq:      result:=(flags and 1)=0; // (flags and 001)=000
    opF_Big:      result:=(flags and 2)=2; // (flags and 010)=010
    opF_NBig:     result:=(flags and 2)=0; // (flags and 010)=000
    opF_Less:     result:=(flags and 4)=4; // (flags and 100)=100
    opF_NLess:    result:=(flags and 4)=0; // (flags and 100)=000
    opF_BigEq:    result:=((flags and 2)=2) or ((flags and 1)=1);
    opF_NBigEq:   result:=not (((flags and 2)=2) or ((flags and 1)=1));
    opF_LessEq:   result:=((flags and 4)=4) or ((flags and 1)=1);
    opF_NLessEq:  result:=not (((flags and 4)=4) or ((flags and 1)=1));
    opF_True:     result:=true;   //result:=(flags and 8)=8;
    opF_False:    result:=false; // result:=(flags and 8)=0;
    else          result:=false;
  end;
end;



function fw_varnames(num:cardinal;values:ppointer):string;
var xp:pRt_ExPchar;
begin
  result:='';
  while num>0 do begin
        xp:=values^;
        //result:=result+' '+s_p2s(xp)+' |';
        result:=result+'('+s_i2s(xp.len)+') '+pchar(@xp.data)+'|';
        inc(values);dec(num);
        end;
  if result<>'' then result:=s_Left(result,length(result)-1);
end;


function fw_clean(r:string):string;
var i:longint;
begin
  result:='';
  if r='' then exit;
  if r[1]='(' then begin
    i:=2;while (i<length(r)) and (r[i]<>')') do i:=i+1;
    if i<length(r) then r:=copy(r,i+1,length(r));
    end;
  while (r<>'') and ((r[1]=' ') or (r[1]='|')) do r:=copy(r,2,length(r));
  while (r<>'') and ((r[length(r)]=' ') or (r[length(r)]='|')) do r:=copy(r,1,length(r)-1);
  result:=r;
end;

function fw_split(var r:string):string;
var i:longint;
begin
  result:='';
  i:=1;while (i<length(r)) and (r[i]<>'|') do i:=i+1;
  if i>=length(r) then begin r:=fw_clean(r);exit;end;
  result:=copy(r,i+1,length(r));
  r:=fw_clean(s_Left(r,i));
end;



function _Error(v:pRt_Var;k:pchar):cardinal;
begin
  result:=FW_FAIL;
  v_Set(v,opt_ConstPchar,k);
end;

function _Ok1(v:pRt_Var;k:cardinal):cardinal;
begin
  result:=FW_OK;
  if v<>nil then v.kind:=k;
end;

function _Ok2(v:pRt_Var;k:cardinal;p:pointer):cardinal;
begin
  result:=FW_OK;
  if v<>nil then begin v.kind:=k;v.v_p:=p;end;
end;


function block_filename(b:pRt_oBlock):pchar;
begin
 result:=@(prt_ExPchar(p_Inc(b.oCode,b.oCode.strings+b.oCode.filename)).data);
end;

function block_params(b:pRt_oBlock):string;
var xp:pRt_ExPchar;
    num:longint;
    values:ppointer;
begin
  result:='';num:=b.vars_params;values:=p_Inc(b.oCode,b.var_names);
  while num>=0 do begin
        xp:=values^;
        result:=result+pchar(@xp.data);
        inc(values);dec(num);
        if num>0 then result:=result+', ';
        end;
  if result<>'' then result:=s_Left(result,length(result)-1);
end;


function s_FloatFromString(s:string;var k1:Cardinal;var v1:longint):boolean;
var k:double;
    err:integer;
    p1:^longint;
begin
  Val(s,k,err);
  result:=err=0;if err<>0 then exit;
  p1:=@k;
  v1:=p1^;inc(p1);k1:=p1^;
end;





// Vale esto NO es muy eficiente, pero lo estoy haciendo en un tren despues de 4 horas de reunion
// con una cerveza en el estomago y la cabza embotada !!!
function  s_FloatToDouble(k1:cardinal;v1:longint):double;
var k:^double;
    e1,e2,e3,e4:extended;
    l:array[0..1] of longint;
    b:boolean;
begin
  l[0]:=v1;l[1]:=k1;
  k:=@l[0];
  e1:=int(k^);        // La parte entera
  result:=0;
  if e1<0 then begin b:=true;e1:=-e1;end else b:=false;
  e4:=1;
  while e1<>0 do begin
        e2:=trunc(e1/10);                   // Esto es la division entera
        e3:=e1-e2*10;                       // Esto es el resto
        result:=e4*trunc(e3)+result;
        e4:=e4*10;
        e1:=e2;                             // Hacemos el shiftr que es equivalente a dividir entre 10
        end;
  if b then result:=-result;
  result:=result+Frac(k^);
  //e1:=Frac(k^)*100;
  //result:=result+'.'+_i2s(round(e1*10) div 10);end;
  //s_alert(FloatToStr(result));
end;


function  s_NumberToDouble(k1:cardinal;v1:longint):double;
begin
  if k1=opT_Integer then result:=v1 else
  if s_isFloat(k1) then result:=s_FloatToDouble(k1,v1) else result:=0;
end;

procedure s_IntToFloat(i:longint;var k1:cardinal;var v1:longint);
var k:double;
    p1:^longint;
begin
  k:=i;
  p1:=@k;
  v1:=p1^;inc(p1);k1:=p1^;
end;

procedure s_DoubleToFloat(d:double;var k1:cardinal;var v1:longint);
var p1:^longint;
begin
  p1:=@d;
  v1:=p1^;inc(p1);k1:=p1^;
end;

procedure s_DoubleToVar(d:double;p:pRt_Var);
begin
  s_DoubleToFloat(d,p.kind,p.v_i);
end;


function _i2s(i:integer):string;
begin
  Str(i,Result);
end;

function s_FloatToString(k1:cardinal;v1:longint):string;
var k:^double;
    e1,e2,e3:extended;
    l:array[0..1] of longint;
    b:boolean;
begin
  l[0]:=v1;l[1]:=k1;
  k:=@l[0];
  //Str(k^,result);

  e1:=int(k^);        // La parte entera
  result:='';
  if e1<0 then begin b:=true;e1:=-e1;end else b:=false;

  while e1<>0 do begin
        e2:=trunc(e1/10);                   // Esto es la division entera
        e3:=e1-e2*10;                       // Esto es el resto
        result:=_i2s(trunc(e3))+result;        // Acumulamos el resto
        e1:=e2;                             // Hacemos el shiftr que es equivalente a dividir entre 10
        end;
  if result='' then result:='0';
  if b then result:='-'+result;

  e1:=Frac(k^)*100;
  result:=result+'.'+_i2s(round(e1*10) div 10);
end;





function s_isFixedValue(k:cardinal):boolean;        // Si    valor & opt_Mask1Constant (cardinal($0000FFF0)) = 0 se trata de una constante (none,integer,pchar,boolean) o un real
begin
  result:=(k and opt_Mask1Constant)=opt_Mask0NoReal;
  if not result then result:=s_isFloat(k);

end;

function s_isFloat(k:cardinal):boolean;              // Si    valor & opt_Mask2Real <> opt_Mask2Real se trata de un real
begin
  result:=(k and opt_Mask2Real)<>opt_Mask2Real;
end;

function s_isNumeric(k:cardinal):boolean;
begin
  result:=(k=opT_Integer) or ((k and opt_Mask2Real)<>opt_Mask2Real);
end;


function s_isNumNan(k:cardinal):boolean;
begin
  result:=(k=opT_Integer) or (k=opt_Nan) or ((k and opt_Mask2Real)<>opt_Mask2Real);
end;

function s_isChar(k:cardinal):boolean;
begin
  result:=(k=opT_ExtPChar) or (k=opt_ConstPchar);
end;



// ---------------------------------------------------------------------------------------------------------------


function  n_AddAfter(p,c:p_node):p_node;
begin
  result:=c;
  c.parent:=p.parent;
  c.next:=p.next;p.next:=c;c.prev:=p;
end;

function  n_Create(parent:p_node;kind:cardinal):p_node;
var p:p_node;
begin
 result:=m_alloc(sizeof(T_Node));
 result.kind:=kind;
 result.data:=nil;
 result.pos:=0;result.len:=0;result.value:=0;
 result.prev:=nil;result.next:=nil;
 result.first:=nil;
 result.parent:=parent;
 result.text:=nil;
 result.number:=0;
 // Lo ponemos al final de los hijos del padre
 if (parent<>nil) then begin
        if parent.first=nil then parent.first:=result
                            else begin p:=n_LastBrother(parent.first);p.next:=result;result.prev:=p;end;
        end;
end;



function  n_parent(n:p_node):p_node;
begin
  if n<>nil then result:=n.parent else result:=nil;
end;

function  n_first(c:p_node):p_node;
begin
  if c=nil then result:=nil else result:=c.first;
end;

function  n_next(c:p_node):p_node;
begin
  if c=nil then result:=nil else result:=c.next;
end;


function  n_descends(p,h:p_node):boolean;
begin
 result:=(p<>nil) and (h<>nil) and (p=h);
 while (result=false) and (p<>nil) and (h<>nil) do begin  h:=h.parent;result:=p=h;end;
end;


function  n_ValuePP(p:p_node):longint;
begin
  if p=nil then result:=0 else begin result:=p.value;p.value:=p.value+1;end;
end;

function  n_NumberPP(p:p_node):longint;
begin
  if p=nil then result:=0 else begin result:=p.number;p.number:=p.number+1;end;
end;

function  n_top(n:p_node):p_node;
begin
  while n_parent(n)<>nil do n:=n_parent(n);
  result:=n;
end;

function  n_Clone(p:p_node):p_node;
begin
  if p<>nil then begin
        result:=n_Create(nil,p.kind);
        result.data:=p.data;
        result.pos:=p.pos;
        result.len:=p.len;
        result.hash:=p.hash;
        result.value:=p.value;
        end else result:=nil;
end;

function  n_hash(p:p_node):cardinal;
begin
  if p=nil then result:=0 else result:=p.hash;
end;


function  n_CreateT(parent:p_node;kind:cardinal;s:string):p_node;
begin
  result:=n_Create(parent,kind);
  result.hash:=s_Hash(s);
  result.text:=m_pchar2(s);
end;

function  n_Text(n:p_node;s:string):pchar;
begin
  result:=n.text;
  m_free(n.text);
  n.text:=m_pchar2(s);
end;


function  n_Free(p:p_node):p_node;
begin
 if p<>nil then begin
        while (p.first<>nil) do n_Free(p.first);
        if (p.parent<>nil) and (p.parent.first=p) then p.parent.first:=p.next;
        if p.text<>nil then m_free(p.text);
        m_free(p);
        end;
 result:=nil;
end;


function n_LastBrother(p:p_node):p_node;
begin
  if p=nil then begin result:=nil;exit;end;
  while (p.next<>nil) do p:=p.next;
  result:=p;
end;

function  n_ChildCount(p:p_node):integer;
begin
  if p<>nil then begin result:=0;p:=p.first;while (p<>nil) do begin result:=result+1;p:=p.next;end;end
            else result:=-1;
end;

function  n_FindChildText(p:p_node;k:string):p_node;
begin
  result:=nil;
  if p<>nil then begin p:=p.first;while (p<>nil) and (result=nil) do if p.text=k then result:=p else p:=p.next;end;
end;

function  n_FindChildHash(p:p_node;k:cardinal):p_node;
begin
  result:=nil;if k=0 then exit;
  if p<>nil then begin p:=p.first;while (p<>nil) and (result=nil) do if p.hash=k then result:=p else p:=p.next;end;
end;


function  n_FindChildData(p,data:p_node):p_node;
begin
  result:=nil;p:=n_first(p);
  while (p<>nil) and (result=nil) do if p.data=data then result:=p else p:=p.next;
end;



procedure n_ClearChildren(p:p_node);
begin
 if p<>nil then begin while (p.first<>nil) do n_Free(p.first);p.first:=nil;end;
end;

function n_RemoveFromParent(a:p_node):p_node;
begin
  result:=a;
  // Si es el primer hijo lo quitamos del padre
  if (a.parent<>nil) and (a.parent.first=a) then a.parent.first:=a.next;
  // Si habia un hermano anterior, este apunta al siguiente
  if (a.prev<>nil) then a.prev.next:=a.next;
  // Si a tenia un hermano posterior
  if (a.next<>nil) then a.next.prev:=a.prev;
  // Ya no hay hermanos de a
  a.parent:=nil;a.prev:=nil;a.next:=nil;
end;


function  n_AddAsChild(p,c:p_node):p_node;
begin
  result:=c;if (c=nil) or (p=nil) then exit;
  c.parent:=p;c.next:=nil;
  if p.first=nil then begin p.first:=c;c.prev:=nil;end
                 else begin p:=n_LastBrother(p.first);p.next:=c;c.prev:=p;end;
end;

function  n_AddAsChildP(p,c:p_node):p_node;
begin
  n_AddAsChild(p,c);result:=p;
end;

function  n_AddAsFirstChild(p,c:p_node):p_node;
begin
  result:=c;if (c=nil) or (p=nil) then exit;
  if p.first<>nil then begin p.first.prev:=c;c.next:=p.first;c.prev:=nil;end;
  c.parent:=p;p.first:=c;
end;

procedure n_MakeBinaryOperator(c:p_node);
var a1,b1:p_node;
begin
  if c=nil then exit;
  // Sacamos el hermano anterior y posterior de c. Si son nulos salimos
  a1:=c.prev;if a1=nil then exit;b1:=c.next;if b1=nil then exit;
  // Sacamos a1 y b1 de su padre
  n_RemoveFromParent(a1);n_RemoveFromParent(b1);
  // Y los a�adimos como hijos de c
  n_AddAsChild(c,a1);n_AddAsChild(c,b1);
end;

function  n_Grandson(c:p_node):p_node;
begin
  if (c<>nil) and (c.first<>nil) then result:=C.first.first else result:=nil;
end;

function  n_kindIs(p:p_node;k:cardinal):boolean;
begin
  result:=(p<>nil) and (p.kind=k);
end;

function  n_kind(p:p_node):cardinal;
begin
  if p=nil then result:=0 else result:=p.kind;
end;

function  n_name(p:p_node;s:pchar):string;
begin
  if (s=nil) or (s^=#0) then result:='' else result:=copy(s+p.pos,1,p.len);

end;




function  n_PointerIsChildNode(c:p_node;p:pointer):boolean;
begin
  result:=c=p;if result then exit;
  c:=c.first;
  while (c<>nil) and (not result) do begin result:=n_PointerIsChildNode(c,p);c:=c.next;end;
end;


function b_create(size:cardinal):pointer;
var b:p_Buffer;
begin
  b:=m_allocz(SizeOf(t_Buffer));
  if size=0 then b.data:=nil else b.data:=m_alloc(size);
  b.size:=size;b.count:=0;
  b.mutex:=0;b.thread_safe:=false;
  result:=b;
end;

function  b_Size(p:pointer):cardinal;
begin
  result:=p_Buffer(p).size;
end;



function b_grow(p:p_Buffer;NewSize:cardinal):longint;
begin
  result:=(p.count+NewSize)-p.size;
  if result>0 then begin p.size:=p.count+NewSize;p.data:=m_realloc(p.data,p.size);end;
end;

function  b_SetCount(p:pointer;k:cardinal):cardinal;
begin
  b_grow(p,k);
  p_Buffer(p).count:=k;
end;


function b_writeint(p:pointer;v:longint):cardinal;
var c:pchar;
    i:^longint;
    p1:p_Buffer;
begin
  p1:=p;
  if p1.thread_safe then s_LockWrite(@p1.mutex);                // El lock de write espera a que nadie este leyendo / escribiendo
  b_Grow(p,SizeOf(longint));
  c:=p1.data;inc(c,p1.count);result:=p1.count;
  i:=pointer(c);i^:=v;p1.count:=p1.count+SizeOf(longint);
  if p1.thread_safe then s_UnlockWrite(@p1.mutex);    // El lock de write espera a que nadie este leyendo / escribiendo
end;

function  b_writepointer(p:pointer;v:pointer):cardinal;
begin
  result:=b_writeint(p,longint(v));
end;


function b_writebyte(p:pointer;v:byte):cardinal;
var c:pchar;
    p1:p_Buffer;
begin
  p1:=p;
  if p1.thread_safe then s_LockWrite(@p1.mutex);    // El lock de write espera a que nadie este leyendo / escribiendo
  b_Grow(p,1);
  c:=p1.data;inc(c,p1.count);result:=p1.count;
  c^:=char(v);p1.count:=p1.count+1;
  if p1.thread_safe then s_UnlockWrite(@p1.mutex);    // El lock de write espera a que nadie este leyendo / escribiendo
end;

function b_pad(p:pointer;k:longint):longint;
var c:pchar;
    p1:p_Buffer;
begin
  p1:=p;
  result:=0;if k<=0 then exit;if k>8 then k:=8;
  if p1.thread_safe then s_LockWrite(@p1.mutex);    // El lock de write espera a que nadie este leyendo / escribiendo
  c:=p1.data;inc(c,p1.count);
  result:=(longint(c) mod k);
  if result>0 then begin result:=k-result;b_Grow(p,result);for k:=1 to result do begin c^:=#0;inc(c);end;p1.count:=p1.count+cardinal(result);end;
  if p1.thread_safe then s_UnlockWrite(@p1.mutex);    // El lock de write espera a que nadie este leyendo / escribiendo
end;



function b_writestring(p:pointer;text:string):cardinal;
var c:pchar;
    p1:p_Buffer;
begin
  p1:=p;
  if length(text)=0 then begin result:=0;exit;end;
  if p1.thread_safe then s_LockWrite(@p1.mutex);    // El lock de write espera a que nadie este leyendo / escribiendo
  b_Grow(p,length(text)+1);
  c:=p1.data;
  inc(c,p1.count);
  result:=p1.count;
  s_pcharCopy(c,pchar(text));
  p1.count:=p1.count+cardinal(length(text))+1;
  if p1.thread_safe then s_UnlockWrite(@p1.mutex);    // El lock de write espera a que nadie este leyendo / escribiendo
end;

function b_writepchar(p:pointer;text:pchar):cardinal;
var c:pchar;
    p1:p_Buffer;
begin
  p1:=p;
  if length(text)=0 then begin result:=0;exit;end;
  if p1.thread_safe then s_LockWrite(@p1.mutex);    // El lock de write espera a que nadie este leyendo / escribiendo
  b_Grow(p,length(text)+1);
  c:=p1.data;
  inc(c,p1.count);
  result:=p1.count;
  p1.count:=p1.count+s_pcharCopyL(c,text)+1;
  if p1.thread_safe then s_UnlockWrite(@p1.mutex);    // El lock de write espera a que nadie este leyendo / escribiendo
end;

function b_writeBlock(p:pointer;block:pchar;size:cardinal):cardinal;
var c:pchar;
    p1:p_Buffer;
begin
  p1:=p;
  if size=0 then begin result:=p1.count;exit;end;
  if p1.thread_safe then s_LockWrite(@p1.mutex);    // El lock de write espera a que nadie este leyendo / escribiendo
  b_Grow(p,size);
  c:=p1.data;inc(c,p1.count);result:=p1.count;
  p1.count:=p1.count+size;
  while size>0 do begin c^:=block^;inc(c);inc(block);size:=size-1;end;
  if p1.thread_safe then s_UnlockWrite(@p1.mutex);    // El lock de write espera a que nadie este leyendo / escribiendo
end;

function b_Free(p:pointer):pointer;
begin
  if p<>nil then begin m_free(p_Buffer(p).data);m_free(p);end;
  result:=nil;
end;


function b_readstring(p:pointer;pos:cardinal):pchar;
var    p1:p_Buffer;
begin
  p1:=p;
  if p1.thread_safe then s_LockRead(@p1.mutex);                                       // El lock de write espera a que nadie este leyendo / escribiendo
  if pos>p1.count then result:=nil else begin result:=p1.data;inc(result,pos);end;
  if p1.thread_safe then s_UnlockRead(@p1.mutex);                                     // El lock de write espera a que nadie este leyendo / escribiendo
end;


function  b_pointercur(p:pointer):pointer;
var r:pchar;
begin
  r:=p_Buffer(p).data;inc(r,p_Buffer(p).count);result:=r;
end;

function  b_pointer(p:pointer;pos:cardinal):pointer;
var r:pchar;
begin
  if pos>p_buffer(p).count then result:=nil else begin r:=p_buffer(p).data;inc(r,pos);result:=r;end;
end;

function  b_setint(p:pointer;pos:cardinal;v:longint):cardinal;
var r:pchar;
    i:^longint;
    p1:p_buffer;
begin
  p1:=p;
  if p1.thread_safe then s_LockRead(@p1.mutex);    // El lock de write espera a que nadie este leyendo / escribiendo
  if pos>p1.count then result:=0 else begin r:=p1.data;inc(r,pos);i:=pointer(r);result:=i^;i^:=v;end;
  if p1.thread_safe then s_UnlockRead(@p1.mutex);    // El lock de write espera a que nadie este leyendo / escribiendo
end;

function b_readint(p:pointer;pos:cardinal):longint;
var r:pchar;
    i:^longint;
    p1:p_buffer;
begin
  p1:=p;
  if p1.thread_safe then s_LockRead(@p1.mutex);    // El lock de write espera a que nadie este leyendo / escribiendo
  if pos>p1.count then result:=0 else begin r:=p1.data;inc(r,pos);i:=pointer(r);result:=i^;end;
  if p1.thread_safe then s_UnlockRead(@p1.mutex);    // El lock de write espera a que nadie este leyendo / escribiendo
end;


function  b_inc(p:pointer;pos:cardinal):longint;
begin
  result:=b_readint(p,pos);
  b_setint(p,pos,result+1);                               // Incrementamos
end;



function b_Count(p:pointer):cardinal;
var  p1:p_buffer;
begin
  p1:=p;
  if p1.thread_safe then s_LockRead(@p1.mutex);      // El lock de write espera a que nadie este leyendo / escribiendo
  result:=p1.count;
  if p1.thread_safe then s_UnlockRead(@p1.mutex);    // El lock de write espera a que nadie este leyendo / escribiendo
end;

function  p_check(var p:pRt_Var):cardinal;
begin
  if p=nil              then begin p:=@var_none;result:=0;end   else
  if p.kind=opt_None    then begin result:=0;p:=nil;end         else
  if p.kind=opt_VarList then begin result:=p.v_c;inc(p);end     else result:=1;
end;



function  pl_create(count,item_size:cardinal):pointer;                                          // Hay Count elementos de tama�o item_Size
var p:pRt_Pool;
    p1:pchar;
    i_s:cardinal;
begin
  p:=m_alloc(SizeOf(tRt_Pool));
  count:=count+1;                                                                               // El item 0 queda como marcador
  p.count:=count;                                                                               // Creamos el pool
  i_s:=item_size+SizeOf(tRt_PoolItem);                                                          // Cada Item del Pool tiene un PayLoad = tRt_PoolItem
  p.items:=m_alloc(count*i_s);                                                                  // Generamos espacio para los N items
  p.stack:=m_alloc(count*SizeOf(pointer));                                                      // Generamos una lista de N punteros
  p.tstack:=p.stack;                                                                            // tStack apunta a la lista de punteros
  p1:=pointer(p.items);                                                                         // Vamos a iniciar el stack, para ello
  while count>0 do begin                                                                        // Recorremos todos los items
        pRt_PoolItem(p1).pool:=p;                                                               // En el PayLoad de cada item guardamos el Pool
        p.tstack^:=p1;                                                                          // Hacemos que un elemento de la lista de punteros apunte al PayLoad
        inc(p.tstack);inc(p1,i_s);dec(count);                                                   // Avanzamos
        end;
  dec(p.tstack);                                                                                // Ahora en stack tenemos una lista de punteros tRt_PoolItems disponibles y tStack apunta al ultimo elemento de esa lista
  result:=p;
end;

function  pl_free(pl:pointer):pointer;                          // Libera el pool y devuelve nil
begin
  if pl<>nil then begin
        m_free(pRt_Pool(pl).items);
        m_free(pRt_Pool(pl).stack);
        m_free(pl);
  end;
  result:=nil;
end;

function  pl_get(pl:pointer):pointer;                           // Saca un elemento del pool y lo marca como ocupado (NIL si no hay ninguno libre)
var p:pRt_Pool;
begin
  p:=pl;
  if p.tstack=p.stack then begin result:=nil;exit;end;         // Si tStack ha llegado al primer elemento hemos agotado el Pool
  result:=@pRt_PoolItem(p.tstack^).data;                       // Devolvemos los datos
  dec(p.tstack);                                               // Avanzamos tStack
end;

function  pl_release(p:pointer):pointer;                       // Libera un elemento del pool (devuelve NIL si OK)
var p1:pchar;
    l:pRt_PoolItem;
begin
  p1:=p;dec(p1,SizeOf(tRt_PoolItem));                          // Vamos al PayLoad = tRt_PoolItem
  l:=pointer(p1);
  inc(l.pool.tstack);                                          // Retrocedemos tStack
  l.pool.tstack^:=p1;                                          // Asignamos el tRt_PoolItem como el proximo elemento disonible
end;


function  pl_item(pl:pointer;item:cardinal):pointer;            // Obtiene un item
var p:pRt_Pool;
    k:ppointer;
begin
  p:=pl;
  item:=item+1;
  if item>=p.count then begin result:=nil;exit;end;
  k:=p.stack;inc(k,item);
  result:=@pRt_PoolItem(k^).data;
end;




function str_Destroy(str:pointer):pointer;
begin
  if str<>nil then begin
      if assigned(pRt_Stream(str).str_clean) then pRt_Stream(str).str_clean(str);
      m_free(pRt_Stream(str).Memory);
      end;
  result:=m_free(str);
end;

function str_GetPosition(str:pRt_Stream):integer;
begin
  if str=nil then result:=0 else result:=str.fPosition;
end;


function str_memory(str:pointer;value:longint):pointer;
var st:pRt_Stream;
begin
  if str=nil then begin result:=nil;exit;end;
  st:=str;
  if value<0        then result:=pChar(st.Memory)+st.fPosition else
  if value>st.fSize then result:=nil else result:=pChar(st.Memory)+value;
end;

function str_Clear(str:pointer):cardinal;
var st:pRt_Stream;
begin
  if str=nil then begin result:=0;exit;end;
  st:=str;result:=st.fSize;
  st.fSize:=0;st.fPosition:=0;
end;


procedure str_SetPosition(str:pRt_Stream;value:integer);
begin
  if value>str.fSize then value:=str.fSize;
  if value<0 then value:=0;
  str.fPosition:=value;
end;

function str_Size(str:pointer):cardinal;
begin
  result:=pRt_Stream(str).fSize;
end;

procedure str_SetCapacity(str:pRt_Stream;value:cardinal);
begin
  if str=nil then exit;
  str.fCapacity := value;
  str.Memory:=m_realloc(str.Memory,value);
  if str.fPosition>=str.fCapacity then str.fPosition := str.fCapacity-1;  // Si hemos recortado
  if str.fSize>=str.fCapacity     then str.fSize := str.fCapacity-1;      // Si hemos recortado
end;


procedure str_SetSize(str:pRt_Stream;Value: cardinal);
begin
  if str=nil then exit;
  if Value>str.fCapacity then str_SetCapacity(str,Value+16384);   // reserve some space for inplace growing
  str.fSize := Value;
end;

procedure str_Grow(str:pRt_Stream;value:integer);
begin
  if value>65536 then str_SetCapacity(str,value+value shr 2) else str_SetCapacity(str,value+16384); // growing by 16KB chunck up to 64KB, then by 1/4 of size
end;


function str_MemoryWrite(str:pointer;Buffer:pchar; Count:cardinal):cardinal;
var Pos: integer;
    st:pRt_Stream;
begin
  st:=str;if (st=nil) or (count=0) then begin result:=0;exit;end;
  Pos:=st.FPosition+Count;                              // Donde acabariamos
  if Pos>st.FSize then begin                            // Si es mas que el Size actual
            if Pos>st.FCapacity then str_Grow(st,pos);
            st.FSize:=Pos;
            end;
  Move(Buffer^, (pChar(st.Memory)+st.FPosition)^, Count);
  st.FPosition := Pos;
  result:=pos;
end;



function str_MemoryRead(str:pointer;Buffer:pointer;count:cardinal):longint;    // Por implementacion siempre devuelve >0
var st:pRt_Stream;
begin
  if (str=nil) then begin result:=0;exit;end;
  st:=str;if st.Memory=nil   then begin result:=0;exit;end;
  if st.fPosition>=0 then begin
        result:=st.FSize-st.FPosition;                                  // Este es el espacio que nos queda por leer
        if result>0 then begin                                          // Si nos queda algo por leer
              if result>count then result:=count;                       // Como maximo lo que nos quede
              Move((pChar(st.Memory)+st.FPosition)^,Buffer,result);     // Sencillamente lo copiamos
              Inc(st.FPosition, result);                                // y actualizamos la posicion
              end;
        end else result:=0;
end;

function str_Read(str:pointer;Buffer:pointer;count:cardinal):longint;                                 // Por implementacion siempre devuelve >0
begin
  if assigned(pRt_Stream(str).str_read) then result:=pRt_Stream(str).str_read(str,buffer,count) else result:=-1;
end;

function str_Write(str:pointer;Buffer:pchar; Count:cardinal):longint;
begin
  if assigned(pRt_Stream(str).str_write) then result:=pRt_Stream(str).str_write(str,buffer,count) else result:=-1;
end;

function str_create(size:cardinal):pointer;
var tr:pRt_Stream;
begin
  tr:=m_alloc(SizeOf(tRt_Stream));
  tr.data:=nil;
  tr.str_clean:=nil;
  tr.fPosition:=0;
  tr.fSize:=0;
  tr.fCapacity:=0;
  tr.Memory:=nil;
  str_SetCapacity(tr,size);
  result:=tr;
end;

function str_asArrayOfPointer(size:cardinal):pointer;
begin
  result:=str_create(size*SizeOf(cardinal));
end;

function str_asArrayOfPointerWrite(str:pointer;c:Pointer):cardinal;
var st:pRt_Stream;
    pos:integer;
    k:ppointer;
begin
  st:=str;Pos:=st.FPosition+4;                                                                  // Donde acabariamos = pointer = 4 bytes
  if Pos>st.FSize then begin if Pos>st.FCapacity then str_Grow(st,pos);st.FSize:=Pos;end;       // Si es necesario crecer
  k:=pointer(pChar(st.Memory)+st.FPosition);k^:=c;
  st.FPosition := Pos;
  result:=pos;
end;

function str_asArrayOfPointerGet(str:pointer;index:cardinal):Pointer;
var k:pPointer;
begin
  index:=index*SizeOf(Pointer);
  k:=str_memory(str,index);
  if k<>nil then result:=k^ else result:=nil;
end;

function  split_create(p:pchar;c:char):pointer;
var sp:pRt_Splitter;
    p1:pchar;
begin
  sp:=m_allocz(SizeOf(TRt_Splitter));result:=sp;
  sp.str:=p;
  sp.count:=0;
  sp.items:=str_asArrayOfPointer(15);                                   // Suponemos que tenemos 15, sino es asi ya crecera mas
  if p=nil then exit;
  p1:=p;
  while p^<>#0 do
        if p^=c then begin
                        str_asArrayOfPointerWrite(sp.items,p1);p^:=#0;
                        inc(p);p1:=p;inc(sp.count);end
                        else inc(p);
  str_asArrayOfPointerWrite(sp.items,p1);
end;

function  split_count(sp:pointer):cardinal;
begin
  result:=pRt_Splitter(sp).count;
end;


function  split_get(sp:pointer;c:cardinal):pchar;
begin
  result:=str_asArrayOfPointerGet(pRt_Splitter(sp).items,c);
end;

function split_destroy(sp:pointer):pointer;
begin
  if sp<>nil then begin str_destroy(  pRt_Splitter(sp).items);m_free(sp);end;
  result:=nil;
end;

function  ctx_NewString(c:pRt_Var;p:pchar;r:pRt_Var):boolean;
var cw:Trt_Var;
    pa:array[0..1] of TRt_Var;
begin
  // Convertimos value en un opt_ExtpChar asociado al contexto
  v_set(@pa[0],opt_VarList,pointer(1));v_set(@pa[1],opt_ConstPchar,p);
  result:=c.v_ct.lib_create(c,v_set(@cw,opT_ExtPChar,nil),@pa,r)=FW_OK;
end;


function strl_create:pointer;
var p1:pRt_StrItem;
begin
  p1:=m_alloc(SizeOf(tRt_StrItem));
  p1.str:='';p1.next:=nil;
  result:=p1;
end;

function strl_push(l:pointer;s:string):pointer;
var p1,p2:pRt_StrItem;
begin
  result:=l;if s='' then exit;
  p2:=m_alloc(SizeOf(tRt_StrItem));p2.str:=s;p2.next:=nil;
  p1:=l;while (p1.next<>nil) do p1:=p1.next;
  p1.next:=p2;
end;

function strl_pop(l:pointer):string;
var p1,p2:pRt_StrItem;
begin
  p1:=l;
  if (p1=nil) or (p1.next=nil) then begin result:='';exit;end;
  p2:=p1.next;
  while p2.next<>nil do begin p1:=p2;p2:=p2.next;end;
  p1.next:=nil;
  result:=p2.str;p2.str:='';m_free(p2);
end;

function strl_free(l:pointer):pointer;
var p1,p2:pRt_StrItem;
begin
  p1:=l;
  while p1<>nil do begin
        p2:=p1;p1:=p1.next;
        p2.str:='';m_free(p2);
        end;
  result:=nil;
end;

function strl_get(l:pointer;i:cardinal):string;
var p1:pRt_StrItem;
begin
  p1:=l;p1:=p1.next;
  while (p1<>nil) and (i>0) do begin p1:=p1.next;i:=i-1;end;
  if p1=nil then result:='' else result:=p1.str;
end;

function strl_join(l:pointer;sep:string):string;
var p1:pRt_StrItem;
    k,m1,m2:longint;
begin
  p1:=l;p1:=p1.next;m2:=length(sep);k:=0;m1:=0;
  while (p1<>nil) do begin
        k:=k+Length(p1.str)+m2;
        p1:=p1.next;
        end;
  if k=0 then begin result:='';Exit;end;
  SetLength(result,k+1);
  p1:=l;p1:=p1.next;k:=1;
  while (p1<>nil) do begin
        m1:=length(p1.str);Move(p1.str[1],result[k],m1);k:=k+m1;
        p1:=p1.next;
        if (p1<>nil) and (m2>0) then begin Move(sep[1],result[k],m2);k:=k+m2;end;
        end;
  result[k]:=#0;
end;


function ctx_NewObject(c,res:pRt_Var):boolean;
var w1:TRt_Var;
begin
  result:=c.v_ct.lib_create(c,v_set(@w1,opt_Object,nil),nil,res)=FW_OK;
end;

function ctx_NewPropertyStr(c,o:pRt_Var;name,value:pchar):boolean;
var w1:TRt_Var;
    p:array[0..3] of TRt_Var;
begin
  p[0].kind:=opt_VarList;p[0].v_i:=1;
  result:=(o.kind=opt_Object) and (c.kind=opt_Context);if not result then exit;
  result:=ctx_NewString(c,value,@p[1]);if not result then exit;
  result:=c.v_ct.lib_create(o,v_set(@w1,opt_ConstPchar,name),@p[0],nil)=FW_OK;
end;

function ctx_NewProperty(c,o:pRt_Var;name:pchar;value:prt_Var):boolean;
var w1:TRt_Var;
    p:array[0..3] of TRt_Var;
begin
  result:=(o.kind=opt_Object) and (c.kind=opt_Context);if not result then exit;
  v_set (@p[0],opt_VarList,pointer(1));
  v_copy(@p[1],value);
  result:=c.v_ct.lib_create(o,v_set(@w1,opt_ConstPchar,name),@p[0],nil)=FW_OK;
end;




initialization

  //S_alert('%vm_utils init01');
  flags_values[0,false]:=0; flags_values[0,true]:=vF_EqOn;
  flags_values[1,false]:=0; flags_values[1,true]:=vF_BigOn;
  flags_values[2,false]:=0; flags_values[2,true]:=vF_LessOn;

  Init;
  //S_alert('%vm_utils init02');

  my_Heap:=HeapCreate(0,128*1024,0);
finalization
  if m_leak then s_alert(m_report);

  HeapDestroy(my_Heap);

end.

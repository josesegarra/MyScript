unit fw_vm_constants;

interface

uses fw_vm_types,fw_system_types;

const
        // Flags de funcionamiento de la VM: Mode_Flags
        opV_DebugOn     =$000001;  // 000000001
        opV_DebugOff    =$FFFFFE;  // 111111110
        opV_StopOn      =$000002;  // 000000010
        opV_StopOff     =$FFFFFD;  // 111111101

        // Estos son los tipos que puede ver el mundo exterior sobre los datos de una VM
        opT_None              =cardinal($FFFF0001);  // Nada
        //opt_None_AsPointer    =pointer(opt_None);


        opT_Integer           =cardinal($FFFF0002);  // Entero
        opT_ExtPChar          =cardinal($FFFF0004);  // Es un Pchar extendido y gestionado por un GC, es decir el primer cardinal tiene la longitud
        opt_ConstPchar        =cardinal($FFFF0008);  // Esto es un Pchar constante
        opt_Boolean           =cardinal($FFFF0010);  // Booleano
        opt_Object            =cardinal($FFFF0020);  // Objeto
        opt_ExternF           =cardinal($FFFF0040);  // ExternF
        opt_Null              =cardinal($FFFF0080);  // Null

        opt_VarList           =cardinal($FFFF0100);  // Lista de variables
        opt_Iterator          =cardinal($FFFF0200);  // Iterador
        opt_PropCache         =cardinal($FFFF0400);  // Cache de propiedades

        
        // Estos son los tipos que puede ver el mundo exterior sobre el estado y configuracion de una VM
        opt_Library           =cardinal($FFFF8001);
        opt_Block             =cardinal($FFFF8002);  // Bloque de codigo
        opt_JumpR             =cardinal($FFFF8003);  // Salto de codigo
        opt_Label             =cardinal($FFFF8004);  // Label solo se usa en tiempo de compilacion
        opt_Offset            =cardinal($FFFF8005);  // Offset
        opt_Name              =cardinal($FFFF8010);  // Un nombre
        opt_Code              =cardinal($FFFF8011);  // Un programa compilado
        opt_Context           =cardinal($FFFF8012);  // Un contexto de ejecucion
        opt_GC                =cardinal($FFFF8013);  // Un GC
        opt_Ivar              =cardinal($FFFF8014);  // Variable libre o indirecta. EL valor es un puntero a un tRt_Var
        opt_Runner            =cardinal($FFFF8015);  // Entorno de ejecucion
        opt_Compiler          =cardinal($FFFF8016);  // Entorno de compilacion
        opt_Debug             =cardinal($FFFF8017);  // Funcion de debug. Es un ExternF
        opt_Warning           =cardinal($FFFF8018);  // Mensaje de Warning 
        opt_Hint              =cardinal($FFFF8019);  // Hint
        opt_Info              =cardinal($FFFF8020);  // Mensaje informativo
        opt_Source            =cardinal($FFFF8021);  // Codigo fuente
        opt_Index             =cardinal($FFFF8022);  // Index
        opt_HostData          =cardinal($FFFF8023);  // Host Object
        opt_HostControl       =cardinal($FFFF8024);  // Host Control
        opt_Buffer            =cardinal($FFFF8025);  // Un buffer: implementado con b_create en fw_utils
        
        opt_Flag              =cardinal($FFFF8101);  // Flags
        opt_FrameCall         =cardinal($FFFF8102);  // Call frame
        opt_FrameError        =cardinal($FFFF8108);  // Error frame
        opt_FrameDetour       =cardinal($FFFF8109);  // Detour frame
        opT_Gvar              =cardinal($FFFF8110);  // Variable global
        opT_CatchVar          =cardinal($FFFF8111);  // Variable de catch
        opt_Var               =cardinal($FFFF8114);  // Variable local
        opt_Reg               =cardinal($FFFF8115);  // Registro
        opt_Cvar              =cardinal($FFFF8118);  // Variable capturda por una funcion. EL valor es un indice a la lista de variables libres de la funcion actual
        opt_Nan               =cardinal($FFFF8119);  // Not A Number
        opt_Param             =cardinal($FFFF8120);  // Parametro enviado
        opt_Rparam            =cardinal($FFFF8121);  // Parametro recibido 
        opt_Closure           =cardinal($FFFF8122);  // Closure List
        opt_Error             =cardinal($FFFF8124);  // Objeto Error
        opt_Unknown           =cardinal($FFFF8127);  // Variable sin definir ni importar (autogenerada) porque se usa en algun sitio
        opt_GetSet            =cardinal($FFFF8128);  // Getter Setter
        opt_FuncDelayed       =cardinal($FFFF8129);  // Una funcion delayed - Se utiliza al iniciar el RTL no se crean Objetos de todas las funciones, sino referencias para crearlos en la primera invocacion
        opt_OwnerShip         =cardinal($FFFF8130);  // Ajusta la ownership - se utiliza en la transferencia de objetos entre contextos (cuando RUN devuelve un Objeto)

        opt_Single            =cardinal($FFFF8200);  // Singleton(s)
        opS_CurF              =longint(1);           // Current function
        opS_This              =longint(2);           // Current this
        ops_Result            =longint(3);           // Resultado


        
        // Los opcodes de la maquina virtual
        op_NOP              =$00;         // NOP
        op_Move             =$01;         // Move                             
        op_NewObject        =$02;         // Crea un objeto
        op_NewFunc          =$03;         // Crea un objeto funcion
        op_CreateParams     =$04;         // Crea un nuevo Params_Frame
        op_Call             =$05;         // Call
        op_Extern           =$06;         // Define & get extern value
        op_Define           =$07;         // Define value
        op_GetProperty      =$08;         // Obtiene la direccion de la propiedad de un objeto, si no la encuentra hereda
        op_SetProperty      =$0A;         // Establece la propiedad de un objeto
        op_Ret              =$0B;         // Salida de funcion, si estamos dentro de un bloque finally lleva el flag NoEnd, sino lleva el flag End
        op_Jump             =$0E;         // Salto
        op_Link             =$0F;         // Linkea una variable local con una capturada
        op_GLink            =$10;         // Linkea una variable local a un Global
        op_NewArray         =$11;         // Crea un objeto Array
        op_Label            =$13;         // Es virtual se trata de un label
        op_JumpR            =$14;         // Salto RESULT -> Si se produce el salto se activa RESULT sino se queda como esta
        op_SetOn            =$15;         // Pone a 0/1 segun el flag que haya
        op_NewIter          =$16;         // Obtiene un iterador de objeto (propiedad)
        op_IterNext         =$17;         // El siguiente iterador
        op_Construct        =$18;         // Este es para borrar !!!  a=new F() -> Se debe reescribir a nivel de GenCode como a={};F.call(a);
        op_PushFrame        =$19;         // Push Frame
        op_PopFrame         =$1A;         // Pop  Frame
        op_DestroyParams    =$1B;         // Popea los parametros
        op_PatchFrame       =$1C;         // Parchea un frame que esta en el stack
        op_GetCache         =$1D;
        op_SetCache         =$1E;
        op_Throw            =$1F;         // Interrumpe la ejecucion del programa
        op_GCVar            =$20;         // Crea una variable en el GC, hace que el primer parametro apunte a la misma y le da el valor del segundo parametro
        op_Setter           =$21;         // Define un setter
        op_Getter           =$22;         // Define un getter
        op_ToObject         =$23;         // Convierte a Objeto
        opcode__min         =$00;
        opcode__max         =$23;



        // Los opcodes de operadores son > 100
        op_Negative         =107;
        op_Not              =104;
        op_TypeOf           =108;
        
        op_Times            =141;
        op_Div              =142;
        op_Mod              =143;
        op_add              =144;
        op_Minus            =145;
        op_shiftl           =146;
        op_shiftr           =147;
        op_shiftrz          =148;
        op_Compare          =149;         // Comparacion normal
        op_StrictCompare    =150;         // Comparacion estricta
        op_Instance         =151;


        // Descriptores de Flags cuando kind=opT_Flag. Los TRUE son pares los negativos NO
        opF_BADFLAG    =-1;
        opF_NoFlag     =00;
        opF_min        =02;
        opF_Eq         =02;
        opF_NEq        =03;
        opF_Big        =04;
        opF_NBig       =05;
        opF_BigEq      =06;
        opF_NBigEq     =07;
        opF_Less       =08;
        opF_NLess      =09;
        opF_LessEq     =10;
        opF_NLessEq    =11;

        opF_True       =12;
        opF_False      =13;
        opF_InMainBody =16; // Cuando se ejecuta un RET si el primer parametro es este FLAG, dice que el RET corresponde al MAIN BODY del prt_oCode
        opF_WithReturn =32; // Cuando se ejecuta un RET si el segundo parametro es este FLAG, quiere decir que el RET proviene de un statement RET y que por tanto hay una expresion que devolver

        // Tipos de frames que podemos cuando en el stack tenemsos un Opt_ErrorFrame
        opFr_TryFrame            =01;      // Try Frame
        opFr_CatchFrame          =02;      // Catch frame
        opFr_FinalFrame          =03;      // FinalFrame
        
        

        // Indicadores en el registro Flags
        // Tenemos los siguientes flags:
        //    Eq:     Binario 001 Decimal 1. Mascara  Binario 110 -> Decimal 6
        //    Big:    Binario 010 Decimal 2. Mascara  Binario 101 -> Decimal 5
        //    Less:   Binario 100 Decimal 4. Mascara  Binario 011 -> Decimal 3
        vF_EqOn       =1;
        vF_BigOn      =2;
        vF_LessOn     =4;
        // Flag de salto, refleja el valor de la ultima ejecucion de JUMPR
        vF_JumpED     =8;
        vf_JumpEDMask =7;
        vF_EndED      =16;
        vF_EndEDMask  =15;



        opT_MaskNonReal       =cardinal($FFFF0000);  // Mascara que cumplen todos los NO REAL
        opt_Mask0NoReal       =cardinal($FFFF0000);
        opt_Mask1Constant     =cardinal($FFFFFF00);  // Si    valor & opt_Mask1Constant = Mask0NoReal se trata de una constante (none,integer,pchar,boolean)
        opt_Mask2Real         =cardinal($FFFF0000);  // Si    valor & opt_Mask2Real <> opt_Mask2Real se trata de un real




        // Gestion de Objeto
        kp_Writable      =$0000001;   // Si existe se puede escribir
        kp_Enumerable    =$0000002;   // Si existe se puede enumerar
        kp_Configurable  =$0000004;   // Si exoste se puede cambiar los atributos de la misma

        kp_DefaultProp   =kp_Writable or kp_Enumerable or kp_Configurable;
        

        



function vm_f2str(k:cardinal):string;
function vm_ef2str(k:cardinal):string;
function vm_t2str(k:cardinal):string;
function vm_o2str(k:cardinal):string;
function vm_tv2str(t:cardinal;v:longint):string;
function vm_code2str(o:pRt_oOpcode):string;
function vm_v2str(v:pRt_Var):string;
function vm_text2str(v:pRt_Var):string;
function vm_text2strl(v:pRt_Var;i:longint):string;


implementation


var SingleNames:array[1..3] of string=('{CurF}','{This}','{Result}');

//opS_CurF              =longint(1);           // Current function
//opS_This              =longint(2);           // Current this
//ops_Result            =longint(3);           // Resultado


function s_i2s(i:integer):string;
begin
  Str(i,Result);
end;

function s_p2s(i:pointer):string;
begin
  Str(longint(i),Result);
end;

function s_left(s:string;i:integer):string;
begin
  if s<>'' then result:=copy(s,1,i) else result:='';
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
end;


function  s_spaces(i:longint):string;
begin
 if i<=0 then begin result:='';exit;end;
 Setlength(result,i);
 while (i>0) do begin result[i]:=' ';i:=i-1;end;
end;


function  s_rpad(const s:string;i:longint):string;
begin
 result:=s_left(s+s_Spaces(i),i);
end;



function vm_t2str(k:cardinal):string;
begin
  if k=opT_None         then begin result:='none';exit;end;
  if k=opT_Integer      then begin result:='integer';exit;end;
  if k=opT_ExtPChar     then begin result:='ExtPchar';exit;end;
  if k=opt_Warning      then begin result:='Warning';exit;end;
  if k=opt_Hint         then begin result:='Hint';exit;end;
  if k=opt_Error        then begin result:='Error';exit;end;
  if k=opt_Info         then begin result:='Info';exit;end;

  if k=opt_ConstPchar   then begin result:='ConstPchar';exit;end;
  if k=opT_Flag         then begin result:='flag';exit;end;
  if k=opT_Object       then begin result:='object';exit;end;
  if k=opT_Iterator     then begin result:='iter';exit;end;
  if k=opT_Boolean      then begin result:='boolean';exit;end;
  if k=opT_ExternF      then begin result:='externF';exit;end;
  if k=opt_Context      then begin result:='context';exit;end;
  if k=opt_Unknown      then begin result:='unknown';exit;end;
  if k=opt_Null         then begin result:='null';exit;end;
  if k=opt_GetSet       then begin result:='GetSet';exit;end;
  if k=opt_Library      then begin result:='Library';exit;end;
  if k=opt_GC           then begin result:='GC';exit;end;
  if k=opt_Compiler     then begin result:='Compiler';exit;end;
  if k=opt_Code         then begin result:='CODE';exit;end;


  if k=opT_Var          then begin result:='VAR_';exit;end;
  if k=opt_Debug        then begin result:='DEBUG_';exit;end;
  if k=opt_Name         then begin result:='NAME_';exit;end;
  if k=opt_Source       then begin result:='SRC_';exit;end;

  if k=opT_Block        then begin result:='BLOCK_';exit;end;
  if k=opT_Param        then begin result:='PARAM_';exit;end;
  if k=opT_RParam       then begin result:='RPARAM_';exit;end;
  if k=opT_GVar         then begin result:='GVAR_';exit;end;
  if k=opT_Reg          then begin result:='REG_';exit;end;
  if k=opT_JumpR        then begin result:='JUMPR_';exit;end;
  if k=opT_Label        then begin result:='LABEL_';exit;end;
  if k=opT_Block        then begin result:='BLOCK_';exit;end;
  if k=opt_FrameError   then begin result:='ERRFRAME_';exit;end;
  if k=opt_FrameCall    then begin result:='CALLFRAME_';exit;end;
  if k=opt_Single       then begin result:='[Single]';exit;end;
  if k=opt_CVar         then begin result:='CVAR';exit;end;
  if k=opt_Offset       then begin result:='OFFSET_';exit;end;
  if k=opt_Runner       then begin result:='RUNNER_';exit;end;
  if k=opT_CatchVar     then begin result:='CATCHVAR_';exit;end;
  if k=opt_FuncDelayed  then begin result:='FUNC_DELAY_';exit;end;
  if k=opt_Closure      then begin result:='CLLIST_';exit;end;
  if k=opt_VarList      then begin result:='VARLIST_';exit;end;
  if k=opt_Ivar         then begin result:='IVAR_';exit;end;
  if k=opt_OwnerShip    then begin result:='OWNERSHIP_';exit;end;


  if (k and opt_Mask2Real)<>opt_Mask2Real  then begin result:='float';exit;end;


  result:='NOT KNOWN: '+s_i2s(k)+' ['+s_i2h(k)+']';
end;

function vm_v2str(v:pRt_Var):string;
begin
  if v=nil then result:='*** NULL ***' else result:=vm_tv2str(v.kind,v.v_i);
end;


function vm_FloatToString(k1:cardinal;v1:longint):string;
var k:^double;
    e1,e2,e3:extended;
    l:array[0..1] of longint;
    b:boolean;
    s:string;
begin
  l[0]:=v1;l[1]:=k1;
  k:=@l[0];
  Str(k^,result);
  e1:=int(k^);        // La parte entera
  result:='';
  if e1<0 then begin b:=true;e1:=-e1;end else b:=false;

  while e1<>0 do begin
        e2:=trunc(e1/10);                   // Esto es la division entera
        e3:=e1-e2*10;                       // Esto es el resto
        result:=s_i2s(trunc(e3))+result;        // Acumulamos el resto
        e1:=e2;                             // Hacemos el shiftr que es equivalente a dividir entre 10
        end;
  if result='' then result:='0';
  if b then result:='-'+result;

  e1:=Frac(k^)*1000;
  s:=s_i2s(round(e1*100) div 100);
  while length(s)<3 do s:=s+'0';
  result:=result+'.'+s;
end;


function vm_ef2str(k:cardinal):string;
begin
 if k=opFr_TryFrame    then result:='TryFrame'   else
 if k=opFr_CatchFrame  then result:='CatchFrame' else
 if k=opFr_FinalFrame  then result:='FinalFrame'  else
 result:='Bad_Error_Frame: '+s_i2s(k)+'  '+s_i2h(k);
end;


function vm_text2str(v:pRt_Var):string;
begin
 result:='';if v=nil then exit;
 if (v.kind=opt_ConstPchar) or (v.kind=opt_Name) then result:=pchar(v.v_pc) else
 if (v.kind=opT_ExtPChar)  then result:=pchar(@(pRt_ExPchar(v.v_p).data));
 if length(result)>63 then result:=s_left(result,60)+'..';
end;

function vm_text2strl(v:pRt_Var;i:longint):string;
begin
 result:='';if v=nil then exit;
 if (v.kind=opt_ConstPchar) or (v.kind=opt_Name) then result:=pchar(v.v_pc) else
 if (v.kind=opT_ExtPChar)  then result:=pchar(@(pRt_ExPchar(v.v_p).data));
 if i>0 then begin
      if length(result)>i+3 then result:=s_left(result,i)+'..';
      end;
end;

function vm_tv2str(t:cardinal;v:longint):string;
var s:string;
begin
  result:=vm_t2str(t);if result='' then exit;
  if (t=opt_Warning) or (t=opt_Hint) or (t=opt_Error)  or (t=opt_Info)      then exit;

  if (t=opt_Single)       then begin result:=SingleNames[v];exit;end;
  if (t=opt_FrameError)   then begin result:='['+result+':'+vm_ef2str(v)+']';exit;end;
  if (t=opt_ConstPchar)   then begin result:='['+result+':'+s_i2s(v);
                                     if v<>0 then s:=pchar(v) else s:='';
                                     if length(s)>43 then s:=s_left(s,40)+'..';
                                     result:=result+' "'+s+'"]';
                                     exit;
                                     end;
  if (t=opt_Name)   then begin result:='['+result+':'+s_i2s(v);
                                     if v<>0 then s:=pchar(v) else s:='';
                                     if length(s)>43 then s:=s_left(s,40)+'..';
                                     result:=result+' "'+s+'"]';
                                     exit;
                                     end;
  if (t=opT_ExtPChar)     then begin result:='['+result+':'+s_i2s(v);s:='';
                                     if (v<>0) then s:=pchar(@(pRt_ExPchar(v).data));
                                     if length(s)>43 then s:=s_left(s,40)+'..';
                                     result:=result+' "'+s+'"]';
                                     exit;
                                     end;
  if (t and opt_Mask2Real)<>opt_Mask2Real  then  begin result:='['+result+':'+vm_FloatToString(t,v)+']';exit;end;
  result:='['+result;
  if result[length(result)]<>'_' then begin
          result:=s_rpad(result,10);
          if (t=opt_Library) or (t=opt_Runner) or  (t=opt_Compiler) or (t=opt_Debug) or
             (t=opt_GC) or  (t=opt_Block) or (t=opt_ExternF) or (t=opt_Ivar) or (t=opt_Code)
             then result:=result+' @'
             else result:=result+' :';
          end;
  result:=result+s_i2s(v)+']';
end;

function vm_o2str(k:cardinal):string;
begin
  case k of
        op_NOP            : result:='NOP';
        op_Move           : result:='MOVE';
        op_ToObject       : result:='TO_OBJECT';
        op_NewObject      : result:='OBJECT';
        op_NewFunc        : result:='FUNCTION';
        op_CreateParams   : result:='NEW_PARAM_LIST';
        op_Setter         : result:='SETTER';
        op_Getter         : result:='GETTER';
        op_Call           : result:='CALL';
        op_GLink          : result:='GLINK';
        op_GCVar          : result:='GC_VAR';
        op_Throw          : result:='THROW';
        op_Extern         : result:='DEFINE_EXTERN';
        op_Define         : result:='DEFINE';
        op_GetProperty    : result:='GET_PROP';
        op_SetProperty    : result:='SET_PROP';
        op_Ret            : result:='RET';
        op_Instance       : result:='INSTANCE';
        op_Compare        : result:='CMP';
        op_StrictCompare  : result:='STRICT_CMP';
        op_Jump           : result:='JMP';
        op_Link           : result:='LINK';
        //op_BlockInit      : result:='BLOCK_INIT';
        op_Label          : result:='LABEL';
        op_JumpR          : result:='JMP_R';
        op_SetOn          : result:='SET';
        op_NewIter        : result:='ITERATOR';
        op_IterNext       : result:='ITER_NEXT';
        op_Construct      : result:='CONSTRUCT';
        op_PushFrame      : result:='PUSH_FRAME';
        op_PopFrame       : result:='POP_FRAME';
        op_DestroyParams  : result:='DESTROY_PARAM_LIST';
        op_NewArray       : result:='CREATE_ARRAY';
        
        op_PatchFrame     : result:='PATCH_FRAME';
        op_GetCache       : result:='GET_CACHE';
        op_SetCache       : result:='SET_CACHE';

        op_Negative       : result:='NEGATIVE';
        op_TypeOF         : result:='TYPEOF';
        op_NOt            : result:='NOT';
        op_Times          : result:='TIMES';
        op_Div            : result:='DIV';
        op_Mod            : result:='MOD';
        op_Add            : result:='ADD';
        op_Minus          : result:='MINUS';
        op_shiftl         : result:='SHIFTL';
        op_shiftr         : result:='SHIFTR';
        op_shiftrz        : result:='SHIFTRZ';
        else result:=s_i2s(k);
        end;
end;



function vm_f2str(k:cardinal):string;
begin
  result:='';
  if k=opF_Eq         then result:='EQ' else
  if k=opF_NEq        then result:='NEQ' else
  if k=opF_Big        then result:='BIG' else
  if k=opF_Nbig       then result:='NBIG' else
  if k=opF_bigEq      then result:='BIGEQ' else
  if k=opF_NbigEq     then result:='NBIGEQ' else
  if k=opF_Less       then result:='LESS' else
  if k=opF_NLess      then result:='NLESS' else
  if k=opF_LessEq     then result:='LESSEQ' else
  if k=opF_NLessEq    then result:='NLESSEQ' else
  if k=opF_InMainBody then result:='MAIN' else
  if k=opF_WithReturn then result:='KEEP_RET' else
  if k=opF_True       then result:='FLAG_TRUE'  else
  if k=opF_False      then result:='FLAG_FALSE'  

  else result:='**BAD_FLAG**';
end;

function vm_code2str(o:pRt_oOpcode):string;
begin
 result:=s_rpad(s_p2s(o),15)+'   '+s_rpad(vm_o2str(o.opcode),15);
 if o.k1<>opT_None then result:=result+vm_tv2str(o.k1,o.v1);
 if o.k2<>opT_None then result:=result+','+vm_tv2str(o.k2,o.v2);
end;

end.

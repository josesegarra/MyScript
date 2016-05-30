unit js_codeoptimizer;

interface

uses js_gencode,js_codeformat,js_tokens,fw_utils,fw_vm_types,fw_system_types,fw_vm_constants;

type
   

    TJS_PeepHoleF=function (d:pJs_CodeNode):pJs_CodeNode of object;

    TJS_OptCodeGen=class(TJs_CodeGen)
    private
        function  GetDelta(desde,hasta:pJs_CodeNode):longint;
        procedure CleanLabels(l:pointer;d:pJs_CodeNode);
        function  NextNonLabel(d:pJs_CodeNode):pJs_CodeNode;
        procedure FixLabels;

        function  CheckPeepHoled(d:pJs_CodeNode):TJS_PeepHoleF;
        function  PH_SimpleMove(d:pJs_CodeNode):pJs_CodeNode;
        function  PH_SimpleJump(d:pJs_CodeNode):pJS_CodeNode;
        function  PH_SimpleSetGet(d:pJs_CodeNode):pJS_CodeNode;
        function  PH_SimpleSetGet2(d:pJs_CodeNode):pJS_CodeNode;
        function  PH_SimpleJump1(d:pJs_CodeNode):pJS_CodeNode;
        function  PH_ChainGetProp(d:pJs_CodeNode):pJS_CodeNode;
        function  PH_DoubleRet(d:pJs_CodeNode):pJS_CodeNode;
        function  IsUsed(k1:cardinal;v1:longint;d:PJs_CodeNode):boolean;
        function  PeepHoleRun:longint;
        procedure CleanNops;
        //function  Gen_Method(d:pJS_CodeNode):pJs_CodeNode;
        procedure LogOptimizer(s:string;k:pointer);
    public
        optimizer_log:string;
        function  Peephole:boolean;
    end;


implementation

uses js_lexer;

const d_BinOp=-1;

function IsThis(k:cardinal;v:longint):boolean;
begin
  result:=(k=opt_Single) and (v=opS_This);
end;

function IsResult(k:cardinal;v:longint):boolean;
begin
  result:=(k=opt_Single) and (v=ops_Result);
end;

function IsNull(k:cardinal;v:longint):boolean;
begin
  result:=(k=opt_Null);
end;

function OpcodeIs(d:pJS_CodeNode;k:longint):boolean;
begin
  if d=nil then begin result:=False;exit;end;
  if k>0 then begin result:=d.opcode=k;exit;end;
  if k=d_BinOp then begin result:=(d.opcode>opcode__max) and js_kindIsBinOp(d.opcode);exit;end;
  result:=false;
end;

function OpcodePIs(d:pJS_CodeNode;p:longint;k:cardinal;v:longint):boolean;
var k1,v1:longint;
    b:boolean;
begin
  if d=nil then begin result:=False;exit;end;
  if p=1 then begin k1:=d.k1;v1:=d.v1;end else begin k1:=d.k2;v1:=d.v2;end;
  result:=k1=d.k1;
  b:=(k=opt_None) or (isThis(k,v)) or (isResult(k,v)) or (isNull(k,v));
  if result and (not b) then result:=result and (v1=d.v1);
end;

function OpcodeP1P2(d1,d2:pJS_CodeNode;p1,p2:longint):boolean;
var k1,v1,k2,v2:longint;
    b:boolean;
begin
  if (d1=nil) or (d2=nil) then begin result:=False;exit;end;
  if p1=1 then begin k1:=d1.k1;v1:=d1.v1;end else begin k1:=d1.k2;v1:=d1.v2;end;
  if p2=1 then begin k2:=d2.k1;v2:=d2.v1;end else begin k2:=d2.k2;v2:=d2.v2;end;
  result:=k1=k2;
  b:= // (k1=opt_None) or
     (isThis(k1,v1)) or (isResult(k1,v1)) or (isNull(k1,v1));
  if result and (not b) then result:=result and (v1=v2);
end;


function TJS_OptCodeGen.PeepHoleRun:longint;
var d:pJs_CodeNode;
    k:longint;
    cb:pJs_Function;
    f:TJS_PeepHoleF;
begin
  k:=l_count(programa.funcs)-1;result:=0;
  while (k>=0) do begin
        cb:=l_get(programa.funcs,k);d:=cb.first;
        while d<>nil do begin f:=CheckPeepHoled(d);if  assigned(f) then begin d:=f(d);inc(result);end else d:=d.next;end;
        k:=k-1;
        end;
  CleanNops;
end;



function TJS_OptCodeGen.NextNonLabel(d:pJs_CodeNode):pJs_CodeNode;
begin
  result:=nil;
  while (result=nil) and (d<>nil) do begin
          if (d.opcode<>op_Label) and (d.opcode<>op_Nop) then result:=d;
          d:=d.next;
          end;
  if result=nil then c_Error('Label can''t be end of block',nil);
  
end;

function TJS_OptCodeGen.GetDelta(desde,hasta:pJs_CodeNode):longint;
var p:pJs_CodeNode;
begin
  result:=0;

  if (desde=nil) or (hasta=nil) or (desde=hasta) then begin c_Error('Bad displacement fixing labels',nil);exit;end;
  result:=0;p:=desde;
  while (p<>nil) and (p<>hasta) do begin inc(result);p:=p.next;end;
  if p=nil then begin
          result:=0;p:=desde;while (p<>nil) and (p<>hasta) do begin dec(result);p:=p.prev;end;
          if p=nil then begin result:=0;c_Error('Label NOT found',nil);end;
          end;
end;


procedure TJS_OptCodeGen.CleanLabels(l:pointer;d:pJs_CodeNode);
begin
  while (error='') and (d<>nil) do begin

        if d.k1=opT_Label then begin d.k1:=opT_JumpR;d.v1:=GetDelta(d,l_get(l,d.v1));end;
        if d.k2=opT_Label then begin d.k2:=opT_JumpR;d.v2:=GetDelta(d,l_get(l,d.v2));end;
        d:=d.next;
        end;
end;


procedure TJS_OptCodeGen.FixLabels;
var k:longint;
    d:pJs_CodeNode;
    cb:pJs_Function;
    labels:pointer;
begin
  k:=l_count(programa.funcs)-1;
  while (k>=0) do begin
        cb:=l_get(programa.funcs,k);d:=cb.first;
        if cb.nlabels>0 then begin
            labels:=l_create(SizeOf(POINTER),cb.nlabels);
            end else labels:=nil;
        while (d<>nil) and (error='') do begin
                if d.opcode=op_Label then begin l_set(labels,d.v1,NextNonLabel(d));d:=js_program_DelCode(cb,d);end else d:=d.next;
                end;
        if labels<>nil then CleanLabels(labels,cb.first);
        labels:=l_free(labels);
        k:=k-1;
        end;
end;

procedure TJS_OptCodeGen.CleanNops;
var k:longint;
    d:pJs_CodeNode;
    cb:pJs_Function;
begin
  k:=l_count(programa.funcs)-1;
  while (k>=0) do begin
        cb:=l_get(programa.funcs,k);d:=cb.first;
        while d<>nil do if d.opcode=op_NOP then d:=js_program_DelCode(cb,d) else d:=d.next;
        k:=k-1;
        end;
end;

function  TJS_OptCodeGen.Peephole:boolean;
    var p0,p1,k:longint;
begin
  p0:=0;p1:=0;
  repeat
      k:=PeepHoleRun;
      p0:=p0+1;p1:=p1+k;
      until (k=0) or (p0=5);
  FixLabels;
  LogOptimizer('Optimization  #Passes: '+s_i2s(p0)+'    #Optimizations: '+s_i2s(p1),nil);
  result:=error='';
end;

function f_Match(k1,k2:array of cardinal):boolean;
var i:longint;
begin
  result:=(low(k1)=low(k2)) and (High(k1)=High(k2));if not result then exit;
  i:=low(k1);
  while result and (i<=high(k1)) do begin result:= k1[i]=k2[i];i:=i+1;end;
end;


// Nos dice si el opcode modifica o no a opt_Result
function f_KeepsResult(d:pJs_CodeNode):boolean;
begin
  // Todos estos opcodes afectan a result
  result:=not ((d.opcode=op_Ret) or (d.opcode=op_Jump) or (d.opcode=op_JumpR) or (d.opcode=op_Call) or (d.opcode=op_GetProperty) or js_tokenIsUnaryPreOp(d.opcode) or js_tokenIsUnaryPostOp(d.opcode) or js_kindIsBinOp(d.opcode));
  if not result then exit;              // Si es alguno de los anteriores salimos
  // EN cualquier otro caso si el primer parametro no es opT_result se supone que este permanece
  result:=not IsResult(d.k1,d.v1);
end;


procedure TJS_OptCodeGen.LogOptimizer(s:string;k:pointer);
begin
  optimizer_log:=optimizer_log+s;
  if k<>nil then optimizer_log:=optimizer_log+' IN '+s_p2s(k);
  optimizer_log:=optimizer_log+#13;
end;

 // MOVE           REG1,RESULT      --->
 //[OPERATOR]      YYYY,REG1
 // Se puede transformar en
 // [OPERATOR] YYY,RESULT

function TJS_OptCodeGen.PH_SimpleMove(d:pJs_CodeNode):pJs_CodeNode;
var d1:pJS_CodeNode;
    used,discarded:boolean;
begin
   result:=d.next.next;
   // Comprobamos que el registro no se use en el codigo posterior
   d1:=result;used:=false;discarded:=false;
   while (d1<>nil) and (used=false) and (discarded=false) do begin
            discarded:=(d1.opcode=op_Move) and (d1.k1=opT_reg) and (d1.v1=d.v1);
            used:=(discarded=false) and (   ((d1.k1=opT_reg) and (d1.v1=d.v1)) or ((d1.k2=opT_reg) and (d1.v2=d.v1)));
            d1:=d1.next;
            end;
   if used then exit;
   // Optimizamos
   d.opcode:=op_NOP;d.k1:=opT_None;d.k2:=opT_None;
   d.next.k2:=opt_Single;d.next.v2:=ops_Result;
   LogOptimizer('Optimization PH_SimpleMove',result);
end;

// JUMP           FLAGS,LABEL1
// JUMP           LABEL2
// LABEL1:
//
// Se puede transformar en
//      JUMP N_FLAGS,LABEL2
function TJS_OptCodeGen.PH_SimpleJump(d:pJs_CodeNode):pJS_CodeNode;
begin
  d.v1:=c_InvertFlag(d.v1);    // Invertimos el Flag
  d.v2:=d.next.v2;              // Sustituimos el destino
  d.next.opcode:=op_NOP;
  d.next.next.opcode:=op_NOP;
  result:=d.next.next.next;
  LogOptimizer('Optimization PH_SimpleJump',result);
end;

// Si tenemos esto:
//    SETPROPERTY    GLOBAL_1,PCHAR_12934308
//    GETPROPERTY    GLOBAL_1,PCHAR_12934308
// Se puede transformar en
//    SETPROPERTY    GLOBAL_1,PCHAR_12934308
// Esta Optimizacion no se puede aplicar si tenemos Getters !!
function TJS_OptCodeGen.PH_SimpleSetGet(d:pJs_CodeNode):pJS_CodeNode;
begin
  d.next.opcode:=op_Nop;result:=d.next.next;
  LogOptimizer('Optimization PH_SimpleSetGet',result);
end;



// Si tenemos
//    SETPROPERTY    GLOBAL_0,PCHAR_12994002
//    MOVE           REG_0,RESULT                       <--- Aqui puede ir cualquier cosa que NO escriba a result
//    GETPROPERTY    GLOBAL_0,PCHAR_12994002
function TJS_OptCodeGen.PH_SimpleSetGet2(d:pJs_CodeNode):pJS_CodeNode;
begin
  d.next.next.opcode:=op_Nop;result:=d.next.next.next;
  LogOptimizer('Optimization PH_SimpleSetGet2',result);
end;

// Si tenemos
//    JUMP [NO_FLAGS],LABEL_X
//    LABEL_X
//  Se puede transformar en
//    LABEL_X
function TJS_OptCodeGen.PH_SimpleJump1(d:pJs_CodeNode):pJS_CodeNode;
begin
  d.opcode:=op_Nop;result:=d.next.next;
  LogOptimizer('Optimization PH_SimpleJump1',result);
end;

// Si tenemos una cadena de propiedades
//    MOVE           REG_2,RESULT
//    GETPROPERTY    REG_2,PCHAR_13152299
//    MOVE           REG_1,RESULT
//    GETPROPERTY    REG_1,PCHAR_13152312
//    MOVE           REG_0,RESULT
// Podemos convertirla en:
//    GETPROPERTY    RESULT,PCHAR_13152299
//    Siempre y cuando el registro eliminado NO sea utilizado mas adelante

function TJS_OptCodeGen.PH_ChainGetProp(d:pJs_CodeNode):pJS_CodeNode;
begin
  if not IsUsed(d.k1,d.v1,d.next.next) then begin
          d.opcode:=op_NOP;d.next.k1:=opt_Single;d.next.v1:=ops_Result;result:=d.next.next;
          LogOptimizer('Optimization PH_ChainGetProp',result);
          end else result:=d.next;
end;

// Doble RET
function TJS_OptCodeGen.PH_DoubleRet(d:pJs_CodeNode):pJS_CodeNode;
begin
  d.next.opcode:=op_Nop;result:=d.next.next;
end;


//  GET_PROP       XXXX,YYYYY
//  MOVE           REG_N,{Result}
//  CALL           XXXX,REG_N
// Se transforma en
//  METHOD         XXXX,YYYYYY
//  Lo hacemos por eficiencia, sobre todo en las conversiones (pej. 'Hola'.toString();)
{
function  TJS_OptCodeGen.Gen_Method(d:pJS_CodeNode):pJs_CodeNode;
var d1,d2:pJS_CodeNode;
begin
 d1:=d.next;d2:=d1.next;result:=d2.next;
 if IsUsed(d1.k1,d1.v2,d1.next) then exit;        // Si posteriormente se utiliza el registro intemedio salimos

 d.opcode:=op_Method;
 d1.opcode:=op_Nop;
 d2.opcode:=op_Nop;
end;
}

function TJS_OptCodeGen.IsUsed(k1:cardinal;v1:longint;d:PJs_CodeNode):boolean;
begin
  result:=false;
  while (result=false) and (d<>nil) do begin
            // Si tenemos un Mov que lo sobreescribe seguro que no lo usamos
            if (d.opcode=op_Move) and (d.k1=k1) and (d.v1=v1) then exit;
            // Si se trata de Result y tenemos un CALL/GETPROPERTY/OPERATOR
            if IsResult(k1,v1) then begin
                                     if d.opcode in [op_Call,op_GetProperty] then exit;
                                     if js_kindIsBinOp(d.opcode) and (not js_flaggedOp(d.opcode)) then exit;
                                    end;
            // Si tenemos un acceso al mismo, seguro que si lo usamos
            result:=((d.k1=k1) and (d.v1=v1)) or ((d.k2=k1) and (d.v2=v1));
            d:=d.next;
            end;
end;



function TJS_OptCodeGen.CheckPeepHoled(d:pJs_CodeNode):TJS_PeepHoleF;
var d2,d3:pJs_CodeNode;
begin
  result:=nil;if d.next=nil then exit else d2:=d.next;d3:=d2.next;

  //if f_Match( [d.opcode       ,d2.opcode      , d.k1 ,d.k2 ,d.v1, d.v2],[op_SetProperty ,op_GetProperty ,d2.k1,d2.k2,d2.v1,d2.v2]) then result:=PH_SimpleSetGet; // Anulada por Getter/Setters

  if OpcodeIs(d,op_Move) and OpcodeIs(d2,d_BinOp) and (d.k1=opT_reg) and (IsResult(d.k2,d.v2)) and (d2.k2=opt_Reg) and (d.v1=d2.v2) then result:=PH_SimpleMove    else
  if f_Match([d.opcode,d.k1,d.v2,d2.opcode]                ,[op_Jump,opt_None,d2.v1,op_Label]) then result:=PH_SimpleJump1                                        else
  if f_match([d.opcode,d.k1,d.k2,d2.opcode,d2.k1,d.v1]     ,[op_Move,opT_reg,op_GetProperty,opT_reg,d2.v1]) then result:=PH_ChainGetProp                          else
  if f_match([d.opcode,d.k1,d.v1,d.k2,d2.opcode,d2.k1,d.v1],[op_Move,opT_reg,opt_Single,ops_Result,op_GetProperty,opT_reg,d2.v1]) then result:=PH_ChainGetProp    else
  if f_Match([d.opcode,d2.opcode,d.k1,d.v1]                ,[op_Ret,op_Ret,d2.k1, d2.v1]) then result:=PH_DoubleRet;

  // Las siguientes son optimizaciones de 3 elementos
  if (assigned(result)) or (d3=nil) then exit;


  //if f_Match( [d.opcode        ,d2.opcode,d3.opcode,d.k1 , d.v1,d2.k1,d2.v1,d2.k2, d2.v2   ],[op_GetProperty  ,op_Move  ,op_Call  ,d3.k1,d3.v1,d3.k2,d3.v2,opt_Single,ops_Result  ]) then result:=Gen_Method else
  if f_Match( [d.opcode,d.k1    ,d2.opcode,d2.k1    ,d3.opcode, d.v2],[op_Jump ,opT_Flag,op_Jump,  opt_None , op_Label, d3.v1]) then result:=PH_SimpleJump
  else if f_Match( [d.opcode       ,d3.opcode      , d.k1 ,d.k2 ,d.v1, d.v2],[op_SetProperty ,op_GetProperty ,d3.k1,d3.k2,d3.v1,d3.v2]) and f_KeepsResult(d2) then result:=PH_SimpleSetGet2;

 
  
end;



end.

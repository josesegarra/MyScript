unit js_gencode;

interface

{$M+}

uses js_codeformat,js_parser,js_tokens,fw_utils,fw_vm_types,fw_vm_constants;

type
  TJs_CodeGen=class(TJs_Parser)
    private
        cur_thisK:cardinal;
        cur_thisV:longint;
        err_ident:cardinal;                         // Ident que esta actuando de error, solo tiene un valor dentro de un bloque catch
        func_ident:cardinal;                        // Ident de la funcion actual
        cb:pJs_Function;                            // Current block
        function  c_incLabel:longint;
        function  c_emit(c:cardinal;k1:cardinal;v1:longint;k2:cardinal;v2:longint):pJS_CodeNode;
        function  c_emitVar(f:pJs_Function;p:p_node;kind:cardinal):boolean;
        function  c_result(p:p_node):boolean;
        function  c_Locate(p:p_node;var k1:cardinal;var v1:longint):boolean;
        function  c_IsCatch(p:p_node;var k1:cardinal;var v1:longint):boolean;
        function  c_allocreg(p:p_node):longint;
        procedure c_unallocreg(k1:cardinal;v1:longint);
        procedure c_unallocregs;
        function  c_tokenToFlag(k:cardinal;OnTrue:boolean):cardinal;
        function  c_FunctionRealName(s:string):string;
        function  c_CatchLevel(p:p_node):longint;

        
        procedure c_MakeSureOutOfTry(p:p_node);  // Salimos de cualquier bloque try-catch-finally en el que podamos estar
        procedure c_MakeSureFinal;
        procedure c_DiscardFrame(k:cardinal);
        function  c_IsProperty(p:p_node):boolean;
        procedure c_LocateVar(p:p_node;var k1:cardinal;var v1:longint);
        function  c_ParentVar(p:p_node):p_node;
        function  c_Propagate(what:p_node):p_node;
        procedure c_MakeClosured(p:p_node);

        function  c_NewLabel:pJs_CodeNode;

        function  GenCodeStatement(p:p_node):boolean;
        function  GenFunctionBody(p:p_node):boolean;
        function  GenCodeBreak(p:p_node):boolean;
        function  GenCodeBreakHelper(p:p_node):boolean;
        function  GenCodeIf(p:p_node):boolean;
        function  GenCodeThrow(p:p_node):boolean;
        function  GenCodeTry(p:p_node):boolean;
        function  GenCodeWhile(p:p_node):boolean;
        function  GenCodeFor(p:p_node):boolean;
        function  GenCodeGetterSetter(p:p_node;dt:cardinal;dv:longint):boolean;
        function  GenCodeForIn(p:p_node):boolean;
        function  GenCodeExpressionGroup(p:p_node;dt:cardinal;dv:longint):boolean;
        function  GenCodeFlaggedExpresssion(p:p_node):longint;
        function  GenCodeFloat(p:p_node;dt:cardinal;dv:longint):boolean;
        function  GenCodeOrExpression(p:p_node;dt:cardinal;dv:longint):boolean;
        function  GenCodeAndExpression(p:p_node;dt:cardinal;dv:longint):boolean;
        function  GenCodeExpression(p:p_node;dt:cardinal;dv:longint):boolean;
        function  GenCodeCall(p:p_node;dt:cardinal;dv:longint):boolean;
        function  GenCodeUnaryOperator(p:p_node;dt:cardinal;dv:longint):boolean;
        function  GenCodeIfOperator(p:p_node;dt:cardinal;dv:longint):boolean;
        function  GenCodeBinOperator(p:p_node;dt:cardinal;dv:longint):boolean;
        function  GenCodeAssign(p:p_node;dt:cardinal;dv:longint):boolean;
        function  GenCodeCarryAssign(p:p_node;dt:cardinal;dv:longint):boolean;
        function  GenCodePostInc(p:p_node;dt:cardinal;dv,k:longint):boolean;
        function  GenCodePreInc(p:p_node;dt:cardinal;dv,k:longint):boolean;
        function  GenCodeIndex(p:p_node;dt:cardinal;dv:longint):boolean;
        function  GenCodeNewExpression(p:p_node;dt:cardinal;dv:longint):boolean;
        function  GenCodeIdent(p:p_node;dt:cardinal;dv:longint):boolean;
        function  GenCodeReturn(p:p_node):boolean;
        function  GenCodeFuncExpression(p:p_node;dt:cardinal;dv:longint):boolean;
        function  GenCodeObject(p:p_node;dt:cardinal;dv:longint):boolean;
        function  GenCodeArrayDef(p:p_node;dt:cardinal;dv:longint):boolean;
    published
         function  c_InvertFlag(k:longint):longint;
    public
        programa:pJs_Program;

        regs:pointer;
        destructor  Destroy; override;
        constructor Create;
        function    GenCode:boolean;
        function  c_Error(s:string;p:p_node):boolean;
        
//        procedure   PrintCode;
    end;


implementation

uses js_lexer;

var gen_code_debug:boolean=false;
    JS_BoolOpRes:array [jst_Min_BoolOp..jst_Max_boolOp,false..true] of cardinal;



constructor TJs_CodeGen.Create;
begin
  inherited Create;
  programa:=js_program_create;
  regs:=l_create(SizeOf(POINTER),0);
  err_ident:=0;
end;



destructor  TJs_CodeGen.Destroy;
begin
  programa:=js_program_free(programa);
  regs:=l_free(regs);
  inherited destroy;
end;

function  TJs_CodeGen.c_incLabel:longint;
begin
  result:=cb.nlabels;inc(cb.nlabels);
end;

function  TJs_CodeGen.c_NewLabel:pJs_CodeNode;
begin
 result:=c_Emit(op_Label,opt_Label,c_incLabel,opt_None,0);
end;

function    TJs_CodeGen.GenCode:boolean;
var p1:p_node;
    oc:pJs_Function;
begin
  result:=true;vars:=root.first;func_ident:=0;
  vars.hash:=js_program_AddFunction(programa,'#@Globals');                                                                                                      // Generamos el bloque de Globals y lo guardamos en vars.HASH. Lo hacemos aqui para que sea el bloque 0
  p1:=vars.next.first;while p1<>nil do begin p1.value:=js_program_AddFunction(programa,p1.text);p1:=p1.next;end;                                                // Generamos el resto de funciones definidas en el orden en que aparecen
  p1:=n_LastBrother(root.first.next.first);while (p1<>nil) and result do begin c_unallocregs;result:=GenFunctionBody(p1);p1:=p1.prev;end;                       // Generamos el codigo en orden inverso, lo hacemos para tener cubiertas los accesos indirectos (que se descubren en una closure definida a posteriori)
  cb:=js_function_create;oc:=cb;p1:=root.first.next.next;                                                                                                       // Ahora el codigo de main en un cb flotante
  while (p1<>nil) and result do begin result:=GenCodeStatement(p1);p1:=p1.next;end;
  vars:=root.first;p1:=vars.first;cb:=js_program_FunctionByIndex(programa,vars.hash);                                                                           // Generamos el codigo de las variables globales
  while (p1<>nil) and result do begin result:=c_emitVar(cb,p1,opT_Gvar);p1:=p1.next;end;
  js_function_move(cb,oc);cb.nlabels:=oc.nlabels;js_function_Free(oc);                                                                                          // Copiamos el codigo de main tras las variables
  c_Emit(op_Ret,opT_Flag,opF_InMainBody,opt_Flag,opF_NoFlag);                                                                                                            // Y emitimos el Ret final
  programa.n_regs:=l_count(regs);                                                                                                                                  // Finalmente guardamos cuantos registros necesitamos y cuantas variables globales
  programa.n_vars:=vars.value;
end;

// Pending: iniciarlo todo como se hacen las funciones
// Var se queda como esta
// ExtValue   se ha de transformar en un define y un extern
// Param      se ha de transformar en un define y en un param
// IndParam   se ha de transformar en un define y en un IndParam
// IndVar     se ha de transformar en un define y en un IndVar
// Link       se ha de transformar en un define y en un Link
// y finalmente todos los defines se adelantan
function  TJs_CodeGen.c_emitVar(f:pJs_Function;p:p_node;kind:cardinal):boolean;
var l0:longint;
begin
  l0:=js_program_AddCodeString(programa,p.text);                                                            // l0 es la posicion de memoria donde esta esta el nombre de la variable
  l_pushi(f.var_names,l0);                                                                                  // Pusheamos l0 en la lista de var_names de la funcion
  result:=true;
  case p.kind of
          jst_Param:      begin c_Emit(op_Move,opt_Var,p.value,opt_RParam,p.value);inc(cb.nvars_p);end;     // Los parametros se copian sin mas, los numeros de orden coinciden
          jst_ExtValue:   c_Emit(op_Extern,kind,p.value,opt_ExtPchar,l0);                                   // Los valores externos se importan
          jst_IndVar:     c_emit(op_GCVar,kind,p.value,opt_None,0);                                         // Crea una variable GC con valor opt_None, hace que kind|p.value apunte a la misma
          jst_IndParam:   c_emit(op_GCVar,kind,p.value,opt_RParam,p.value);                                 // Crea una variable GC con valor el del Own_Param recibido
          jst_LinkCopy:   begin                                                                             // Esta es una variable que debe ser "capturad" ya que es usada por una funcion  de Closure
                             if cb.nvars_ex=-1 then cb.nvars_ex:=js_program_AddCodeInteger(cb.programa,0);  // Si no teniamos ninguna variable capturada, ponemos a 0 el contador
                             js_program_AddCodeInteger(cb.programa,p.number);                               // Añdimos la variable padre a la lista de variables a capturar al iniciar la funcion
                             c_Emit(op_Link,kind,p.value,opt_CVar,b_inc(cb.programa.integers,cb.nvars_ex)); // Añadimos codigo para que la variable local apunta a la capturada
                          end;
          jst_GCopy:     begin
                             if cb.nvars_ex=-1 then cb.nvars_ex:=js_program_AddCodeInteger(cb.programa,0);  // Si no teniamos ninguna variable capturada, ponemos a 0 el contador
                             js_program_AddCodeInteger(cb.programa,-p.number-1);                              // Añdimos la variable GLOBAL a la lista de variables a capturar al iniciar la funcion. El signo diferencia entre Var_Ancestors y globales. Añadimos -1 porque +0=-0
                             c_Emit(op_Link,kind,p.value,opt_CVar,b_inc(cb.programa.integers,cb.nvars_ex)); // Añadimos codigo para que la variable local apunta a la capturada
                          end;
          jst_Var:       begin end; //if kind=opT_Gvar then c_emit(op_Define,kind,p.value,opt_ExtPchar,l0);
                         
          else result:=c_Error('Bad kind in GLOBAL VARS '+s_i2s(p.kind)+' '+js_token2str(p.kind),p);
          end;
  if result and n_kindIs(p.first,jst_FuncDef) then result:=GenCodeFuncExpression(p.first,kind,p.value);
end;


function  TJs_CodeGen.c_emit(c:cardinal;k1:cardinal;v1:longint;k2:cardinal;v2:longint):pJS_CodeNode;
begin
 if c<>op_Move then begin result:=js_program_AddCode(cb,c,k1,k2,v1,v2);exit;end;
 // Si tenemos un move a nada, o un move a si mismo, salimos
 if (k1=opT_none) then begin result:=nil;exit;end;
 if (k1=k2) and (v1=v2) then begin result:=nil;exit;end;
 // Generamos el move
 result:=js_program_AddCode(cb,c,k1,k2,v1,v2);
end;

function  TJs_CodeGen.c_Error(s:string;p:p_node):boolean;
begin
  result:=false;
  if error='' then begin
        error:='GenCode error: '+s;
        if p<>nil then begin
              error:=error+#13#10+js_token2str(p.kind);
              if p.text<>'' then error:=error+' ['+p.text+']';
              if p.pos>0 then error:=error+#13#10+CalcXY(pchar(src+p.pos));end;
              end;
  token:=jst_Error;
end;

// Todo ident que exista debe de ser un GVar o Var, c_Locate se encarga de averiguar cual
function  TJs_CodeGen.GenCodeIdent(p:p_node;dt:cardinal;dv:longint):boolean;
var et:cardinal;
    ev:longint;
begin
  result:=c_Locate(p,et,ev);    // Localizamos el objeto
  if result and (dt<>opT_None) then c_Emit(op_Move,dt,dv,et,ev);
  c_unallocreg(et,ev);
end;

// p es un nodo VAR y queremos ir al VAR del padre
function  TJs_CodeGen.c_ParentVar(p:p_node):p_node;
begin
   // si el padre es root salimos, si no vamos a la funcion (parent),luego a donde esta definida (data) y luego al vars (first)
  if p.parent=root then result:=nil else result:=p.parent.data.first;
end;

function  TJs_CodeGen.c_CatchLevel(p:p_node):longint;
var k,m:cardinal;
begin
  k:=p.hash;p:=p.parent;result:=0;m:=0;
  while (result=0) and (p.parent<>root) do begin
          if (p.kind=jst_catch) then begin
                  if (n_kind(p.first)=jst_ident) then if p.first.hash=k then result:=m;
                  m:=m+1;
                  end;
           p:=p.parent;
        end;
end;

// Caso especial, cuando localizamos una variable cuyo nombre coincide con el valor de CATCH(nombre)
function  TJs_CodeGen.c_IsCatch(p:p_node;var k1:cardinal;var v1:longint):boolean;
begin
 if err_ident<>0 then begin
       if p.hash=err_ident  then begin k1:=opT_CatchVar;v1:=0;result:=true;exit;end;
       v1:=c_CatchLevel(p);if v1<>0 then begin k1:=opT_CatchVar;result:=true;exit;end;
       end;
 result:=false;
end;

// En tiempo de ejecucion: toda variable a la que tiene acceso una funcion es a traves de las variables del Runner actual, es decir de pRt_Runner.locals
// Existen variables de scope local y de scope global
// Las variables de scope local: son las definidas con VAR dentro de la propia funcion y los parametros de llamada a la funcion RPARAM
// Las variabels de scope global: son las globales y las closures


procedure TJs_CodeGen.c_LocateVar(p:p_node;var k1:cardinal;var v1:longint);
var p0,p1:p_node;
begin
 if c_IsCatch(p,k1,v1) then exit;                                                                                       // Caso especial, es el ident de un bloque de catch 
 if p.hash=func_ident then begin k1:=opt_Single;v1:=opS_CurF;exit;end;                                                  // Caso especial, es el nombre de la funcion actual, solo se utiliza en expresiones recursivas cuando el nombre de la funcion es optativo (si es obligatorio iremos a la definicion de la funcion)
 p1:=p.data;                                                                                                            // Localizamos en p1 la definicion de la variable (que debe colgar de cur_func.vars = jst_vars)
 if p1=nil then begin                                                                                                   // Si no tenemos una definicion
      p0:=vars;                                                                                                         // Las primeras variables en las que buscamos son las locales
      repeat
        p1:=n_FindChildHash(p0,p.hash);                                                                                 // Buscamos el nodo en las variables locales
        if p1=nil then p0:=c_ParentVar(p0);                                                                             // Si no lo encontramos subimos a la funcion padre de la actual
        until (p0=nil) or (p1<>nil);                                                                                    // Se acaba cuando lo encontramos o llegamos a tope de la cadea
      if p1=nil then begin p1:=n_CreateT(root.first,jst_ExtValue,p.text);p1.value:=n_ValuePP(root.first);end            // Si no lo hemos encontrado la creamos en root como valor global (y/p externo)
                else p1:=c_propagate(p1);                                                                               // Si la hemos encontrado nos aseguramos que se defina como indirecta y que propague en las funciones intermedias
      end;
 if vars=root.first  then begin k1:=opt_GVar;p.data:=p1;v1:=p1.value;exit;end;                                          // Si estamos en la funcion Global, solo puede ser una variable global que seguro hemos localizado (si no existia la hemos creado)
 if (p1.parent.parent.kind=jst_Root) then begin                                                                         // Si se trata de una variable Global !!!
         k1:=p1.value;p1:=n_CreateT(vars,jst_GCopy,p.text);p1.value:=n_ValuePP(vars);p1.number:=k1;                     // Creamos una variable local, cuyo origen es una referencia a una Global
      end;
 k1:=opT_Var;p.data:=p1;v1:=p1.value;
end;


procedure TJS_CodeGen.c_MakeClosured(p:p_node);
begin
  if not (p.kind in [jst_Param,jst_Var]) then exit;
  p.number:=n_NumberPP(p.parent);                                           // Asignamos a what un nº de variable de closure
  if p.kind=jst_Param then p.kind:=jst_IndParam else
  //if p.kind=jst_Var then
  p.kind:=jst_IndVar;
end;


// Cuando llegamos aqui, what es una definicion de variable que esta en una funcion que
// engloba (o no) a la actual
function  TJs_CodeGen.c_Propagate(what:p_node):p_node;
var p1,p2a,p2b:p_node;
begin
  if (what.parent=vars) or (what.parent=root.first) then begin result:=what;exit;end;           // Si el nodo pertenece a Vars de la funcion actual o Vars global, es accesible directamente y no hay posibilidad de closure
  c_MakeClosured(what);                                                                         // La variable se define como closured

  result:=n_CreateT(vars,jst_LinkCopy,what.text);result.value:=n_ValuePP(vars);                 // Creamos en vars de la funcion actual el nodo que devolveremos
  result.data:=pointer(10);
  p1:=c_ParentVar(vars);p2a:=result;
  while p1<>what.parent do begin
                p2b:=n_CreateT(p1,jst_LinkCopy,what.text);
                p2b.value:=n_ValuePP(p1);
                p2a.number:=//(1 shl 16) or
                            p2b.value;
                p2a:=p2b;
                p1:=c_ParentVar(p1);
                end;                    // Vamos subiendo e incrementando el nivel hasta que coincida


  p2a.number:=what.value;                                          
end;

{
// Cuando llegamos aqui, what es una definicion de variable que esta en una funcion que
// engloba (o no) a la actual
function  TJs_CodeGen.c_Propagate(what:p_node):p_node;
var p0,p1:p_node;
begin
  if (what.parent=vars) or (what.parent=root.first) then begin result:=what;exit;end;           // Si el nodo pertenece a Vars de la funcion actual o Vars global, es accesible directamente y no hay posibilidad de closure

  if what.kind in [jst_Param,jst_Var] then begin                                                // Si aun no era una variable de closure
                what.number:=n_NumberPP(what.parent);                                           // Asignamos a what un nº de variable de closure
                if what.kind=jst_Param then what.kind:=jst_IndParam else what.kind:=jst_IndVar; // Nos aseguramos de que se inicie correctamente
                end;

  result:=n_CreateT(vars,jst_LinkCopy,what.text);result.value:=n_ValuePP(result.parent);        // Creamos en vars de la funcion actual el nodo que devolveremos
  vars.hash:=vars.hash+1;                                                                       // Marcamos que hay una variable capturada que se utilizara
  p1:=vars;
  while p1<>what.parent do begin inc(result.number);p1:=c_ParentVar(p1);end;                    // Vamos subiendo e incrementando el nivel hasta que coincida
  //s_alert('Para acceder a '+what.text+' niveles a subir '+s_i2s(result.number)+#13+
  //        'Para acceder a '+what.text+' el nº de captured var es '+s_i2s(what.number)+#13+'Que se codifica como '+s_i2h(result.number shl 16  or what.number));
  result.number:=result.number shl 16  or what.number;                                           // Los niveles a subir son 4 bytes a la izquierda
end;
}


function  TJs_CodeGen.GenCodeFloat(p:p_node;dt:cardinal;dv:longint):boolean;
var k1:cardinal;v1:longint;
begin
   result:=s_s2f(p.text,k1,v1);if not result then exit;
   c_emit(op_Move,dt,dv,k1,k1);
end;

function TJs_CodeGen.c_Locate(p:p_node;var k1:cardinal;var v1:longint):boolean;
begin
  result:=true;
  case p.kind of
    jst_Ident:   if c_IsProperty(p) then begin k1:=opT_ExtPChar;v1:=js_program_AddCodeString(programa,p.text);end else c_LocateVar(p,k1,v1);
    jst_Integer:   begin k1:=opT_Integer;v1:=s_s2i(p.text);end;
    jst_float:     result:=s_s2f(p.text,k1,v1);
    jst_PChar:     begin k1:=opT_ExtPChar;v1:=js_program_AddCodeString(programa,p.text);end;
    jst_This:      begin k1:=opt_Single;v1:=opS_This;end;
    else           begin k1:=opT_reg;v1:=c_allocreg(p);result:=GenCodeExpression(p,opT_reg,v1);end;
  end;
end;


function  TJs_CodeGen.c_allocreg(p:p_node):longint;
begin
  result:=l_count(regs)-1;
  while (result>=0) and (l_get(regs,result)<>nil) do result:=result-1;
  if result>=0 then l_set(regs,result,p) else result:=l_push(regs,p);
end;


procedure TJs_CodeGen.c_unallocreg(k1:cardinal;v1:longint);
begin
  if k1=opT_Reg then l_set(regs,v1,nil);
end;

procedure TJs_CodeGen.c_unallocregs;
var i:longint;
begin
  for i:=0 to l_count(regs)-1 do l_set(regs,i,nil);
end;

function   TJs_CodeGen.GenCodeStatement(p:p_node):boolean;
begin
  if p=nil then begin result:=true;exit;end;        // El statement vacio no genera codigo
  
  cur_ThisK:=opT_Single;cur_ThisV:=opS_This;

  case p.kind of
       jst_Begin:       begin p:=p.first;result:=true;while (p<>nil) and result do begin result:=GenCodeStatement(p);p:=p.next;end;end;
       jst_Return:      result:=GenCodeReturn(p);
       jst_If:          result:=GenCodeIf(p);
       jst_Throw:       result:=GenCodeThrow(p);
       jst_Try:         result:=GenCodeTry(p);
       jst_finally:     result:=GencodeStatement(p.first);
       jst_While:       result:=GenCodeWhile(p);
       jst_BreakHelper: result:=GenCodeBreakHelper(p);
       jst_Break:       result:=GenCodeBreak(p);
       jst_For:         result:=GenCodeFor(p);
       jst_ForIn:       result:=GenCodeForIn(p);
       else             result:=GenCodeExpression(p,opT_None,0);
     end;
end;


function  TJs_CodeGen.GenCodeReturn(p:p_node):boolean;
var flag_root,flag_return:cardinal;
begin
  if p.first<>nil then begin result:=GenCodeExpression(p.first,opt_Single,ops_Result);flag_return:=opF_WithReturn;end else begin result:=true;flag_return:=opF_NoFlag;end;
  if not result then exit;
  c_MakeSureOutOfTry(p);              // Salimos de cualquier bloque try-catch-finally en el que podamos estar
  flag_root:=opF_InMainBody;
  while (flag_root=opF_InMainBody) and (p<>nil) do if p.kind=jst_Functions then flag_root:=opF_NoFlag else p:=p.parent;
  c_Emit(op_Ret,opT_Flag,flag_root,opt_Flag,flag_return);
end;


procedure TJs_CodeGen.c_DiscardFrame(k:cardinal);
begin
  c_Emit(op_PopFrame,opt_FrameError,k,opt_None,0)
end;                                    

procedure TJs_CodeGen.c_MakeSureFinal;
var c:pJs_CodeNode;
begin
  c:=c_Emit(op_PatchFrame,opt_FrameError,opFr_TryFrame,opt_Label,c_incLabel);
  c_Emit(op_PopFrame,opt_FrameError,opFr_FinalFrame,opt_None,0);     // Ejecutamos Frame_Final, el control pasara al bloque Final que cuando acabe ejecutara Frame_Try
  c_Emit(op_Label,opT_Label,c.v2,opt_None,0);                        // Que nos llevara aqui
end;

 // La estructura es
 //  finally
 //     try
 //         ---- Try Code
 //         catch
 //             --- Catch Code
 //     --- Finally Code

// Esta funcion se llama cuando se ejecuta alguna instruccion que rompe el flujo normal: RET, BREAK o CONTINUE.
// Justo antes del salto. Comprueba si estamos en un bloque TRY-CATCH-FINALLY y actua en consecuenta
procedure TJs_CodeGen.c_MakeSureOutOfTry(p:p_node);
var s:string;
    p1:p_node;
begin
  // Podemos estar en alguna de estas situaciones en el STACK
  //  -> Sin FRAMES antes de la vuelta
  //  -> Si estamos en codigo TRY:    y tenemos CATCH|FINAL     Descartamos CATCH, modificamos TRY para que vuelva a LABEL y hacemos POP_FINAL
  //  ->                              y tenemos CATCH           Descartamos CATCH, y descartamos TRY
  //  ->                              y tenemos FINAL           Modificamos TRY para que vuelva a LABEL y hacemos POP_FINAL
  //  -> Si estamos en codigo CATCH:  y tenemos CATCH|FINAL     Modificamos TRY para que vuelva a LABEL y hacemos POP_FINAL
  //  ->                              y tenemos CATCH           Descartamos TRY
  //  -> Si estamos en codigo FINAL:  y tenemos FINAL           Descartamos TRY
  //  ->                              y tenemos CATCH|FINAL     Descartamos TRY

  s:='';p1:=nil;
  repeat
      if (p.kind in [jst_try,jst_catch,jst_finally]) and (p1=nil) then p1:=p;
      if p.kind=jst_Try then begin
                if p.first.next<>nil then s:='C' else s:='';
                if p.parent.kind=jst_Finally then begin s:=s+'F';p:=p.parent;end;

                if (p1.kind=jst_Try) and (s='CF')     then begin c_DiscardFrame(opFr_CatchFrame);c_MakeSureFinal;p1:=nil;end else
                if (p1.kind=jst_Try) and (s='C')      then begin c_DiscardFrame(opFr_CatchFrame);c_DiscardFrame(opFr_TryFrame);p1:=nil;end else
                if (p1.kind=jst_Try) and (s='F')      then begin c_MakeSureFinal;p1:=nil;end else
                if (p1.kind=jst_catch) and (s='CF')   then begin c_MakeSureFinal;p1:=nil;end else
                if (p1.kind=jst_catch) and (s='C')    then begin c_DiscardFrame(opFr_TryFrame);p1:=nil;end else
                if (p1.kind=jst_finally) and (s='CF') then begin c_DiscardFrame(opFr_TryFrame);p1:=nil;end else
                if (p1.kind=jst_finally) and (s='F')  then begin c_DiscardFrame(opFr_TryFrame);p1:=nil;end;
                end;
      p:=p.parent;
      until p.kind=jst_Root;
end;


// Si tenemos a+=b, se genera codigo para result=a+b y ahora hacemos a=result
function  TJs_CodeGen.GenCodeCarryAssign(p:p_node;dt:cardinal;dv:longint):boolean;
var  o1:cardinal;
     v1:longint;
begin
  case p.kind of
     jst_ident: begin result:=true;c_emit(op_Move,dt,dv,opt_Single,ops_Result);end;
     jst_Index,jst_member: begin
                 result:=c_Locate(p.first,o1,v1) and c_Locate(p.first.next,dt,dv); // Localizamos el objeto y su propiedad

                 c_Emit(op_SetProperty,o1,v1,dt,dv);
                 c_unallocreg(o1,v1);c_unallocreg(dt,dv);
                 end;
       else result:=c_Error('Only referencec can  be assigned',p);
  end;
end;


function IsResult(os:cardinal;ov:longint):boolean;
begin
  result:=(os=opt_Single) and (ov=opS_result);
end;


// Podemos tener las siguientes opciones
//  UN ASSIGN SIMPLE        a=(expresion)             a...b=(expresion)
//  UN ASSIGN MULTIPLE      a=b=c=(expresion)         a...b=c=d=(expresion)

function  TJs_CodeGen.GenCodeAssign(p:p_node;dt:cardinal;dv:longint):boolean;
var p1,p2:p_node;
    o1,o2,os:cardinal;
    v1,v2,vs:longint;
begin
  p2:=n_LastBrother(p.first);p1:=p2.prev;   // p1 = parte izquierda de la expresion ; p2 = expresion a asignar
  case p1.kind of
    jst_Ident: begin result:=c_Locate(p1,os,vs);if result then result:=GenCodeExpression(p2,os,vs);c_unallocreg(os,vs);end; // Tenemos en os,vs el valor asignado
    jst_Index,jst_Member: begin
                 result:=c_Locate(p1.first,o1,v1);
                 result:=result and c_Locate(p1.first.next,o2,v2); // Localizamos el objeto y su propiedad
                 if result then result:=GenCodeExpression(p2,opt_Single,ops_Result);
                 c_Emit(op_SetProperty,o1,v1,o2,v2);
                 c_unallocreg(o1,v1);c_unallocreg(o2,v2);
                 os:=opt_Single;vs:=ops_Result;
                 end;
    else result:=c_Error('Can''t assign',p1);
  end;
  if not result then exit;
  p1:=p1.prev;
  // Si tenemos un assign encadenado y ya hemos "Ocupado" result, comprobamos si es necesario un registro
  // podemos evitar esto SI SABEMOS que en todos los p1.prev NO se utiliza result (es decir NO hay index, ni CALls, ni se definen
  // objetos,..) pero de momento NO LO HACEMOS.
  if (p1<>nil) and IsResult(os,vs) then begin os:=opt_reg;vs:=c_allocreg(p2);c_Emit(op_Move,opt_reg,vs,opt_Single,ops_Result);end;

  while result and (p1<>nil) do begin
          if p1.kind=jst_Ident then begin result:=c_Locate(p1,o2,v2);c_Emit(op_Move,o2,v2,os,vs);c_unallocreg(o2,v2);end else
          if (p1.kind=jst_Index) or (p1.kind=jst_Member) then begin
                    result:=c_Locate(p1.first,o1,v1) and c_Locate(p1.first.next,o2,v2); // Localizamos el objeto y su propiedad
                    if result then begin if not IsResult(os,vs) then c_Emit(op_Move,opt_Single,ops_Result,os,vs);c_Emit(op_SetProperty,o1,v1,o2,v2);end;
                    c_unallocreg(o1,v1);c_unallocreg(o2,v2);
                    end
          else result:=c_Error('Can''t assign',p1);
          p1:=p1.prev;
          end;
  // Arrastramos el move
  c_emit(op_Move,dt,dv,os,vs);
  c_unallocreg(os,vs);
end;


function  TJs_CodeGen.GenCodeUnaryOperator(p:p_node;dt:cardinal;dv:longint):boolean;
var os:cardinal;
    vs:longint;
begin
    result:=c_Locate(p.first,os,vs);                  // Localizamos el valor (que pasara a result)
    c_emit(p.kind,dt,dv,os,vs);                       // Se ejecuta el operador y el resultado pasa al destino esperado
    c_unallocreg(os,vs);                              // Por si acaso !!
end;

function  TJs_CodeGen.GenCodePreInc(p:p_node;dt:cardinal;dv,k:longint):boolean;
var o1,o2,os:cardinal;
    v1,v2,vs:longint;
begin
  p:=p.first;if not (p.kind in [jst_ident,jst_Index,jst_member]) then begin result:=c_Error('Can''t autoincrement',p);exit;end;
  if p.kind=jst_Ident then begin
               result:=c_Locate(p,os,vs);
               c_emit(jst_Add,os,vs,opT_Integer,k);
               c_emit(op_Move,os,vs,opt_Single,ops_Result);
               c_unallocreg(os,vs);
               c_emit(op_Move,dt,dv,opt_Single,ops_Result);
               exit;
               end;
  // Si llegamos aqui se trata de un jst_Index
  result:=c_Locate(p.first,o1,v1) and c_Locate(p.first.next,o2,v2); // Localizamos el objeto y su propiedad
  c_Emit(op_GetProperty,o1,v1,o2,v2);
  c_emit(jst_Add,opt_Single,ops_Result,opT_Integer,k);
  c_Emit(op_SetProperty,o1,v1,o2,v2);
  c_unallocreg(o1,v1);c_unallocreg(o2,v2);
  c_emit(op_Move,dt,dv,opt_Single,ops_Result);
end;


function  TJs_CodeGen.GenCodePostInc(p:p_node;dt:cardinal;dv,k:longint):boolean;
var o1,o2,os:cardinal;
    v1,v2,vs:longint;
begin
  p:=p.first;if not (p.kind in [jst_ident,jst_Index,jst_member]) then begin result:=c_Error('Can''t autoincrement',p);exit;end;
  if p.kind=jst_Ident then begin
               result:=c_Locate(p,os,vs);                       // Localizamos el identificador
               c_emit(op_Move,dt,dv,os,vs);                     // Lo copiamos al destino
               c_emit(jst_Add,os,vs,opT_Integer,k);             // Lo incrementamos
               c_emit(op_Move,os,vs,opt_Single,ops_Result);     // Y guardamos el cambio
               c_unallocreg(os,vs);
               exit;
               end;
  // Si llegamos aqui se trata de un jst_Index
  result:=c_Locate(p.first,o1,v1) and c_Locate(p.first.next,o2,v2); // Localizamos el objeto y su propiedad
  c_Emit(op_GetProperty,o1,v1,o2,v2);                               // Sacamos el valor de la propiedad
  c_emit(op_Move,dt,dv,opt_Single,ops_Result);                      // La copiamos en el destino
  c_emit(jst_Add,opt_Single,ops_Result,opT_Integer,k);              // La incrementamos
  c_Emit(op_SetProperty,o1,v1,o2,v2);                               // Y la guardamos
  c_unallocreg(o1,v1);c_unallocreg(o2,v2);
end;



// Nos dice si p ESCRIBE el valor de result
function  TJs_CodeGen.c_result(p:p_node):boolean;
begin
  result:=(p.kind=jst_Index) or (p.kind=jst_member) or (p.kind=jst_fcall) or js_tokenIsUnaryPreOp(p.kind) or js_tokenIsUnaryPostOp(p.kind) or js_kindIsBinOp(p.kind);
end;

function  TJs_CodeGen.c_IsProperty(p:p_node):boolean;
begin
 result:=(p.parent.kind=jst_member) and (p.prev<>nil);
end;


function TJs_CodeGen.c_tokenToFlag(k:cardinal;OnTrue:boolean):cardinal;
begin
  if not js_flaggedOp(k) then k:=opF_Eq;
  result:=JS_BoolOpRes[k,OnTrue];
end;

function TJs_CodeGen.c_FunctionRealName(s:string):string;
var i:longint;
begin
  i:=length(s)-1;
  while (i>0) and (s[i]<>'#') do i:=i-1;
  if (i>0) and (s[i+1]<>'!') then result:=copy(s,i+1,length(s)) else result:='';
end;

function TJs_CodeGen.c_InvertFlag(k:longint):longint;
var i:longint;
begin
   if (k=opF_BADFLAG) or (k=opF_NoFlag) then begin result:=k;exit;end;
   if k=opF_True then begin result:=opF_False;exit;end;
   if k=opF_False then begin result:=opF_True;exit;end;

   result:=0;
   i:=jst_Min_BoolOp;while (result=0) and (i<=jst_Max_boolOp) do if JS_BoolOpRes[i,true]=k then result:=JS_BoolOpRes[i,false] else i:=i+1;
   if result<>0 then exit;
   i:=jst_Min_BoolOp;while (result=0) and (i<=jst_Max_boolOp) do if JS_BoolOpRes[i,false]=k then result:=JS_BoolOpRes[i,true] else i:=i+1;
end;


function  TJs_CodeGen.GenCodeAndExpression(p:p_node;dt:cardinal;dv:longint):boolean;
var  l,f:longint;
     k:Cardinal;
begin
  p:=p.first;result:=true;l:=c_incLabel;
  while (p<>nil) and result do begin
    // Si es un boolean
    //      SI es FALSO emitiremos un salto, Si es TRUE lo ignoramos
    if p.kind=jst_bool then begin if p.value=0 then begin k:=opt_None;f:=opF_NoFlag;p:=n_LastBrother(p);end;end
                       else begin k:=opt_Flag;f:=c_InvertFlag(GenCodeFlaggedExpresssion(p));result:=f<>opF_BADFLAG;end;
    if result then c_emit(op_JumpR,k,f,opt_Label,l);
    p:=p.next;
    end;
  c_emit(op_Label,opT_Label,l,opt_None,0);                            // La etiqueta de salida
  if dt<>opT_None then c_emit(op_SetOn,dt,dv,opT_Flag,opF_False);   // Si hubo algun salto

end;


function  TJs_CodeGen.GenCodeOrExpression(p:p_node;dt:cardinal;dv:longint):boolean;
var  l,f:longint;
     k:Cardinal;
begin
  p:=p.first;result:=true;l:=c_incLabel;k:=opt_None;f:=opF_NoFlag;
  while (p<>nil) and result do begin
    // Si es un boolean
    //      TRUE: SI es cierto emitiremos un salto, Si es falso lo ignoramos
    if p.kind=jst_bool then begin if p.value<>0 then begin k:=opt_None;f:=opF_NoFlag;p:=n_LastBrother(p);end;end
                       else begin k:=opt_Flag;f:=GenCodeFlaggedExpresssion(p);result:=f<>opF_BADFLAG;end;
    if result then c_emit(op_JumpR,k,f,opt_Label,l);
    p:=p.next;
    end;
  c_emit(op_Label,opT_Label,l,opt_None,0);                            // La etiqueta de salida
  if dt<>opT_None then c_emit(op_SetOn,dt,dv,opT_Flag,opF_True);    // Si hubo algun salto

end;


// El unico operador binario de derercha a izquierda es assign, asi que los
// gestionamos a piñon de izquierda a derecha
function  TJs_CodeGen.GenCodebinOperator(p:p_node;dt:cardinal;dv:longint):boolean;
var p1:p_node;
    o1,o2:cardinal;
    v1,v2:longint;
    copied:boolean;
begin
  p1:=p.first;  result:=c_Locate(p1,o1,v1);if not result then exit;  // Localizamos el primer operador
  p1:=p1.next;  result:=c_Locate(p1,o2,v2);if not result then exit;   // Y el segundo
  copied:=false;
  if js_flaggedOp(p.kind)  then begin
          c_emit(op_Compare,o1,v1,o2,v2);
          // La expresion 1==1==5==3 da true (se toma el primer resultado), que en este caso ya estaria copiado
          if (dt<>opT_None) and (dt<>opT_Flag) then begin copied:=true;c_emit(op_SetOn,dt,dv,opT_Flag,c_tokenToFlag(p.kind,true));end;
          end else c_emit(p.kind,o1,v1,o2,v2);
  if js_IsAssignOp(p.kind) then result:=GenCodeCarryAssign(p.first,o1,v1); // Si se trata de +=,-=,... hemos de copiar result en p.first

  p1:=p1.next;
  c_unallocreg(o1,v1);c_unallocreg(o2,v2);


  while result and (p1<>nil) do begin
        if c_result(p1) then begin o1:=opT_reg;v1:=c_allocreg(p1);c_emit(op_Move,opT_Reg,v1,opt_Single,ops_Result);end
                        else if not js_IsAssignOp(p.kind) then begin o1:=opt_Single;v1:=ops_Result;end else c_emit(op_Move,o1,v1,opt_Single,ops_Result);
        result:=c_Locate(p1,o2,v2);
        if result then begin

              if js_flaggedOp(p.kind)  then c_emit(op_Compare,o1,v1,o2,v2) else c_emit(p.kind,o1,v1,o2,v2);
              if js_IsAssignOp(p.kind) then result:=GenCodeCarryAssign(p.first,o1,v1); // Si se trata de +=,-=,... hemos de copiar result en p.first
              end;        // Emitimos

        c_unallocreg(o2,v2);
        c_unallocreg(o1,v1);
        p1:=p1.next;                           // Si tenemos mas de dos operadores, el primero sera result
        end;
  // Si tenemos un destino
  if not copied then c_emit(op_Move,dt,dv,opt_Single,ops_Result);
end;




function  TJs_CodeGen.GenCodeArrayDef(p:p_node;dt:cardinal;dv:longint):boolean;
var c:pJs_CodeNode;
    o1:cardinal;
    v1:longint;
begin
  p:=p.first;result:=true;
  if dt=opt_Param then begin o1:=opt_Reg;v1:=c_allocreg(p);end else begin o1:=dt;v1:=dv;end;
  c:=c_emit(op_CreateParams,opT_Integer,0,opT_none,0);                                                            // Generamos un frame de llamada
  while (p<>nil) and result do begin
           result:=GenCodeExpression(p,opT_Param,c.v1);
           c.v1:=c.v1+1;
           p:=p.next;
           end;
  c_emit(op_NewArray,o1,v1,opt_None,0);                                                                             // Creamos el array en dt,dv
  c_emit(op_DestroyParams,opt_None,0,opt_None,0);                                                                 // Destruimos los parametros
  if dt=opt_Param then begin c_emit(op_Move,dt,dv,opt_Reg,v1);c_unallocreg(opt_Reg,v1);end;
end;


function  TJs_CodeGen.GenCodeNewExpression(p:p_node;dt:cardinal;dv:longint):boolean;
var o1,o2:cardinal;
    v1,v2:longint;
    c:pJs_CodeNode;
    b:boolean;
begin
  if p.kind=jst_Begin then p:=p.first;                      // Si tenemos parametros lo tendremos agrupado con un jst_Begin (es un "error" del parser)
  result:=c_Locate(p,o1,v1);if not result then exit;        // Localizamos el constructos
  c:=c_emit(op_CreateParams,opT_Integer,0,opT_none,0);          // Generamos un frame
  p:=p.next;                                                // Pasamos a los parametros (si lo hay)
  while (p<>nil) and result do begin result:=GenCodeExpression(p,opT_Param,c.v1);c.v1:=c.v1+1;p:=p.next;end;
  if not result then exit;
  b:=dt=opt_None;
  if b then begin dt:=opt_reg;dv:=c_allocreg(p);end;        // Por si acaso NO teniamos donde recoger el objeto

  // Si dt es result la llamada al call lo machacara,
  // Si dt es opt_Param se refiere al parametro del frame precedente
  // En ambos casos usaremos un registro intermedio
  if (IsResult(dt,dv)) or (dt=opt_Param) then begin o2:=opT_reg;v2:=c_allocreg(p);end else begin o2:=dt;v2:=dv;end;
  c_emit(op_NewObject,o2,v2,o1,v1);                         // Creamos el objeto, indicando que hemos de copiar el prototype
  c_emit(op_Call,o2,v2,o1,v1);                              // Ejecutamos el constructor (this sera restaurado a la vuelta)
  c_emit(op_Construct,o2,v2,opT_None,0);                    // Comprobamos si hay algo que restaurar
  // Si dt era result emitimos el move
  if (IsResult(dt,dv)) or (dt=opt_Param) then c_emit(op_Move,dt,dv,o2,v2);
  c_unallocreg(o2,v2);
  c_unallocreg(o1,v1);
  if b then c_unallocreg(dt,dv);
  c_emit(op_DestroyParams,opT_None,0,opT_none,0);          // Generamos un frame

end;


function  TJs_CodeGen.GenCodeForIn(p:p_node):boolean;
var o1,o2,os:cardinal;
    v1,v2,v3,vs:longint;
    c1,c2:pJs_CodeNode;
    p1:p_node;
begin
  p:=p.first;                                                                                           // Esta es la clausula IN
  if n_ChildCount(p)<>2 then begin result:=c_Error('Bad encoding in For In',p);exit;end;                // Que solo puede tener dos hijos
  p1:=p.first.next;result:=c_Locate(p1,o2,v2);if not result then exit;                                  // El segundo ha de ser un objeto
  v3:=c_allocreg(p);                                                                                    // Reservamos un registro V3 donde guardar el iterador
  c_emit(op_NewIter,opT_reg,v3,o2,v2);                                                                  // Generamos el iterador (sin asignar) en el registro V3
  c_unallocreg(o2,v2);                                                                                  // Ya no necesitamos el objeto
  p1:=p.first;                                                                                          // Esta es la referencia que recibira el valor del iterador
  c1:=c_NewLabel;                                                                                       // La etiqueta de vuelta
  if p1.kind=jst_Ident                 then  result:=c_Locate(p1,os,vs) else                            // Si la referencia es una variable
  if p1.kind in [jst_Index,jst_Member] then  begin                                                      // Si es un Objeto
                  result:=c_Locate(p1.first,o1,v1);                                                     // Localizamos el Objeto
                  result:=result and c_Locate(p1.first.next,o2,v2);                                     // Localizamos el objeto y su propiedad
                  os:=opt_Single;vs:=ops_Result;
                  end else result:=c_Error('For IN needs a reference',p);
  if not result then exit;
  c_emit(op_IterNext,os,vs,opT_reg,v3);                                                                 // Copia el valor del iterador en os,vs y avanza
  c2:=c_emit(op_Jump,opT_Flag,opF_NEq,opT_Label,c_incLabel);                                            // Saltamos a la salida
  if os=opt_Single then begin
                  c_Emit(op_SetProperty,o1,v1,o2,v2);                                                   // Si se trataba de un Objeto
                  c_unallocreg(o1,v1);c_unallocreg(o2,v2);
                  end;
  result:=GenCodeStatement(p.next);if not result then exit;                                             // Generamos el codigo
  c_emit(op_Jump,opT_None,0,opT_Label,c1.v1);                                                           // Volvemos al principio
  c_emit(op_Label,opT_Label,c2.v2,opt_None,0);                                                          // La etiqueta de salida
  c_unallocreg(opT_reg,v3);
  c_unallocreg(o1,v1);
end;

function  TJs_CodeGen.GenCodeFor(p:p_node):boolean;
var c1,c2:pJS_CodeNode;
    k1:cardinal;
    f,k2:longint;
begin
  p:=p.first;
  result:=GenCodeStatement(p);if not result then exit;  //
  c1:=c_NewLabel;                                       // El inicio del bucle
  p:=p.next;                                            // La condicion de salida
  if p.kind<>jst_None then begin
          // Si la expresion activa flags, la generamos directamente, sino la localizamos
          if js_flaggedOp(p.kind) then begin result:=GenCodeExpression(p,opT_None,0);f:=c_tokenToFlag(p.kind,false);end
                                  else begin result:=c_Locate(p,k1,k2);c_emit(op_Compare,k1,k2,opt_Null,0);c_unallocreg(k1,k2);f:=opF_NEq;end;
          if not result then exit;
          // Si la expresion no se cumple saltamos al final
          c2:=c_emit(op_Jump,opT_Flag,f,opt_Label,c_incLabel);
          end else c2:=nil;
  p:=p.next;
  // Emitimos el codigo
  result:=GenCodeStatement(p.next);if not result then exit;
  // Emitimos el incremento
  if p.kind<>jst_None then result:=GenCodeStatement(p);
  // Volvemos al principio
  c_emit(op_Jump,opt_None,0,opt_Label,c1.v1);
  // Salida del bucle
  if c2<>nil then c_emit(op_Label,opT_Label,c2.v2,opt_None,0);
end;


function  TJs_CodeGen.GenCodeBreak(p:p_node):boolean;
begin
  p.value:=c_incLabel;
  result:=c_emit(op_Jump,opT_None,0,opt_Label,p.value)<>nil;
end;

function  TJs_CodeGen.GenCodeBreakHelper(p:p_node):boolean;
begin
 result:=c_Emit(op_Label,opt_Label,p.data.value,opt_None,0)<>nil;
end;

// Genera codigo para una expresion que es convertible a BOOLEANA, devuelve
// los flags que se activan si la expresion es TRUE -> Si falla devuelve opF_BADFLAG
function  TJs_CodeGen.GenCodeFlaggedExpresssion(p:p_node):longint;
var k1:cardinal;
    k2:longint;
begin
 result:=opF_BADFLAG;
 // Si la expresion es de tipo flagged la hacemos y devolvemos el FLAG que se activa si la expresion es cierta
 if js_flaggedOp(p.kind) then begin if GenCodeExpression(p,opT_None,0) then result:=c_tokenToFlag(p.kind,true);exit;end;

 // Si llegamos aqui se trata de una expresion que no activa Flags Directamente
 if not c_Locate(p,k1,k2) then exit;              // Calculamos la expresion
 c_emit(op_Compare,k1,k2,opt_Null,0);             // La comparamos con NULL
 c_unallocreg(k1,k2);
 result:=opF_NEq;                                 // La expresion sera OK si se activa el FLAG de EQUAL
end;



function  TJs_CodeGen.GenCodeWhile(p:p_node):boolean;
var p1:p_node;
    c1,c2:pJs_CodeNode;
    f:longint;
begin
  p1:=p.first;result:=true;
  // En el caso particular de siempre false, se trata de DeadCode que nunca se ejecuta
  if (p1.kind=jst_bool) and (p1.value=0) then exit;
  // Generamos una etiqueta
  c1:=c_NewLabel;
  // Si tenemos un booleano solo puede ser true (la expresion nunca falla), en otro caso generamos codigo para calcular la expression
  if p1.kind<>jst_bool then begin
         f:=GenCodeFlaggedExpresssion(p1);result:=f<>opF_BADFLAG;if not result then exit;
         c2:=c_emit(op_Jump,opT_Flag,c_InvertFlag(f),opt_Label,c_incLabel);  // Si la expresion falla salimos
         end else c2:=nil;
  p1:=p1.next;
  result:=GenCodeStatement(p1);
  c_emit(op_Jump,opt_None,0,opT_Label,c1.v1);                   // Volvemos al principio
  if c2<>nil then c_Emit(op_Label,opt_Label,c2.v2,opt_None,0);  // El punto de salida
end;



function  TJs_CodeGen.GenCodeTry(p:p_node):boolean;
var nc,nf:p_node;
    i1,i2,old_err_ident:longint;
begin
  if n_kind(p.parent)=jst_finally   then begin nf:=p.next;nf.value:=c_incLabel;end else nf:=nil;               // nf nos dice si tenemos nodo de finally
  if n_kind(p.first.next)=jst_catch then begin nc:=p.first.next.first;nc.value:=c_incLabel;end else nc:=nil;   // nc nos dice si tenemos node de catch
  i1:=c_incLabel;                                                                                              // La etiqueta de final de bloque
  i2:=c_incLabel;                                                                                              // La etiqueta de salida del codigo
  c_emit(op_PushFrame,opt_FrameError,opFr_TryFrame,opt_Offset,1);                                              // Iniciamos un bloque de Try, necesario. El retorno es +1
                                                                                                               // para instrucciones que rompen el flujo: return, break,...
  if nf<>nil then c_emit(op_PushFrame,opt_FrameError,opFr_FinalFrame,opT_Label,nf.value);                           // Pusheamos la direccion de Finally
  if nc<>nil then c_emit(op_PushFrame,opt_FrameError,opFr_CatchFrame,opT_Label,nc.value);                           // Pusheamos la direccion de Catch
  result:=GenCodeStatement(p.first);if not result then exit;                                                   // Hacemos el codigo del Try
  if nc<>nil then c_emit(op_PopFrame,opt_FrameError,opFr_CatchFrame,opt_None,0);                               // Si hemos llegado aqui no ha habido error y eliminamos el CatchFrame
  if nf<>nil then c_emit(op_PopFrame,opt_FrameError,opFr_FinalFrame,opt_None,0)                                // Si tenemos un FinalFrame lo ejecutamos
             else c_emit(op_Jump,opt_None,0,opT_Label,i2);                                                     // Sino saltamos a detras del bloque TRY|CATCH|FINALLY
  if nc<>nil then begin                                                                                        // Si hay un error
                  c_emit(op_Label,opT_Label,nc.value,opt_None,0);                                              // Aqui comienza el bloque de catch
                  old_err_ident:=err_ident;err_ident:=nc.hash;                                                 // Dentro del Catch tenemos activa la variable ERROR
                  result:=GenCodeStatement(nc.next);if not result then exit;                                   // Hacemos el codigo del Catch
                  err_ident:=old_err_ident;                                                                    // Desactivamos el acceso al ident de error dentro del catch
                  if nf<>nil then c_emit(op_PopFrame,opt_FrameError,opFr_FinalFrame,opt_None,0)                // Si tenemos un FinalFrame lo ejecutamos
                             else c_emit(op_Jump,opt_None,0,opT_Label,i2);                                     // Sino saltamos a detras del bloque TRY|CATCH|FINALLY
                  end;
  if nf<>nil then begin
                  c_emit(op_Label,opT_Label,nf.value,opt_None,0);                                              // Aqui comienza el bloque de finally
                  result:=GenCodeStatement(nf);                                                                // Generamos el codigo de Finally
                  end;
  c_emit(op_Label,opT_Label,i2,opt_None,0);                                                                    // Esto es el final del bloque TRY|CATCH|FINALLY
  c_emit(op_PopFrame,opt_FrameError,opFr_TryFrame,opt_None,0);                                                      // Salimos del todo
  c_emit(op_Label,opT_Label,i1,opt_None,0);                                                                    // Esto es el despues del bloque TRY|CATCH|FINALLY

end;



function  TJs_CodeGen.GenCodeThrow(p:p_node):boolean;
var et:cardinal;
    ev:longint;
begin
  result:=c_Locate(p.first,et,ev);    // Localizamos la expresion a lanzar
  c_emit(op_Throw,et,ev,opt_none,0);             // La lanzamos
  c_unallocreg(et,ev);                // La "deslocalizamos"  
end;

function  TJs_CodeGen.GenCodeIf(p:p_node):boolean;
var p1:p_node;
    c1,c2:pJs_CodeNode;
    f:longint;
begin
  p1:=p.first;
  // Si siempre es true o false NO generamos codigo para la parte que no se ejecuta nunca
  if p1.kind=jst_bool then begin p:=p1.next;if p1.value=0 then p:=n_next(p);p:=n_first(p);result:=GenCodeStatement(p);exit;end;
  // Generamos codigo para la expresion, devuelve el FLAG si la expresion es cierta
  f:=GenCodeFlaggedExpresssion(p1);result:=f<>opF_BADFLAG;if not result then exit;
  // Si la expresion NO Se cumple saltamos al final o al else
  c1:=c_emit(op_Jump,opT_Flag,c_InvertFlag(f),opt_Label,c_incLabel);
  p1:=p1.next;result:=GenCodeStatement(p1);if not result then exit;     // Generamos codigo para el bloque de true
  p1:=p1.next;
  if p1<>nil then c2:=c_Emit(op_Jump,opt_None,0,opt_Label,c_incLabel);  // Si tenemos un bloque de else nos lo saltamos
  c_Emit(op_Label,opt_Label,c1.v2,opt_None,0);
  if p1<>nil then begin result:=GenCodeStatement(p1);c_Emit(op_Label,opt_Label,c2.v2,opt_None,0);end;
end;

function  TJs_CodeGen.GenCodeIfOperator(p:p_node;dt:cardinal;dv:longint):boolean;
var p1:p_node;
    c1,c2:pJs_CodeNode;
    k1:cardinal;
    f,k2:longint;
begin
  p1:=p.first;
  // Si siempre es true o false NO generamos codigo para la parte que no se ejecuta nunca
  if p1.kind=jst_bool then begin p:=p1.next;if p1.value=0 then p:=n_next(p);p:=n_first(p);result:=GenCodeStatement(p);exit;end;
  // Si la expresion activa flags, la geeneramos directamente, sino la localizamos
  if js_flaggedOp(p1.kind) then begin result:=GenCodeExpression(p1,opT_None,0);f:=c_tokenToFlag(p1.kind,false);end
                           else begin result:=c_Locate(p1,k1,k2);c_emit(op_Compare,k1,k2,opt_Null,0);c_unallocreg(k1,k2);f:=opF_NEq;end;
  if not result then exit;
  c1:=c_emit(op_Jump,opT_Flag,f,opt_Label,c_incLabel);
  p1:=p1.next;result:=GenCodeExpression(p1,dt,dv);if not result then exit;  // Generamos codigo para el bloque de true
  p1:=p1.next;
  if p1<>nil then c2:=c_Emit(op_Jump,opt_None,0,opt_Label,c_incLabel);
  // Emitimos un label
  c_Emit(op_Label,opt_Label,c1.v2,opt_None,0);
  if p1<>nil then begin result:=GenCodeExpression(p1,dt,dv);c_Emit(op_Label,opt_Label,c2.v2,opt_None,0);end;
end;




function TJs_CodeGen.GenCodeCall(p:p_node;dt:cardinal;dv:longint):boolean;
var ok,o1:cardinal;
    ov,v1:longint;
    c:pJs_CodeNode;
    p1:p_node;
begin
  p1:=p.first;result:=true;                                                                                          // Este es el nodo con a que llamamos
  p:=p1.next;c:=c_emit(op_CreateParams,opT_Integer,0,opT_none,0);                                                    // Vamos a los parametros y generamos un frame de llamada
  while (p<>nil) and result do begin result:=GenCodeExpression(p,opT_Param,c.v1);c.v1:=c.v1+1;p:=p.next;end;         // Y metemos en el mismo los parametros
  if not result then exit;                                                                                           // Si ha ocurrido algun fallo salimos
  if p1.kind in [jst_Index,jst_Member] then begin ok:=cur_ThisK;ov:=cur_thisV;end else ok:=opt_None;                 // Si la funcion a llamar es un miembro o indice, en el calculo de la misma se sobreescribira Cur_This
  result:=c_Locate(p1,o1,v1);if not result then exit;                                                                // Localizamos la funcion a llamar
  c_emit(op_Call,cur_ThisK,cur_thisV,o1,v1);                                                                         // Generamos el call
  c_emit(op_DestroyParams,opt_None,0,opt_None,0);                                                                    // Devolvemos los parametros (NO COPIA result)
  c_emit(op_Move,dt,dv,opt_Single,ops_Result);                                                                       // Copiamos result
  c_unallocreg(o1,v1);                                                                                               // Por si acaso hemos hecho un alloc de o1,v1
  if (ok<>cur_ThisK) then begin                                                                                       // Si se ha modificado This
        c_unallocreg(cur_ThisK,cur_ThisV);                                                                           // Liberamos
        cur_ThisK:=ok;cur_thisV:=ov;                                                                                 // Y restauramos
        end;

end;

function TJs_CodeGen.GenCodeIndex(p:p_node;dt:cardinal;dv:longint):boolean;
var r,v1,v2:longint;
    o1,o2:cardinal;

    keep_this:boolean;
begin
  keep_this:=p.parent.kind=jst_fcall;                                                                               // Marcamos si estamos en un call
  p:=p.first;result:=c_Locate(p,o1,v1);if not result then exit;                                                     // Obtenemos la base
  p:=p.next;                                                                                                        // Ahora la referencia
  if (p.parent.kind=jst_member) then begin o2:=opT_ExtPChar;v2:=js_program_AddCodeString(programa,p.text);end       // . ident lo tratamos como ['ident']
                                else begin result:=c_Locate(p,o2,v2);if not result then exit;end;                   // Cualquier otra expresion la resolvemos
  if keep_this then begin
        if o1<>opt_Reg then r:=c_allocreg(p) else r:=v1;
        c_emit(op_ToObject,opt_Reg,r,o1,v1);
        o1:=opt_Reg;v1:=r;
        end;
  c_emit(op_GetProperty,o1,v1,o2,v2);                                                                               // Obtenemos la propiedad
  c_emit(op_Move,dt,dv,opt_Single,ops_Result);                                                                      // La copiamos en result
  if not keep_this then c_unallocreg(o1,v1) else begin cur_thisK:=o1;cur_thisV:=v1;end;                             // Si no estamos en call hacemos unalloc. Si estamos en call aun lo necesitaremos  
  c_unallocreg(o2,v2);
end;


function TJs_CodeGen.GenCodeExpressionGroup(p:p_node;dt:cardinal;dv:longint):boolean;
begin
  p:=p.first;result:=true;
  while (p.next<>nil) and result do begin result:=GenCodeExpression(p,opT_None,0);p:=p.next;end;
  if not result then exit;
  result:=GenCodeExpression(p,dt,dv);
end;

function TJs_CodeGen.GenCodeExpression(p:p_node;dt:cardinal;dv:longint):boolean;
begin
   if p=nil then begin s_alert('GenCodeExpression for nil');result:=false;Exit;end;

   case p.kind of
     jst_New:        result:=GenCodeNewExpression(p.first,dt,dv);
     jst_ExpGroup:   result:=GenCodeExpressionGroup(p,dt,dv);
     jst_FuncDef:    result:=GenCodeFuncExpression(p,dt,dv);
     jst_Assign:     result:=GenCodeAssign(p,dt,dv);
     jst_Index:      result:=GenCodeIndex(p,dt,dv);
     jst_member:     result:=GenCodeIndex(p,dt,dv);
     jst_fcall:      result:=GenCodeCall(p,dt,dv);
     jst_pchar:      begin c_emit(op_Move,dt,dv,opT_ExtPChar,js_program_AddCodeString(programa,p.text));result:=true;end;
     jst_Integer:    begin c_emit(op_Move,dt,dv,opT_Integer,s_s2i(p.text));result:=true;end;
     jst_This:       begin c_emit(op_Move,dt,dv,opt_Single,opS_This);result:=true;end; // c_emit ya filtra los automove(s)
     jst_Null:       begin c_emit(op_Move,dt,dv,opt_Null,0);result:=true;end;
     jst_bool:       begin c_emit(op_Move,dt,dv,opt_Boolean,p.value);result:=true;end;
     jst_ident:      result:=GenCodeIdent(p,dt,dv);
     jst_ObjectDef:  result:=GenCodeObject(p,dt,dv);
     jst_ArrayDef:   result:=GenCodeArrayDef(p,dt,dv);
     jst_CondIf:     result:=GenCodeIfOperator(p,dt,dv);
     jst_PostInc:    result:=GenCodePostInc(p,dt,dv,1);
     jst_PostDec:    result:=GenCodePostInc(p,dt,dv,-1);
     jst_PreInc:     result:=GenCodePreInc(p,dt,dv,1);
     jst_PreDec:     result:=GenCodePreInc(p,dt,dv,-1);
     jst_Or:         result:=GenCodeOrExpression(p,dt,dv);
     jst_And:        result:=GenCodeAndExpression(p,dt,dv);
     jst_Float:      result:=GenCodeFloat(p,dt,dv);
     jst_None:       result:=true;
     jst_TypeOf,jst_Not,jst_Negative,jst_Positive,jst_bNot: result:=GenCodeUnaryOperator(p,dt,dv);
     //jst_Begin:      begin result:=true;p:=p.first;while result and (p<>nil) do begin result:=GenCode
     else if js_kindIsBinOp(p.kind) then result:=GenCodeBinOperator(p,dt,dv)
                                    else result:=c_error('Not implemented genCode for '+s_i2s(p.kind),p);

    end;
end;


function  TJs_CodeGen.GenCodeFuncExpression(p:p_node;dt:cardinal;dv:longint):boolean;
var p1:p_node;
begin
   // Nos aseguramos de que la Function_Expresion tenga codigo asociado
   p1:=p.data;if n_kind(p1)<>jst_CodeDef  then begin result:=c_Error('Internal error FE without body',p);exit;end;
   // El codigo asociado debe de tener un bloque de generacion de codigo, lo creamos ya.
   if p1.value=0 then p1.value:=js_program_AddFunction(programa,p1.text);
   // Creamos un objeto funcion en dt,dv y que llame el bloque de codigo
   c_emit(op_NewFunc,dt,dv,opT_Block,p1.value);
   // Queda pendiente la copia de variables en el objeto a efectos de closures !!
   result:=true;
end;


function  TJs_CodeGen.GenCodeGetterSetter(p:p_node;dt:cardinal;dv:longint):boolean;
var p1,p2:p_node;
    k:cardinal;
    l:longint;
begin
   l:=js_program_AddCodeString(programa,p.text);p1:=p.first;                                                                // Guardamos el nombre y vamos al primer hijo
   while p1<>nil do begin
          p2:=p1.data;                                                                                                      // Vamos al primer Getter o Setter
          if n_kind(p2)<>jst_CodeDef  then begin result:=c_Error('Internal error GET-SET without body',p1);exit;end;        // Que debe tener codigo
          if p2.value=0 then p2.value:=js_program_AddFunction(programa,p2.text);                                            // Que debe haber generado un bloque
          c_emit(op_NewFunc,opt_Single,ops_Result,opT_Block,p2.value);                                                      // Creamos una funcion que apunta a ese bloque
          if p1.kind=jst_Getter then k:=op_Getter else k:=op_Setter;                                                        // ¿ Es un getter o un setter ?
          c_emit(k,dt,dv,opT_ExtPChar,l);                                                                                   // Emitimos la creacion del mismo
          p1:=p1.next;
          end;
   result:=true;
end;



function  TJs_CodeGen.GenCodeObject(p:p_node;dt:cardinal;dv:longint):boolean;
var et:cardinal;
    ev:longint;
begin
   // A no se que se trate del objeto vacio, vamos a machacar RESULT, asi que si lo necesitamos...
   if (IsResult(dt,dv)) and (p.first<>nil) then begin et:=opt_reg;ev:=c_allocreg(p);end else begin et:=dt;ev:=dv;end;
   // Generamos el objeto y lo guardamos en dt,dv
   c_emit(op_NewObject,et,ev,opt_none,0);
   // ahora vamos a hacer las propiedades y metodos del objetos
   p:=p.first;result:=true;
   while (p<>nil) and (result) do begin
            case p.kind of
                  jst_Member: begin result:=genCodeExpression(p.first,opt_Single,ops_Result);c_Emit(op_SetProperty,et,ev,opT_ExtPChar,js_program_AddCodeString(programa,p.text));end;
                  jst_GetSet: result:=GenCodeGetterSetter(p,et,ev);
                  else result:=c_Error('Internal Error in Object members',p);
                  end;
            p:=p.next;
            end;
   if (et<>dt) then begin c_emit(op_Move,dt,dv,et,ev);c_unallocreg(et,ev);end;
end;


// Genera el codigo de una funcion, durante la generacion del codigo pueden "crearse" en el nodo de variables
// las variables indirectas (que corresponden a closures). Como cuando se conoce esto ya se han emitido los opcodes de variables
// hay dos posibilidades: insertar los opcodes de variables tras la generacion de statements
//                        generar el codigo fuera de PROGRAM, luego las variables en PROGRAM y mover el codigo a PROGRAM (esta es la opcion implementada)
function  TJs_CodeGen.GenFunctionBody(p:p_node):boolean;
var oc:pJs_Function;
    pfunc_ident:cardinal;
    pv,p1:p_node;
    s:string;
begin
  pv:=vars;s:=c_FunctionRealName(p.text);                                                                               // Este es el nombre real de la funcion
  pfunc_ident:=func_ident;if (s<>'') and (p.number=1) then func_ident:=s_hash(s) else func_ident:=0;                    // Si la funcion era anonima y tenia un nombre usaremos CurFunc para implementar recursividad 
  result:=p.value<>0;if not result then begin c_Error('Missing function code',p);exit;end;                              // No deberia de ocurrir nunca, para eso esta el parser
  vars:=p.first;cb:=js_function_create;oc:=cb;                                                                          // Creamos un bloque de codigo nuevo, y lo hacemos CB (current_block)
  p1:=vars.next;while (p1<>nil) and result do begin result:=GenCodeStatement(p1);p1:=p1.next;end;                       // Generamos el codigo
  if result then begin                                                                                                  // Si todo ha ido bien
              cb:=js_program_FunctionByIndex(programa,p.value);                                                         // Volvemos al bloque que habiamos reservado para la funcion
              cb.src_start:=p.pos;cb.src_len:=p.len;                                                                    // guardamos donde comienza y la longitud del bloque (solo tiene sentido para funciones)
              p1:=vars.first;cb.nvars:=vars.value;cb.nvars_cl:=vars.number;                                             // Guardamos cuantas variables se definen en stack y cuantas en GC
              while (p1<>nil) and result do begin result:=c_emitVar(cb,p1,opT_Var);p1:=p1.next;end;                     // Y emitimos el codigo para generar las variables, incluidas las capturadas !!
              b_pad(cb.programa.integers,4);                                                                            // Padeamos  
              js_function_move(cb,oc);                                                                                  // Finalmente movemos el codigo de oc a cb
              cb.nlabels:=oc.nlabels;
              if n_kind(n_LastBrother(p.first))<>jst_Return then c_Emit(op_Ret,opT_None,0,opt_None,0);          // Si la ultima instruccion NO es return añadimos un return
              end;
  js_function_Free(oc);                                                                                                 // Liberamos la funcion
  vars:=pv;func_ident:=pfunc_ident;
end;

{
procedure   TJs_CodeGen.PrintCode;
var p:pjs_smalllist;
    n,k:longint;
    d:pJs_CodeNode;
    s,r1,r2:string;
begin
  js_print(' ');
  p:=programa.blocks;k:=js_smalllist_count(p);n:=0;
  while k>0 do begin
        cb:=p^;
        js_print('----------------------------------------------------------------------');
        js_print('Block: '+js_i2s(n)+'  '+js_program_Pchar(programa,cb.name));
        d:=cb.first;
        while d<>nil do begin
                s:=js_pad(js_LCode2Str(d.opcode,d.k1,d.v1,d.k2,d.v2),50);
                if d.k1=opT_PChar then r1:=js_pad('p1='+js_program_Pchar(programa,d.v1),15)+'  ' else r1:='';
                if d.k2=opT_PChar then r2:=js_pad('p2='+js_program_Pchar(programa,d.v2),15) else r2:='';

                if (r1<>'') or (r2<>'') then s:=s+';  ';
                if r1<>'' then s:=s+r1;
                if r2<>'' then s:=s+r2;
                js_print( js_pad( js_Pointer2Str(d),15)+s);
                d:=d.next;
                end;
        k:=k-1;inc(p);n:=n+1;
        end;
end;
}

initialization

 // Cuando se ejecute una comparacion Booleana, el flag que se activa, dependera del resultado de la operacion. Esto se mapea aqui para
 // cada operador
 JS_BoolOpRes[jst_Equals    ,true]:=opF_Eq;
 JS_BoolOpRes[jst_Diff      ,true]:=opF_NEq;
 JS_BoolOpRes[jst_strictEq  ,true]:=opF_Eq;
 JS_BoolOpRes[jst_strictDif ,true]:=opF_NEq;
 JS_BoolOpRes[jst_Big       ,true]:=opF_Big;
 JS_BoolOpRes[jst_BigEq     ,true]:=opF_BigEq;
 JS_BoolOpRes[jst_Less      ,true]:=opF_Less;
 JS_BoolOpRes[jst_LessEq    ,true]:=opF_LessEq;
 JS_BoolOpRes[jst_In        ,true]:=opF_Eq;

 JS_BoolOpRes[jst_Equals   ,false]:=opF_NEq;
 JS_BoolOpRes[jst_Diff     ,false]:=opF_Eq;
 JS_BoolOpRes[jst_strictEq ,false]:=opF_NEq;
 JS_BoolOpRes[jst_strictDif,false]:=opF_Eq;
 JS_BoolOpRes[jst_Big      ,false]:=opF_NBig;
 JS_BoolOpRes[jst_BigEq    ,false]:=opF_NBigEq;
 JS_BoolOpRes[jst_Less     ,false]:=opF_NLess;
 JS_BoolOpRes[jst_LessEq   ,false]:=opF_NLessEq;
 JS_BoolOpRes[jst_In       ,false]:=opF_NEq;

end.

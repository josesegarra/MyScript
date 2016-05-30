unit js_tokens;

interface

uses fw_utils,fw_vm_constants;

const
     // Los tokens de control del lexer
      jst_Root       =255;
      jst_None       =254;
      jst_Error      =253;
      jst_Eof        =252;
      jst_vars       =251;
      jst_Functions  =250;

      jst_CodeDef    =249;
      jst_FuncDef    =248;
      jst_CodeDef2   =247;
      jst_ObjectDef  =246;
      jst_ArrayDef   =245;
      jst_Unassigned =244;

      jst_Internal   =240;
      // Los tokens de simbolos
      jst_Begin      =060;
      jst_End        =059;
      jst_OpenPar    =058;
      jst_ClosePar   =057;
      jst_OpenB      =056;
      jst_CloseB     =055;
      jst_PComma     =054;
      jst_Dot        =053;
      jst_Comma      =052;
      jst_2Puntos    =051;
      jst_ExpGroup   =050;



      // Los tokens de palabras clave
      jst_GCopy      =040;
      jst_if         =039;
      jst_else       =038;
      jst_while      =037;
      jst_try        =036;
      jst_catch      =035;
      jst_finally    =034;
      jst_Return     =033;
      jst_Function   =032;
      jst_Var        =031;
      jst_Throw      =030;
      jst_Param      =029;
      jst_ExtValue   =028;
      jst_IndParam   =027;
      jst_IndVar     =026;
      jst_LinkCopy   =025;
      jst_For        =024;
      jst_Break      =023;
      jst_Continue   =022;
      jst_BreakHelper=021;
      jst_Do         =020;
      jst_ForIn      =019;
      jst_ForEachIn  =018;
      jst_Each       =017;
      jst_This       =016;
      jst_Null       =015;
      jst_Undefined  =014;
      jst_Getter     =013;  // Es un get dentro de la definicion de un Objeto
      jst_Setter     =012;  // Es un set dentro de la definicion de un Objeto
      jst_GetSet     =011;

      // Los tokens de elementos
      jst_pchar      =001;
      jst_float      =002;
      jst_bool       =003;
      jst_ident      =004;
      jst_true       =005;
      jst_false      =006;
      jst_integer    =007;
      jst_pchar2     =009;
      
      // Los operadores unarios que van antes
      jst_New        =101;
      jst_PreInc     =102;
      jst_PreDec     =103;
      // Operadores binarios
      jst_Not        =OP_NOT;
      jst_bnot       =105;
      jst_Positive   =106;
      jst_Negative   =107;
      jst_TypeOf     =OP_TYPEOF;
      jst_delete     =110;
      jst_Void       =111;
      // Los unarios posteriores
      jst_member     =112;
      jst_fcall      =113;
      jst_PostInc    =114;
      jst_PostDec    =115;
      jst_Index      =116;
      // Los operadores binarios despues
      jst_Times      =OP_TIMES;
      jst_Div        =OP_DIV;
      jst_Mod        =OP_MOD;
      jst_Add        =OP_ADD;
      jst_Minus      =OP_MINUS;
      jst_shiftl     =OP_SHIFTL;
      jst_shiftr     =OP_SHIFTR;
      jst_shiftrz    =OP_SHIFTRZ;
      jst_instance   =OP_INSTANCE;



      jst_Equals     =160;
      jst_Diff       =161;
      jst_strictEq   =162;
      jst_strictDif  =163;
      jst_Less       =164;
      jst_LessEq     =165;
      jst_Big        =166;
      jst_BigEq      =167;
      jst_In         =168;
      jst_Min_BoolOp =jst_Equals;
      jst_Max_BoolOp =jst_In;



      jst_bAnd       =169;
      jst_bXOr       =170;
      jst_bOr        =171;
      jst_And        =172;
      jst_Or         =173;


      // El operador Ternario condicional If
      jst_CondIf     =174;

      // Aqui van los operadores de asignacion (con menos prioridad)
      jst_Assign     =175;
      jst_AsgnAdd    =176;
      jst_AsgnMinus  =177;
      jst_AsgnTimes  =178;
      jst_AsgnDiv    =179;
      jst_AsgnMod    =180;
      jst_AsgnLeft   =181;
      jst_AsgnRight  =182;
      jst_AsgnZRight =183;
      jst_AsgnAnd    =184;
      jst_AsgnXor    =185;
      jst_AsgnOr     =186;


function js_token2str(k:cardinal):string;

function js_maxBinOp:integer;
function js_minBinOp:integer;
function js_maxPreOp:integer;
function js_minPreOp:integer;

// Normalmente un token se corresponde con un operador
// pero hay algunos que afectan a mas de uno:
//    + (Signo Positivo / Suma) o - (Signo Negativo / Resta)
//    ++ (PreInc / PostInc) o -- (PreDec/PostDec)
//


// Por eficiencia todos operadores BINARIOS coinciden con su token
function js_tokenIsBinOp(token,ope:cardinal):boolean;
// Esta funcion resuelve las ambiguedades de los tokens unarios
function js_token2UnaryOp(token:cardinal;pre:boolean):cardinal;
// Nos dice si un token es unario (pre o post)
function js_tokenIsUnaryPreOp(token:cardinal):boolean;
function js_tokenIsUnaryPostOp(token:cardinal):boolean;
function js_tokenIsLoop(token:cardinal):boolean;
function js_keyword(h:cardinal):cardinal;
// Nos dice si Kind es un operador binario
function js_kindIsBinOp(k:cardinal):boolean;

// Nos dice si un operador se evalua IZ->DER o DER-IZ
function js_tokenEvalLeft(ope:cardinal):boolean;

// Nos dice si un operador afecta a los flags (pej. operadores de comparacion)
function js_flaggedOp(k:cardinal):boolean;

// Nos dice si un operador debe copiar el resultado en el primer operando (pej. a+=b)
function js_IsAssignOp(k:cardinal):boolean;


implementation


type  TJs_KeyWord=record hash,value:cardinal;end;

const JS_NKEYWORDS    =21;        // Cuantas Keywords tenemos

var   JS_KeyWords: array[0..JS_NKEYWORDS] of TJS_KeyWord;   // Palabras clave

      //   PRE: new, ++  -- ! ~ + - typeof void delete  CLASS_NAME
      JS_Op_Pre:   array[0..9] of byte=(jst_New,jst_PreInc,jst_PreDec,jst_Not,jst_bnot,jst_Positive,jst_Negative,jst_TypeOf,jst_void,jst_delete);
      //   POST -> EXTENDIDOS . [] () y SIN EXTENDER ++ --
      JS_Op_Pos:   array[0..4] of byte=(jst_member,jst_index,jst_fcall,jst_PostInc,jst_PostDec);
      //   binarios * / % + - << >> >>>
      //           < <= > >= in instanceof == !=
      //           === !== & ^ | && ||  =
      //           += -= *= /= %= <<= >>= >>>= &= ^= |=

      JS_Op_Bin:   array[0..35] of byte=(jst_Times,jst_Div,jst_Mod,jst_Minus,jst_Add,jst_shiftl,jst_shiftr,jst_shiftrz,
                                        jst_Less,jst_LessEq,jst_Big,jst_BigEq,jst_In,jst_instance,jst_Equals,jst_Diff,
                                        jst_strictEq,jst_strictDif,jst_bAnd,jst_bXOr,jst_bOr,jst_And,jst_Or,
                                        jst_CondIf,
                                        jst_Assign,jst_AsgnAdd,jst_AsgnMinus,jst_AsgnTimes,jst_AsgnDiv,jst_AsgnMod,
                                        jst_AsgnLeft,jst_AsgnRight,jst_AsgnZRight,jst_AsgnAnd,jst_AsgnXor,jst_AsgnOr);
      // Nos dejamos estos ya que al ser unicos los gestionamos directamente desde el parser
      //   trinarios ?:
      //   multiples ,




function js_maxBinOp:integer;
begin
  result:=high(JS_Op_Bin);
end;

function js_minBinOp:integer;
begin
  result:=low(JS_Op_Bin);
end;

function js_tokenIsLoop(token:cardinal):boolean;
begin
  result:=token in [jst_For,jst_while,jst_Do,jst_ForEachIn,jst_ForIn];
end;


function js_flaggedOp(k:cardinal):boolean;
begin
 result:=k in [jst_Less,jst_LessEq,jst_Big,jst_BigEq,jst_In,jst_Equals,jst_Diff];
end;

function js_IsAssignOp(k:cardinal):boolean;
begin
 result:=k in [jst_AsgnAdd,jst_AsgnMinus,jst_AsgnTimes,jst_AsgnDiv,jst_AsgnMod,jst_AsgnLeft,jst_AsgnRight,jst_AsgnZRight,jst_AsgnAnd,jst_AsgnXor,jst_AsgnOr];
end;


function js_maxPreOp:integer;
begin
  result:=high(JS_Op_Pre);
end;

function js_minPreOp:integer;
begin
  result:=low(JS_Op_Pre);
end;


function js_tokenIsBinOp(token,ope:cardinal):boolean;
begin
  if (ope<=High(JS_Op_Bin)) then result:=token=JS_Op_Bin[ope] else result:=false;
end;



function js_kindIsBinOp(k:cardinal):boolean;
var i:longint;
begin
  result:=false;i:=low(JS_Op_Bin);
  while (result=false) and (i<=High(JS_Op_Bin)) do begin result:=JS_Op_Bin[i]=k;i:=i+1;end;
end;


function js_tokenEvalLeft(ope:cardinal):boolean;
begin
  if  (ope<=High(JS_Op_Bin)) then result:=JS_Op_Bin[ope]<>jst_Assign else result:=true;
end;


function js_token2UnaryOp(token:cardinal;pre:boolean):cardinal;
begin
  result:=token;
  case token of
    jst_Dot:        if not pre then result:=jst_Member;
    jst_OpenPar:    if not pre then result:=jst_fcall;
    jst_OpenB:      if not pre then result:=jst_index;
    jst_PostInc:    if pre then result:=jst_PreInc else result:=jst_PostInc;
    jst_PostDec:    if pre then result:=jst_PreDec else result:=jst_PostDec;
    jst_Add:        if pre then result:=jst_Positive;
    jst_Minus:      if pre then result:=jst_Negative;
  end;
end;


function js_tokenIsUnaryPreOp(token:cardinal):boolean;
var i:longint;
begin
  result:=false;i:=js_minPreOp;
  while (result=false) and (i<=js_maxPreOp) do begin result:=JS_Op_Pre[i]=token;i:=i+1;end;

end;

function js_tokenIsUnaryPostOp(token:cardinal):boolean;
var i:longint;
begin
  result:=false;i:=low(JS_Op_Pos);
  while (result=false) and (i<=high(JS_Op_Pos)) do begin result:=JS_Op_Pos[i]=token;i:=i+1;end;
end;

function js_token2str(k:cardinal):string;
begin
  case k of
    jst_Throw:      result:='[THROW]';  
    jst_TypeOf:     result:='[TYPE_OF]';
    jst_This:       result:='[THIS]';
    jst_ExpGroup:   result:='[EXP_GROUP]';
    jst_Getter:     result:='[GETTER]';
    jst_Setter:     result:='[SETTER]';

    jst_Positive:   result:='[POSITIVE]';
    jst_Null:       result:='[null]';
    jst_Negative:   result:='[NEGATIVE]';
    jst_fCall:      result:='[call]';
    jst_root:       result:='[ROOT]';
    jst_None:       result:='[NONE]';
    jst_vars:       result:='[VARS]';
    jst_Eof:        result:='[EOF]';
    jst_Begin:      result:='[BEGIN]';
    jst_IndParam:   result:='[IND_PARAM]';
    jst_IndVar:     result:='[IND_VAR]';
    jst_LinkCopy:   result:='[LINKCOPY]';
    jst_End:        result:='[END]';
    jst_ExtValue:   result:='[EXTVALUE]';
    jst_OpenPar:    result:='[OPENPAR]';
    jst_ClosePar:   result:='[CLOSEPAR]';
    jst_OpenB:      result:='[OPENB]';
    jst_CloseB:     result:='[CLOSEB]';
    jst_PComma:     result:='[PCOMMA]';
    jst_if:         result:='[IF]';
    jst_While:      result:='[WHILE]';
    jst_For:        result:='[FOR]';
    jst_ForIn:      result:='[FOR IN]';
    jst_New:        result:='[NEW]';
    jst_ForEachIn:  result:='[FOR EACH IN]';
    jst_Break:      result:='[BREAK]';
    jst_Do:         result:='[DO]';
    jst_BreakHelper:result:='[BREAK_HELPER]';
    jst_Continue:   result:='[CONTINUE]';
    jst_Index:      result:='[INDEX]';
    jst_integer:    result:='integer';
    jst_pchar:      result:='pchar';
    jst_float:      result:='float';
    jst_bool:       result:='bool';
    jst_ident:      result:='ident';
    jst_Add:        result:='[add]';
    jst_AsgnAdd:    result:='[add_asign]';
    jst_AsgnMinus:  result:='[minus_asign]';
    jst_Dot:        result:='[dot]';
    jst_Comma:      result:='[comma]';
    jst_Assign:     result:='[assign]';
    jst_AsgnAnd:    result:='[assign_and]';
    jst_Try:        result:='[try]';
    jst_Catch:      result:='[catch]';
    jst_Finally:    result:='[finally]';
    jst_ObjectDef:  result:='[object]';
    jst_ArrayDef:   result:='[array]';
    jst_2Puntos:    result:='[:]';
    jst_AsgnOr:     result:='[assign_or]';
    jst_Diff:       result:='[diff]';
    jst_Var:        result:='[var]';
    jst_Equals:     result:='[equals]';
    jst_Less:       result:='[less]';
    jst_Return:     result:='[return]';
    jst_LessEq:     result:='[less_eq]';
    jst_In:         result:='[in]';
    jst_Big:        result:='[bigger]';
    jst_BigEq:      result:='[bigger_eq]';
    jst_Minus:      result:='[minus]';
    jst_Times:      result:='[times]';
    jst_Div:        result:='[div]';
    jst_Undefined:  result:='[undefined]';
    jst_Mod:        result:='[mod]';
    jst_shiftl:     result:='[shiftl]';
    jst_shiftr:     result:='[shiftr]';
    jst_And:        result:='[and]';
    jst_Or:         result:='[or]';
    jst_PreInc:     result:='[pre_inc]';
    jst_PreDec:     result:='[pre_dec]';
    jst_Not:        result:='[not]';
    jst_CondIf:     result:='[if_operator]';
    jst_instance:   result:='[instance]';
    jst_Param:      result:='[param]';
    jst_PostInc:    result:='[post_inc]';
    jst_Functions:  result:='[functions]';
    jst_GetSet:     result:='[getset]';
    jst_PostDec:    result:='[post_dec]';
    jst_GCopy:      result:='[GCOPY]';

    //jst_CallParams: result:='[c_params]';
    jst_CodeDef:    result:='[code]';      // Define un bloque de codigo (pej. una funcion fuera de una expresion)
    jst_CodeDef2:   result:='[code_2]';    // Se trata del nodo anterior tras haber sido procesado por el Generador de Codigo 
    jst_FuncDef:    result:='[def_func]';  // Define una funcion dentro de una expresion
    jst_member:     result:='[property]';
    

    else            result:='-E:'+s_i2s(k);
  end;
end;

function js_keyword(h:cardinal):cardinal;
var i:cardinal;
begin
  result:=jst_ident;i:=low(JS_KeyWords);
  while (result=jst_ident) and (i<=JS_NKEYWORDS) do if JS_KeyWords[i].hash=h then result:=JS_KeyWords[i].value else i:=i+1;
end;

procedure Init_Tokens;
begin
 JS_KeyWords[0].hash :=s_Hash('if');        JS_KeyWords[0].value :=jst_If;
 JS_KeyWords[1].hash :=s_Hash('else');      JS_KeyWords[1].value :=jst_Else;
 JS_KeyWords[2].hash :=s_Hash('while');     JS_KeyWords[2].value :=jst_While;
 JS_KeyWords[3].hash:=s_Hash('typeof');     JS_KeyWords[3].value:=jst_TypeOf;
 JS_KeyWords[4].hash:=s_Hash('function');   JS_KeyWords[4].value:=jst_Function;
 JS_KeyWords[5].hash:=s_Hash('var');        JS_KeyWords[5].value:=jst_Var;
 JS_KeyWords[6].hash:=s_Hash('return');     JS_KeyWords[6].value:=jst_Return;
 JS_KeyWords[7].hash:=s_Hash('new');        JS_KeyWords[7].value:=jst_New;
 JS_KeyWords[8].hash:=s_hash('this');       Js_KeyWords[8].value:=jst_This;
 JS_KeyWords[9].hash:=s_Hash('instanceof'); JS_KeyWords[9].value:=jst_Instance;
 JS_KeyWords[10].hash:=s_Hash('true');      JS_KeyWords[10].value:=jst_True;
 JS_KeyWords[11].hash:=s_Hash('false');     JS_KeyWords[11].value:=jst_False;
 JS_KeyWords[12].hash:=s_Hash('try');       JS_KeyWords[12].value:=jst_Try;
 JS_KeyWords[13].hash:=s_Hash('catch');     JS_KeyWords[13].value:=jst_Catch;
 JS_KeyWords[14].hash:=s_Hash('finally');   JS_KeyWords[14].value:=jst_Finally;
 JS_KeyWords[15].hash:=s_Hash('throw');     JS_KeyWords[15].value:=jst_Throw;
 JS_KeyWords[16].hash:=s_hash('for');       Js_KeyWords[16].value:=jst_For;
 JS_KeyWords[17].hash:=s_hash('break');     Js_KeyWords[17].value:=jst_Break;
 JS_KeyWords[18].hash:=s_hash('continue');  Js_KeyWords[18].value:=jst_Continue;
 JS_KeyWords[19].hash:=s_hash('each');      Js_KeyWords[19].value:=jst_Each;
 JS_KeyWords[20].hash:=s_hash('in');        Js_KeyWords[20].value:=jst_In;
 JS_KeyWords[21].hash:=s_hash('null');      Js_KeyWords[21].value:=jst_Null;                  // Null es un KeyWord
 // JS_KeyWords[22].hash:=s_hash('undefined'); Js_KeyWords[22].value:=jst_Undefined;          // Undefined NO y NaN tampoco


end;


initialization
 Init_Tokens;


end.

unit js_parser;

interface

uses js_lexer,fw_utils;

type
      TJs_Parser=class(TJs_Lexer)
        private
            function    s_IsValidSeparator:boolean;
            function    s_Error(s:string):boolean;
            function    s_ErrorN(s:string;p:p_node):boolean;
            function    s_ErrorP(s:string;p:p_node):pointer;
            function    s_node(p:p_node;k:cardinal):p_node;
            function    s_nodeP(p:p_node;k:cardinal):p_node;
            function    s_nodec(parent:cardinal;child:p_node):p_node;
            function    s_nodenc(parent:cardinal;child:p_node):p_node;
            function    s_expressionOrEmpty:p_node;
            function    s_name(s:string):string;
            function    s_identList(a:p_node;k:cardinal):boolean;
            function    s_ParamExpressionList(a:p_node):boolean;
            function    s_makelocal(p:p_node):boolean;
            function    Syntax_Statement:boolean;
            function    Syntax_TryStatement:boolean;
            function    Syntax_Statements(c:p_node):boolean;
            function    Syntax_GetterSetter(k:cardinal;r:p_node):p_node;
            function    Syntax_ObjectField(r:p_node):p_node;
            
            function    Syntax_IfStatement:boolean;
            function    Syntax_ForStatement:boolean;
            function    Syntax_ForEachStatement:boolean;
            function    Syntax_ForInStatement(v:boolean;p:p_node):boolean;
            function    Syntax_WhileStatement:boolean;
            function    Syntax_ThrowStatement:boolean;
            function    Syntax_Expression:p_node;
            function    Syntax_ExpressionGroup(p:p_node):p_node;
            function    Syntax_ExpressionGroupOn(p:p_node;k:longint):p_node;
            function    Syntax_BinExpression(op:longint):p_node;
            function    Syntax_TercExpression(op:longint;r:p_node):p_node;
            function    Syntax_VarStatement:boolean;

            function    Syntax_UnaryExpression:p_node;
            function    Syntax_Factor:p_node;
            function    Syntax_ObjectExpression:p_node;
            function    Syntax_ArrayExpression:p_node;
            function    Syntax_FunctionStatement:boolean;
            function    Syntax_BreakStatement:boolean;
            function    Syntax_ReturnStatement:boolean;
            function    Syntax_FunctionExpression(n_required:boolean):p_node;

        public
            root:p_node;
            cur,vars,funcs:p_Node;                                        // Nodos pivote
            function    s_nodeT(p:p_node;k:cardinal;s:string):p_node;
            constructor Create;
            destructor  Destroy; override;
            function    Parse:boolean;
            end;


var object_debug:longint=0;

implementation

uses js_tokens;


function    TJs_Parser.s_identList(a:p_node;k:cardinal):boolean;
begin
 repeat
      result:=token=jst_ident;
      if result then begin
            if n_FindChildHash(a,hash)<>nil then result:=s_error('Duplicated symbol') else s_node(a,k).value:=n_ValuePP(a);
            end else s_error('Expected ident');
      until (result=false) or (lex_gtg(jst_Comma)<>1);
 if result then result:=error='';
end;


function    TJs_Parser.s_name(s:string):string;
var i:longint;
begin
  i:=length(s);while (i>0) and (s[i]<>'#') do i:=i-1;
  result:=copy(s,i+1,length(s));
end;

function    TJs_Parser.s_nodeT(p:p_node;k:cardinal;s:string):p_node;
begin
  result:=n_CreateT(p,k,s);
  result.pos:=tpos-src;
  result.len:=pos-tpos;
end;

function    TJs_Parser.s_node(p:p_node;k:cardinal):p_node;
begin
  result:=n_CreateT(p,k,lex_token);
  result.pos:=tpos-src;
  result.len:=pos-tpos;
end;

function    TJs_Parser.s_nodeP(p:p_node;k:cardinal):p_node;
begin
  result:=n_CreateT(p,k,lex_token);
  result.pos:=tpos-src;
  result.len:=pos-tpos;
  result:=p;
end;

function    TJs_Parser.s_nodenc(parent:cardinal;child:p_node):p_node;
begin
  result:=n_Create(nil,parent);
  result.pos:=tpos-src;
  result.len:=0;
  n_AddAsChild(result,child);
end;

function    TJs_Parser.s_nodec(parent:cardinal;child:p_node):p_node;
begin
  result:=s_node(nil,parent);n_AddAsChild(result,child);
end;


function    TJs_Parser.s_ErrorN(s:string;p:p_node):boolean;
begin
 result:=false;
 if error='' then begin
        error:=s;
        if p<>nil then begin error:=error+' ('+js_token2str(p.kind)+') '+p.text;error:=error+CalcXY(src+p.pos);end;
        end;
  token:=jst_Error;
end;
        
function    TJs_Parser.s_Error(s:string):boolean;
begin
  result:=false;
  if error='' then begin
        error:=s+' ('+js_token2str(token)+':'+lex_token+')';
        error:=error+CalcXY(tpos);
        end;
  token:=jst_Error;
end;

function    TJs_Parser.s_ErrorP(s:string;p:p_node):pointer;
begin
  s_error(s);if p<>nil then n_Free(p);
  result:=nil;
end;




constructor TJs_Parser.Create;
begin
  inherited Create;
  cur:=nil;
  root:=s_nodeT(nil,jst_root,'');
  vars:=s_nodeT(root,jst_vars,'');
  funcs:=s_nodeT(root,jst_functions,'');
end;

destructor  TJs_Parser.Destroy;
begin
  n_Free(root);
  inherited Destroy;
end;



function    TJs_Parser.Parse:boolean;
var p1,p2,p3:p_node;
begin
 error:='';token:=jst_None;
 result:=lex;
 while result and (token<>jst_Eof) do begin
          cur:=root;vars:=root.first;result:=Syntax_Statement;
          end;
 // Hacemos una pequeña limpieza y ponemos las definiciones de Codigo al final de la lista
 if not result and (error='') then error:='UNDEFINED ERROR';
 // Ponemos el codigo al final
 if result then begin
        p1:=n_Create(nil,0);
        p2:=root.first;while p2<>nil do begin p3:=p2.next;if p2.kind=jst_CodeDef then n_AddAsChild(p1,n_RemoveFromParent(p2));p2:=p3;end;
        p2:=p1.first;  while p2<>nil do begin p3:=p2.next;n_AddAsChild(root,n_RemoveFromParent(p2));p2:=p3;end;
        n_Free(p1);
        end;
end;


// http://stackoverflow.com/questions/2846283/what-are-the-rules-for-javascripts-automatic-semicolon-insertion
// http://bclary.com/2004/11/07/#a-7.9.1
// http://inimino.org/~inimino/blog/javascript_semicolons

function    TJs_Parser.s_IsValidSeparator:boolean;
var p1:pchar;
begin
  p1:=spos;dec(p1);
  if p1^='}' then begin result:=true;exit;end;
  if p1^=#13 then begin result:=true;exit;end;
  if p1^=#10 then begin result:=true;exit;end;
  repeat result:=(p1^=#13) or (p1^=';');inc(p1); until result or (p1=tpos);

  if not result then s_error('Expected [;] and have');
end;


function    TJs_Parser.Syntax_Statement:boolean;
var p:pchar;
    c1,c2:p_node;
begin
  p:=pos;c1:=cur;c2:=vars;
  case token of
      jst_PComma:   result:=lex;
      jst_Break:    result:=Syntax_BreakStatement;
      jst_Function: result:=Syntax_FunctionStatement;
      jst_If:       result:=Syntax_IfStatement;
      jst_While:    result:=Syntax_WhileStatement;
      jst_For:      begin result:=lex;if result then if token=jst_Each then result:=Syntax_ForEachStatement else result:=Syntax_ForStatement;end;
      jst_Try:      result:=Syntax_TryStatement;
      jst_Return:   result:=Syntax_ReturnStatement;
      jst_Throw:    result:=Syntax_ThrowStatement;
      jst_Var:      result:=Syntax_VarStatement;
      else          result:=n_AddAsChild(cur,Syntax_ExpressionGroup(nil))<>nil; // Para el resto de casos hacemos una expresion
  end;
  if result then begin
      if (token=jst_PComma) then result:=lex else
      if token<>jst_end     then result:=s_IsValidSeparator;
      end;

  cur:=c1;vars:=c2;
  //if pc and result and (token=pPComma) then result:=lex;
  if result and (p=pos) then result:=s_error('Endless loop');
end;

// Las prioridades de una expresion son las siguientes, de mas a menos
//  FACTOR
//  UNARIOS
//  BINARIOS
//  TERCIARIOS
//  ASIGNACION
//  COMA


function    TJs_Parser.Syntax_ExpressionGroup(p:p_node):p_node;
begin
  if p=nil then result:=Syntax_Expression else result:=p;   // Si teniamos algun previo lo usamos, sino lo leemos

  if (result<>nil) and (token=jst_Comma) then begin
        result:=s_nodenc(jst_ExpGroup,result);
        while (result<>nil) and (token=jst_Comma) do
                  if lex then begin if n_AddAsChild(result,Syntax_Expression)=nil then result:=n_Free(result);end
                         else result:=n_Free(result);

        end;
end;



function    TJs_Parser.Syntax_Expression:p_node;
begin
  result:=Syntax_BinExpression(js_maxBinOp);
end;

function    TJS_Parser.s_expressionOrEmpty:p_node;
begin
  if (token=jst_PComma) or (token=jst_End) then result:=nil else result:=Syntax_Expression;
end;



function    TJS_Parser.Syntax_VarStatement:boolean;
var p1,p2:p_node;
begin
 repeat
    result:=lex_ge(jst_ident);if not result then exit;                                                                                  // Consumimos var|, y esperamos encontrar un identificador
    p1:=n_FindChildHash(vars,hash);if p1=nil then begin p1:=s_Node(vars,jst_Var);p1.value:=n_ValuePP(vars);end;                         // Localizamos o creamos el identificador en la lista de variables
    case lex_gtg(jst_Assign) of
            0: result:=False;
            1: begin
                  p2:=s_nodeT(s_NodeT(cur,jst_assign,'='),jst_ident,p1.text);p2.data:=p1;;
                  p1:=n_AddAsChild(p2.parent,Syntax_Expression);result:=p1<>nil;
               end;   
            end;
    until (result=false) or (token<>jst_Comma);
end;


function    TJS_Parser.Syntax_ReturnStatement:boolean;
begin
  result:=lex;
  // Atencion segun la gramatica despues de return NO podemos tener un linebreak o se auto-insertara un
  // semicolon
  if result then result:=n_AddAsChildP(s_NodeT(cur,jst_Return,'return'),s_expressionOrEmpty)<>nil;
end;


function    TJS_Parser.Syntax_BreakStatement:boolean;
var p1:p_node;
begin
  result:=lex;
  if result then begin
          p1:=cur; // Buscamos el loop
          while (p1<>nil) and (js_tokenIsLoop(p1.kind)=false) do p1:=p1.parent;
          if p1=nil then begin result:=s_Error('Break can only be used in loops');exit;end;
          p1:=n_AddAfter(p1,s_node(nil,jst_BreakHelper));
          p1.data:=s_NodeT(cur,jst_Break,'Break');
          end;
end;



// Parsea un statement              function name(){..some code .}
// Que en realidad quiere decir:    name=CreateFunctionObject( ..some code....)
// Y nosotros lo traducimos como:   #scope_name = { ..some code ..}
//                                  name=CreateFunctionObject( #scope_name )


function    TJS_Parser.Syntax_FunctionStatement:boolean;
var c1,c2:p_node;
begin
  c1:=Syntax_FunctionExpression(true);result:=c1<>nil;if not result then exit;
  c2:=n_FindChildText(vars,s_name(c1.data.text));
  if c2=nil then begin c2:=s_nodeT(vars,jst_Var,s_name(c1.data.text));c2.value:=n_ValuePP(vars);end else n_ClearChildren(c2);
  // Ahora generamos una expresion NEW_FUNCTION_OBJECT(CODE_INSTANCE);
  n_AddAsChild(c2,c1);
end;

function TJS_Parser.s_makelocal(p:p_node):boolean;
var p1:p_node;
begin
  if p.kind=jst_ExpGroup then begin p:=p.first;repeat result:=s_makelocal(p);p:=p.next;until (p=nil) or (result=false);exit;end;
  if p.kind=jst_Assign then p:=p.first;
  if p.kind<>jst_ident then begin result:=s_ErrorN('Expected IDENT',p);exit;end;
  p1:=n_FindChildHash(vars,p.hash);
  if p1=nil then begin p1:=s_NodeT(vars,jst_Var,p.text);p1.value:=n_ValuePP(vars);end;
  p.data:=p1;
  result:=true;
end;


// Entramos con function. Parsea el codigo de la funcion Como un CodeDef y defuelve un nodo jst_NewFunc
function TJS_Parser.Syntax_FunctionExpression(n_required:boolean):p_node;
var name:string;
    c1,c2:p_node;
    f_start:pchar;
begin
  f_start:=tpos;                                                                                      // Donde empieza el nombre de la funcion
  result:=nil;if not lex then exit;                                                                   // Consumimos [function]
  if (token<>jst_Ident) and n_required then begin s_error('Expected funcion name');exit;end;          // Leemos el nombre de la funcion
  if token=jst_Ident then begin name:=lex_token;if not lex then exit;end                              // o generamos uno si es anonima
                     else begin name:='!F'+s_i2s(root.value);n_ValuePP(root);end;
  if not lex_eg(jst_OpenPar) then exit;                                                               // Consumimos (
  c1:=cur;c2:=vars;                                                                                   // Guardamos cur y vars
  name:=c1.text+'#'+name;                                                                             // Este es el nombre cualificado de la funcion. El sufijo se utiliza para distinguir Getters y Setters. Internamente son funciones con distinto nombre
  cur:=n_FindChildText(funcs,name);
  if cur<>nil then n_ClearChildren(cur) else cur:=s_nodeT(funcs,jst_CodeDef,name);                    // Si esta repe eliminamos el contenido de la anterior, sino creamos una nueva
  if n_required=false then cur.number:=1;                                                             // Si se trata de una function expression ponemos el campo number a 1
  vars:=s_nodeT(cur,jst_Vars,'');vars.hash:=0;                                                        // Creamos el nodo con las variables locales. Utilizaremos los campos value, number y hash para contar variables definidas, capturadas y externas
  if token<>jst_ClosePar then if not s_identList(vars,jst_Param) then exit;                           // Leemos los parametros
  if not lex_eeg(jst_ClosePar,jst_Begin) then exit;                                                   // Consumimos )
  while (token<>jst_End) and  Syntax_Statement do begin end;                                          // Hacemos los statements
  if token<>jst_end then exit;                                                                        // Salimos si no tenemos }, es decir hubo error
  cur.pos:=f_start-src;cur.len:=tpos-f_start+1;                                                       // Guardamos donde empieza y donde acaba la funcion
  if not lex then exit;                                                                               // Consumimos el }
  vars:=c2;cur.data:=c1;                                                                              // La funcion se define en C1-VARS
  while (cur.data<>root) and (cur.data.kind<>jst_CodeDef) do cur.data:=cur.data.parent;               // Localizamos el scope donde se define la funcion
  result:=s_nodeT(nil,jst_FuncDef,cur.text);result.data:=cur;                                         // Definimos la funcion (que apunta al codigo)
  cur:=c1;                                                                                            // Y restauramos cur
end;



function    TJS_Parser.Syntax_ForEachStatement:boolean;
begin
  result:=s_Error('Not implemented FOR EACH');
end;

// Si tenemos for (x in a,b) --> x in b
// Si tenemos for ( 'true' in


function    TJS_Parser.Syntax_ForInStatement(v:boolean;p:p_node):boolean;
var c1,c:p_node;
begin
  c1:=cur;
  cur:=s_nodeT(cur,jst_ForIn,'FOR_IN');n_AddAsChild(cur,p);
  if v then begin result:=s_MakeLocal(p.first);if not result then exit;end;
  if token=jst_Comma then begin
        c:=Syntax_ExpressionGroupOn(n_RemoveFromParent(p.first.next),jst_ClosePar);
        result:=c<>nil;if not result then exit;
        n_AddAsChild(p,c);
        end;
  result:=lex_eg(jst_ClosePar);if not result then exit;
  if result then result:=Syntax_Statements(cur);
  cur:=c1;
end;


function    TJs_Parser.Syntax_ExpressionGroupOn(p:p_node;k:longint):p_node;
begin
  // Si tenemos K salimos con p o none
  if token=k then begin if p<>nil then result:=p else result:=s_nodeT(nil,jst_None,'');exit;end;
  // Parseamos la expresion que tengamos
  result:=Syntax_ExpressionGroup(p);
end;



function    TJS_Parser.Syntax_ForStatement:boolean;
var c:p_node;
    cvar:boolean;
begin
  result:=lex_eg(jst_OpenPar);if not result then exit; // Esperamos un (
  cvar:=token=jst_Var;if cvar then result:=lex;if not result then exit;

  if token<>jst_PComma then begin
          c:=Syntax_Expression;result:=c<>nil;if not result then exit;
          if c.kind=jst_In then begin result:=Syntax_ForInStatement(cvar,c);exit;end;
          c:=Syntax_ExpressionGroupOn(c,jst_PComma);result:=c<>nil;if not result then exit;
          end else c:=s_nodeT(nil,jst_None,'');
  // Si tenemos Var
  if cvar then result:=s_MakeLocal(c);if not result then exit;
  // Generamos el nodo FOR y la primera expresion
  cur:=s_nodeT(cur,jst_for,'FOR');n_AddAsChild(cur,c);
  result:=lex_eg(jst_PComma);if not result then exit;
  // Ahora la segunda
  c:=n_AddAsChild(cur,Syntax_ExpressionGroupOn(nil,jst_PComma));
  result:=c<>nil;if not result then exit;
  // Y la tercera
  result:=lex_eg(jst_PComma);if not result then exit;
  c:=n_AddAsChild(cur,Syntax_ExpressionGroupOn(nil,jst_ClosePar));
  result:=c<>nil;if not result then exit;
  result:=lex_eg(jst_ClosePar);if not result then exit;
  if result then result:=Syntax_Statements(cur);
end;


function    TJS_Parser.Syntax_Statements(c:p_node):boolean;
var c1:p_node;
begin
 c1:=cur;cur:=c;
 if token=jst_Begin then begin
          result:=lex;cur:=s_nodeT(cur,jst_Begin,'');
          while (token<>jst_End) and  result do result:=Syntax_Statement;
          if result then result:=lex_eg(jst_end);
          end else result:=Syntax_Statement;
 cur:=c1;
end;

function    TJS_Parser.Syntax_ThrowStatement:boolean;
begin
  cur:=s_node(cur,jst_throw);
  result:=lex;
  if result then result:=n_AddAsChild(cur,Syntax_Expression)<>nil;
end;

function    TJS_Parser.Syntax_WhileStatement:boolean;
begin
  cur:=s_node(cur,jst_while);
  result:=lex_geg(jst_OpenPar);if not result then exit;
  result:=n_AddAsChild(cur,Syntax_Expression)<>nil;if not result then exit;
  result:=lex_eg(jst_ClosePar);if not result then exit;
  result:=Syntax_Statements(cur);
end;

function    TJS_Parser.Syntax_TryStatement:boolean;
var o,p:p_node;
begin
  result:=lex_ge(jst_Begin);if not result then exit;
  p:=s_node(cur,jst_Try);o:=p;
  result:=Syntax_Statements(p);
  if result then begin
         result:=false;
         if token=jst_catch then begin
                result:=lex_gege(jst_OpenPar,jst_ident);
                if result then p:=s_nodeP(s_node(p,jst_catch),token) else s_Error('Expected [Ident] in CATCH block');
                if result then result:=lex_gege(jst_ClosePar,jst_Begin);
                if result then result:=Syntax_Statements(p);
                end;
         if token=jst_finally then begin
                if lex_ge(jst_Begin) then begin
                        n_RemoveFromParent(o);
                        o:=n_AddAsChildP(s_node(nil,jst_finally),o);
                        n_AddAsChild(cur,o);
                        result:=Syntax_Statements(o);
                        end;
                end;
         end;
   if not result then result:=s_Error('Expected [CATCH or FINALLY] after TRY');
end;

function    TJS_Parser.Syntax_IfStatement:boolean;
begin
  cur:=s_nodeT(cur,jst_if,'IF');
  result:=lex_geg(jst_OpenPar);if not result then exit;
  result:=n_AddAsChild(cur,Syntax_Expression)<>nil;if not result then exit;
  result:=lex_eg(jst_ClosePar);if not result then exit;
  result:=Syntax_Statements(cur);if not result then exit;
  if token<>jst_else then exit else result:=lex;
  if result then result:=Syntax_Statements(cur);
end;


function    TJs_Parser.Syntax_TercExpression(op:longint;r:p_node):p_node;
var a:p_node;
begin
 result:=r;
 if lex then begin
      a:=n_AddAsChild(result,Syntax_Expression);
      if (a<>nil) and lex_eg(jst_2Puntos) then n_AddAsChild(result,Syntax_Expression) else result:=n_Free(result);
      end else result:=n_Free(result);
end;

function TJs_Parser.Syntax_BinExpression(op:longint):p_node;
var a:p_node;
begin
  // Hacemos los operadores de mayor prioridad
  if op<js_minBinOp then result:=Syntax_UnaryExpression else result:=Syntax_BinExpression(op-1);
  if not js_tokenIsBinOp(token,op) then exit else result:=s_nodec(token,result);
  // Si se trata del operador terciario ?:
  if token=jst_CondIf then begin result:=Syntax_TercExpression(token,result);exit;end;
  while (result<>nil) and js_tokenIsBinOp(token,op) do begin
         if lex then begin
                 if op<js_minBinOp then a:=Syntax_UnaryExpression else a:=Syntax_BinExpression(op-1);
                 if a<>nil then n_AddAsChild(result,a) else result:=n_Free(result);
                 end else result:=n_Free(result);
        end;
end;

function TJs_Parser.Syntax_UnaryExpression:p_node;
var p:p_node;
    k:cardinal;
begin
  result:=nil;k:=js_token2UnaryOp(token,true);
  while (token<>jst_Error) and js_tokenIsUnaryPreOp(k) do begin result:=s_node(result,k);lex;k:=js_token2UnaryOp(token,true);end;
  if token=jst_Error then begin result:=n_Free(n_top(result));exit;end;
  p:=Syntax_Factor;
  if p=nil then begin result:=n_Free(n_top(result));exit;end;
  // Si tenemos postexpression y el post tiene mas prioridad que los operadores de pre !!
  k:=js_token2UnaryOp(token,false);
  while (token<>jst_Error) and js_tokenIsUnaryPostOp(k) and lex do begin
          case k of
            jst_fcall:  begin p:=s_nodenc(k,p);s_ParamExpressionList(p);if n_kind(result)=jst_New then p.kind:=jst_Begin;end;
            jst_index:  begin if n_kind(result)<>jst_New then begin  p:=s_nodenc(k,p);p.kind:=jst_Index;end
                                                       else begin  p:=n_AddAsChildP(s_node(nil,k),n_AddAsChildP(result,p));result:=nil;end;
                              n_AddAsChild(p,Syntax_Expression);
                              lex_eg(jst_CloseB);
                              end;
            jst_member: begin
                            if n_kind(result)<>jst_New then begin  p:=s_nodenc(k,p);p.kind:=jst_member;end
                                                       else begin  p:=n_AddAsChildP(s_node(nil,k),n_AddAsChildP(result,p));result:=nil;end;
                            s_node(p,jst_ident);
                            lex_eg(jst_ident);
                        end;
            else p:=s_nodec(k,p);
          end;
          k:=js_token2UnaryOp(token,false);
          end;
  result:=n_top(n_AddAsChild(result,p)); // Subimos al nodo inicial
  if token=jst_Error then result:=n_Free(result);
end;


function    TJs_Parser.s_ParamExpressionList(a:p_node):boolean;
begin
 if token=jst_ClosePar then begin result:=lex;exit;end;
 repeat
        result:=n_AddAsChild(a,Syntax_Expression)<>nil;
        if result then begin
                if (token<>jst_ClosePar) and (token<>jst_Comma) then result:=s_Error('Expected [comma]') else result:=lex_tg(jst_Comma)<>0;
                end;
        until (result=false) or (token=jst_ClosePar);
 if result then result:=lex;
end;

function TJs_Parser.Syntax_Factor:p_node;
var next:boolean;
begin
  next:=true;
  case token of
    jst_true:  begin result:=s_nodeT(nil,jst_bool,'true');result.value:=1;end;
    jst_false: begin result:=s_nodeT(nil,jst_bool,'false');result.value:=0;end;
    jst_this,jst_null,jst_pchar,jst_ident,jst_integer,jst_float: begin result:=s_node(nil,token);end;
    jst_OpenPar: if lex then begin result:=Syntax_ExpressionGroup(nil);if token<>jst_ClosePar then result:=n_Free(result);end;
    jst_Function: begin result:=Syntax_FunctionExpression(false);next:=false;end;
    jst_Begin: begin if lex then result:=Syntax_ObjectExpression else result:=nil;next:=false;end;
    jst_OpenB: begin if lex then result:=Syntax_ArrayExpression  else result:=nil;next:=false;end;
    //pDiv:      if lex_RegExpression then result:=s_lex(s_CreateT(nil,pRegExpDef,lex_token));
    else result:=s_errorP('Expected factor',nil);
    end;
  if next and (result<>nil) then if not lex then result:=n_Free(result);
  
end;



function    TJs_Parser.Syntax_GetterSetter(k:cardinal;r:p_node):p_node;
var c1,c2:p_node;
    name:string;
begin
  c1:=Syntax_FunctionExpression(true);if c1=nil then begin result:=n_free(result);exit;end;                                                                     // Parseamos la funcion que sigue
  c1.kind:=k;name:=s_name(c1.text);                                                                                                                             // Este es el nombre de la propiedad
  c2:=n_FindChildText(r,name);                                                                                                                                  // Buscamos la propiedad
  if c2=nil then c2:=n_CreateT(r,jst_GetSet,name)                                                                                                               // Si no existe la creamos
            else if c2.kind<>jst_GetSet then begin result:=s_ErrorP('['+name+'] duplicated in Get/Set',c1);n_Free(result);exit;end;                             // Si existe debe de ser un Getter / Setter
  if k=jst_Getter then name:='@G'+s_p2s(c2)+'_'+name else name:='@S'+s_p2s(c2)+'_'+name;                                                                        // Le damos un nombre unico
  n_Text(c1.data,name);n_Text(c1,name);                                                                                                                         // A la funcion y a la definicion
  if n_FindChildText(c2,name)<>nil then begin result:=s_ErrorP('Duplicated ['+name+'] getter/setter',c1);n_Free(result);exit;end;                               // Si tenemos dos getters o dos setters damos error
  n_AddAsChild(c2,c1);                                                                                                                                          // Si no lo añadimos
  result:=r;                                                                                                                                                    // Y devolvemos result
end;

function    TJs_Parser.Syntax_ObjectField(r:p_node):p_node;
var c:p_node;
begin
  if not (token in [jst_Ident,jst_pchar]) then begin result:=s_errorP('Expected field definition',r);exit;end;
  c:=n_FindChildHash(r,hash);
  if c=nil then c:=s_node(r,jst_Member) else n_ClearChildren(c);      // Permitimos campos duplicados, pero en ese caso el segundo sobreescribe al primero
  
  if not lex_geg(jst_2Puntos)               then result:=n_Free(r) else
  if n_AddAsChild(c,Syntax_Expression)=nil  then result:=n_Free(r) else result:=r;
end;


// Entramos tras {
function    TJs_Parser.Syntax_ObjectExpression:p_node;
begin
  result:=s_NodeT(nil,jst_ObjectDef,'Object');
  if token<>jst_End then begin
      repeat
            if lex_token='get' then result:=Syntax_GetterSetter(jst_Getter,result) else
            if lex_token='set' then result:=Syntax_GetterSetter(jst_Setter,result) else
                                    result:=Syntax_ObjectField(result);
            until (result=nil) or (lex_tg(jst_Comma)<>1);
      if result=nil then exit;
      if token=jst_Error then begin result:=n_Free(result);exit;end;
      end;
  if not lex_eg(jst_End) then result:=n_Free(result);
end;

// Entramos tras [, usamos Syntax Sugar y se evalua como new Array(...)
function    TJs_Parser.Syntax_ArrayExpression:p_node;
//var p:p_node;
begin
  result:=s_node(nil,jst_ArrayDef);
  if token<>jst_CloseB then begin
        repeat
            if n_AddAsChild(result,Syntax_Expression)=nil  then begin result:=n_Free(result);exit;end;
            until lex_tg(jst_Comma)<>1;
        if token=jst_Error then begin result:=n_Free(result);exit;end;
        end;
  if not lex_eg(jst_CloseB) then result:=n_Free(result);
  {
  result:=s_nodeT(nil,jst_New,'NEW');
  p:=s_node(result,jst_Begin);s_nodeT(p,jst_Ident,'Array');
  if token<>jst_CloseB then begin
        repeat
            if n_AddAsChild(p,Syntax_Factor)=nil  then begin result:=n_Free(result);exit;end;
            until lex_tg(jst_Comma)<>1;
        if token=jst_Error then begin result:=n_Free(result);exit;end;
        end;
  if not lex_eg(jst_CloseB) then result:=n_Free(result);
  }
end;




end.

unit js_lexer;

interface

uses fw_utils,fw_vm_types,fw_system_types;

type
    TJs_OnComment=function (src,tpos,pos:pchar;data:pointer):boolean;

    TJs_Lexer=class
      private
        CommentHandlers:pointer; 
        CommentData:pointer;
        function    LexError(s:string):boolean;                         // Genera un error lexico
        function    pos_matches(p:pchar;k:cardinal):boolean;
        procedure   lex_accept(k:cardinal);   inline;
        function    CheckInternal:boolean;
      public
        src:pchar;                                                      // Texto fuente
        spos,tpos,pos:pchar;                                            // Posicion inicial del lexer, de inicio de token y del cursor
        error:string;                                                   // Error que se ha producido
        prevtoken,token,hash:cardinal;                                  // Token anterior,Token Actual, hash del mismo y longitud
        constructor Create;
        destructor  Destroy;  override;
        procedure   registerCommentHandler(f:TJs_OnComment;data:pointer);
        procedure   CheckCommentHandlers;
        function    Lex:boolean;                                        // Avanza el lexer
        function    lex_gtg(que:cardinal):integer;                      // Lex GetTestGet
        function    lex_gt(que:cardinal):integer;                       // Lex GetTest
        function    lex_tg(que:cardinal):integer;                       // Lex TestGet
        function    lex_tge(que1,que2:cardinal):integer;                // Lex TestGetExpect
        function    lex_ge(que:cardinal):boolean;                       // Lex GetExpect
        function    lex_eg(que:cardinal):boolean;                       // Lex ExpectGet
        function    lex_N_eg(que:cardinal):boolean;                      // Lex NONE or EXPECTGET
        function    lex_geg(que:cardinal):boolean;                      // Lex GetExpectGet
        function    lex_gge(que:cardinal):boolean;                      // Lex GetGetExpect
        function    lex_eeg(k1,k2:cardinal):boolean;                    // Lex ExpectExpectGet
        function    lex_ee(k1,k2:cardinal):boolean;                     // Lex ExpectExpect
        function    lex_gee(k1,k2:cardinal):boolean;                    // Lex GetExpectExpect
        function    lex_gege(k1,k2:cardinal):boolean;                   // Lex GetExpectGetExpect
        function    lex_token:string;                                   // El token
        procedure   SetSource(s:pchar);                                    // Crea el lexer
        function    CalcXY(p1:pchar):string;
        function    lex_RegExpression:boolean;
      end;



implementation

uses js_tokens;

constructor TJs_Lexer.Create;
begin
  inherited Create;
  CommentHandlers:=l_create(SizeOf(POINTER),0);
  CommentData:=l_create(SizeOf(POINTER),0);
end;

destructor  TJs_Lexer.Destroy;
begin
  l_free(CommentHandlers);
  l_free(CommentData);
  inherited Destroy;
end;

procedure   TJS_Lexer.registerCommentHandler(f:TJs_OnComment;data:pointer);
begin
  if assigned(f) then begin l_push(CommentHandlers,@f);l_push(CommentData,data);end;
end;

procedure   TJS_Lexer.CheckCommentHandlers;
var i:longint;
    f:TJs_OnComment;
begin
  i:=l_count(CommentHandlers);
  while (i>0) do begin i:=i-1;f:=l_get(CommentHandlers,i);if f(src,tpos,pos,l_get(CommentData,i)) then i:=0;end;
end;

procedure   TJS_Lexer.SetSource(s:pchar);
begin
  inherited Create;
  src:=s;pos:=src;error:='';
  token:=jst_None;prevtoken:=token;
  //OnComment:=LexComment;
end;

procedure TJS_Lexer.lex_accept(k:cardinal);
begin
  inc(pos);token:=k;
end;

function    TJS_Lexer.pos_matches(p:pchar;k:cardinal):boolean;
var k1:pchar;
begin
  k1:=pos;result:=k>0;
  while result and (k>0) do begin result:=p^=k1^;k:=k-1;inc(p);inc(k1);end;
end;

function    TJS_Lexer.Lex:boolean;
begin
 if token=jst_Error then begin result:=false;exit;end;
 prevtoken:=token;result:=true;token:=jst_None;spos:=pos;
 // Nos saltamos los espacios y comentarios
 repeat
      while (pos^<>#0) and (pos^<=' ') do inc(pos);
      if pos_matches('//',2) then begin
                        inc(pos,2);tpos:=pos;
                        while (pos^<>#13) and (pos^<>#0) do inc(pos);
                        CheckCommentHandlers;spos:=pos;
                        end;
      if pos_matches('#$',2) then begin
                        inc(pos,2);tpos:=pos;
                        while (pos^<>'#') and (pos^<>#0) do inc(pos);
                        if pos^='#' then inc(pos);
                        CheckCommentHandlers;spos:=pos;
                        end;


      if pos_matches('/*',2) then begin
                        inc(pos,2);tpos:=pos;
                        repeat inc(pos) until pos_matches('*/',2) or (pos^=#0);
                        if pos^='*' then inc(pos,2) else begin result:=LexError('Unexpected END');exit;end;
                        CheckCommentHandlers;spos:=pos;
                        end;
      until (pos^=#0) or (pos^>' ');

 // Si tenemos el final salimos
 if pos^=#0 then begin token:=jst_Eof;exit;end;
 // Guardamos en que posicion estamos
 tpos:=pos;
 // Hacemos segun pos^
 case pos^ of
      '{': lex_accept(jst_Begin);
      '}': lex_accept(jst_End);
      '(': lex_accept(jst_OpenPar);
      ')': lex_accept(jst_ClosePar);
      ':': lex_accept(jst_2Puntos);
      '[': lex_accept(jst_OpenB);
      ']': lex_accept(jst_CloseB);
      ',': lex_accept(jst_Comma);
      ';': lex_accept(jst_PComma);
      '~': lex_accept(jst_bNot);
      '.': lex_accept(jst_Dot);
      '?': lex_accept(jst_CondIf);
      '&': begin lex_accept(jst_bAnd);  case pos^ of '&': lex_accept(jst_And);          '=': lex_accept(jst_AsgnAnd);end;end;
      '|': begin lex_accept(jst_bOr);   case pos^ of '|': lex_accept(jst_Or);           '=': lex_accept(jst_AsgnOr);end;end;
      '+': begin lex_accept(jst_Add);   case pos^ of '+': lex_accept(jst_PostInc);       '=': lex_accept(jst_AsgnAdd);end;end;
      '-': begin lex_accept(jst_Minus); case pos^ of '-': lex_accept(jst_PostDec);   '=': lex_accept(jst_AsgnMinus);end;end;
      '^': begin lex_accept(jst_bXOr);  if pos^='=' then lex_accept(jst_AsgnXor);end;
      '*': begin lex_accept(jst_Times); if pos^='=' then lex_accept(jst_AsgnTimes);end;
      '/': begin lex_accept(jst_div);   if pos^='=' then lex_accept(jst_AsgnDiv);end;
      '%': begin lex_accept(jst_Mod);   if pos^='=' then lex_accept(jst_AsgnMod);end;
      '<': begin lex_accept(jst_Less);
                 case pos^ of
                   '<': begin lex_accept(jst_shiftl);if pos^='=' then lex_accept(jst_AsgnLeft);end;
                   '=': lex_accept(jst_LessEq);
                 end;
           end;
      '>': begin lex_accept(jst_big);
                 case pos^ of
                   '>': begin lex_accept(jst_shiftr);
                              case pos^ of
                                '=': lex_accept(jst_AsgnRight);
                                '>': begin lex_accept(jst_shiftrz);if pos^='=' then lex_accept(jst_AsgnZRight);end;
                              end;
                        end;
                   '=': lex_accept(jst_BigEq);
                 end;
           end;
      '!': begin lex_accept(jst_Not);   begin if pos^='=' then lex_accept(jst_Diff);  if pos^='=' then lex_accept(jst_strictDif);end;end;
      '=': begin lex_accept(jst_Assign);begin if pos^='=' then lex_accept(jst_Equals);if pos^='=' then lex_accept(jst_strictEq);end;end;
      #39: begin token:=jst_PChar;inc(pos);end;
      #34: begin token:=jst_PChar2;inc(pos);end;
      '#': begin token:=jst_Internal;inc(pos);end;
      '0'..'9':               begin token:=jst_Integer;inc(pos);end;
      'A'..'Z','$','a'..'z','_':  begin token:=jst_Ident;inc(pos);end;
    else begin tpos:=pos;result:=LexError('Illegal character');end;
 end;
 if token=jst_Error then exit;
 case token of
    jst_PChar:  begin while ((pos^>=' ') or (pos^=#9)) and (pos^<>#39) do inc(pos);if pos^<>#39 then result:=LexError('Unterminated string ') else inc(pos);end;
    jst_PChar2: begin token:=jst_PChar;while ((pos^>=' ') or (pos^=#9))  and (pos^<>#34) do inc(pos);if pos^<>#34 then result:=LexError('Unterminated string ') else inc(pos);end;
    jst_Integer: begin while (pos^ in ['0'..'9']) do inc(pos);if (pos^='.') and ((pos+1)^ in ['0'..'9']) then begin token:=jst_Float;inc(pos);while (pos^ in ['0'..'9']) do inc(pos);end;end;
    jst_Dot:     if (pos)^ in ['0'..'9'] then begin token:=jst_float;inc(pos);while (pos^ in ['0'..'9']) do inc(pos);end;
    jst_Ident:   begin while (pos^ in ['A'..'Z','a'..'z','_','0'..'9']) do inc(pos);hash:=s_hash(lex_token);token:=js_keyword(hash);end;
    jst_Internal: result:=CheckInternal;

  end;
end;


function  TJS_Lexer.CheckInternal:boolean;
var p1:pchar;
    s:string;
begin
   p1:=pos;
   while (pos^ in ['A'..'Z','a'..'z','_','0'..'9']) do inc(pos);
   s:=copy(p1,1,pos-p1);
   if s='....INTERNALCOMAND.' then begin token:=0;result:=true;exit;;end;
   token:=jst_Error;
   result:=LexError('Unknown Internal token: '+s);
end;



// Las funciones __Test__ Dan: 0 Si FALLO 1: SI  -1: No
function TJS_Lexer.lex_gtg(que:cardinal):integer;
begin
  result:=0;
  if lex then begin
      if token<>que then result:=-1 else if lex then result:=1;
  end;
end;


function TJS_Lexer.lex_geg(que:cardinal):boolean;
begin
  result:=lex;if not result then exit;
  result:=token=que;
  if result then result:=lex else LexError('Expected geg '+js_token2str(que)+' and have ');
end;

function    TJS_Lexer.lex_gge(que:cardinal):boolean;                      // Lex GetGetExpect
begin
  result:=lex and lex;
  if result then result:=token=que;
end;


// Entramos con / inicial
function    TJS_Lexer.lex_RegExpression:boolean;
begin
  tpos:=pos;repeat inc(pos);until (pos^<' ') or (pos^='/');
  if pos^<>'/' then begin result:=LexError('Unterminated Regular Expression');exit;end;
  result:=true;
  inc(pos);
  if pos^ in ['a'..'z','A'..'Z'] then inc(pos);
end;

function    TJS_Lexer.lex_N_eg(que:cardinal):boolean;                      // Lex NONE or EXPECT then GET
begin
  result:=que=jst_None;
  if not result then result:=lex_eg(que);
end;

function TJS_Lexer.lex_eg(que:cardinal):boolean;
begin
  result:=token=que;
  if result then result:=lex else LexError('Expected eg '+js_token2str(que)+' and have ');
end;

function    TJS_Lexer.lex_gege(k1,k2:cardinal):boolean;                   // Lex GetExpectGetExpect
begin
  result:=lex_geg(k1);
  if result then result:=token=k2;
end;

function    TJS_Lexer.lex_eeg(k1,k2:cardinal):boolean;                      // Lex ExpectExpectGet
begin
  result:=token=k1;
  if result then begin result:=lex_ge(k2);if result then result:=lex;end
            else LexError('Expected eeg '+js_token2str(k1)+' and have ');
end;

function    TJS_Lexer.lex_ee(k1,k2:cardinal):boolean;                      // Lex ExpectExpectGet
begin
  result:=token=k1;
  if result then result:=lex_ge(k2) else LexError('Expected ee '+js_token2str(k1)+' and have ');
end;

function    TJS_Lexer.lex_gee(k1,k2:cardinal):boolean;                    // Lex GetExpectExpect
begin
  result:=lex_ge(k1);if result then result:=lex_ge(k2);
end;

function TJS_Lexer.lex_ge(que:cardinal):boolean;
begin
  result:=lex;if not result then exit;
  result:=token=que;if not result then LexError('Expected ge '+js_token2str(que)+' and have ')
end;



// Las funciones __Test__ Dan: 0 Si FALLO 1: SI  -1: No
function TJS_Lexer.lex_tg(que:cardinal):integer;
begin
  if token<>que then result:=-1 else if lex then result:=1 else result:=0;
end;

function    TJS_Lexer.lex_tge(que1,que2:cardinal):integer;                       // Lex TestGetExpect
begin
  if token<>que1 then result:=-1
                else begin result:=0;if lex then if token=que2 then result:=1;end;
end;

function TJS_Lexer.lex_token: string;
begin
  if token<>jst_PChar then  result:=copy(tpos,1,pos-tpos) else result:=copy(tpos,2,pos-tpos-2);
end;

function TJS_Lexer.lex_gt(que:cardinal):integer;
begin
  if not lex then result:=0 else if token=que then result:=1 else result:=-1;
end;

function    TJS_Lexer.LexError(s:string):boolean;
begin
  result:=false;
  if error='' then begin
        error:='Lex error: '+s+' ('+js_token2str(token)+') '+lex_token;
        error:=error+CalcXY(tpos);
        end;
  token:=jst_Error;
end;


function TJS_Lexer.CalcXY(p1:pchar):string;
var p:pchar;
    posx,posy:cardinal;
begin
  posy:=1;p:=p1;
  while (p^>=' ') and (p<>src) do dec(p);
  posx:=p1-p;if p=src then posx:=posx+1;
  p:=p1;
  while (p<>src) do begin if p^=#10 then posy:=posy+1;dec(p);end;
  result:=' in  position '+s_i2s(posx)+','+s_i2s(posy);
end;

end.


program JSHOST;


uses     fw_utils in './headers/fw_utils.pas'
	   	,fw_system_types in './headers/fw_system_types.pas'
	   	,js_gencode in 'js_gencode.pas'
	   	,js_codeoptimizer in 'js_codeoptimizer.pas'
	   	,js_codeformat in 'js_codeformat.pas'
	   	,js_parser in 'js_parser.pas'
	   	,js_lexer in 'js_lexer.pas'
	   	,js_tokens in 'js_tokens.pas';


var src:pchar;
	MyCompiler:TJS_OptCodeGen; 
	MyCode:pRt_oCode;

procedure DoFinish(errorMsg:string);
begin
	if (errorMsg<>'') then WriteLn('Error: '+errorMsg);
	if (MyCompiler<>nil) then MyCompiler.free;
	if (MyCode<>nil) then m_free(MyCode);
	MyCode:=nil;
	MyCompiler:=nil;
end;


function  print_ast(k:p_node;s:cardinal):string;
var s1,s2:string;
begin
  s1:=s_rpad(s_p2s(k),10);                              // 10 caracteres para la direccion
  s1:=s1+s_rpad(s_Spaces(s)+js_token2str(k.kind),50);   // 50 caracteres para el arbol
  if k.text=nil then s2:='' else s2:=k.text;
  s1:=s1+s_rpad(s2,20);                                  // 15 caracteres para el texto
  if s1[length(s1)-3]<>' ' then begin s1[length(s1)-3]:='.';s1[length(s1)-2]:='.';s1[length(s1)-1]:=' ';s1[length(s1)]:=' ';end;


  s1:=s1+s_rpad(s_i2s(k.value),10);                      // 10 caracteres para value
  s1:=s1+s_rpad(s_p2s(k.data),15);                       // 15 caracteres para data
  s1:=s1+s_rpad(s_i2s(k.hash),15);                       // 15 caracteres para HASH
  s1:=s1+s_rpad(s_i2s(k.number),10);                     // 10 caracteres para number
  if k.kind=jst_CodeDef then result:='------------------------------------------------------------------------------------|'+#13#10 else result:='';

  result:=result+s1+#13#10;
  k:=k.first;s:=s+2;
  while k<>nil do begin result:=result+print_ast(k,s);k:=k.next;end;

end;

function  print_astT(s:cardinal):string;
var s1,s2:string;
begin
  s1:=s_rpad('Address',10);                                    // 10 caracteres para la direccion
  s1:=s1+s_rpad(s_Spaces(s)+'Token',50);                 // 50 caracteres para el arbol
  s2:='';
  s1:=s1+s_rpad(s2,20);                                  // 15 caracteres para el texto
  s1:=s1+s_rpad('Value',10);                      // 10 caracteres para value
  s1:=s1+s_rpad('Data',15);                       // 15 caracteres para data
  s1:=s1+s_rpad('Hash',15);                       // 15 caracteres para HASH
  s1:=s1+s_rpad('Number',10);                     // 10 caracteres para number
  result:=s1+#13#10;
  result:=result+'----------------------------------------------------------------------------------------------------------------------';
  result:=result+#13#10;
end;


// This function is called by js_program_2oCode to fetch memory for the actual code. This memory can be GC or non GC depending on execution context
function f_alloc(sender,host_object:pointer;size:cardinal):pointer;                                                stdcall;
//var c:pRt_Compiler;
begin
  if (host_object=nil) then result:=m_alloc(size);												

  //c:=host_data;
  //if c.GC<>nil then result:=GC_Malloc(c.GC,size,nil) else result:=m_alloc(size);
end;


begin
	MyCode:=nil;
	if (ParamCount<1) then begin WriteLn('Missing input file');exit;end;
    src:=s_LoadFile(pchar(ParamStr(1)));
    if (src=nil) then begin  WriteLn('Could not load file: '+ParamStr(1));exit;end;
    //WriteLn('Compiling  '+ ParamStr(1));
	MyCompiler:=TJS_OptCodeGen.Create; 
	MyCompiler.SetSource(src);

	if not MyCompiler.Parse 	then DoFinish(MyCompiler.error);
	if MyCompiler<>nil then if not MyCompiler.GenCode 	then DoFinish(MyCompiler.error);
	if MyCompiler<>nil then begin WriteLn(print_astT(2));WriteLn(print_ast(MyCompiler.root,2));end;
	if MyCompiler<>nil then if not MyCompiler.Peephole 	then DoFinish(MyCompiler.error);
	WriteLn(' ');
	if MyCompiler<>nil then begin
	  	MyCode:=js_program_2oCode(MyCompiler.programa,pchar(ParamStr(1)),nil,@f_alloc);
	  	if (MyCode<>nil) then WriteLn(js_code2str(MyCode));
        end;




	DoFinish('');
    m_free(src);
end.

unit js_codeformat;

interface

uses fw_utils,fw_vm_types,fw_system_types,fw_vm_constants;

type
    // Esto es un programa en estado intermedio !!
    tJs_Program=record
        strings:      pointer;        // Apunta a un buffer de memoria (p_Buffer) con los strings
        integers:     pointer;        // Apunta a un buffer de memoria (p_buffer) con los enteros 
        strings_hash: pointer;           // Apunta a una lista (pJs_Dlist) con los hashes de los strings que guardamos
        funcs:        pointer;          // Apunta a una lista (pJs_smalllist) con las funciones
        n_regs:       cardinal;        // # de registros usados
        n_vars:       cardinal;        // # de variables globales usadas
        cache_prop:   pointer;          // Cache de instrucciones
        end;
    pJs_Program=^tJs_Program;


    // Esto es una instruccion
    pJs_CodeNode=^tJs_CodeNode;
    tJs_CodeNode=record
          opcode:cardinal;
          k1,k2:cardinal;
          v1,v2:longint;
          next,prev:pJs_CodeNode;
    end;


    tJs_Function=record
        programa:   pJs_Program;
        name:       cardinal;                 // Nombre de la funcion que este bloque implementa
        nvars:      cardinal;                 // Nº de variables que define
        nvars_cl:   cardinal;                 // Nº de variables GC que la funcion define
        nvars_p:    cardinal;                 // Nº de parametros
        nvars_ex:   longint;                  // Posicion al buffer de enteros con variables que se capturan, el primer numero es el tamaño del buffer
        nparams:    cardinal;                 // Nº de parametros que la funcion define
        nlabels:    cardinal;                 // Nº de labels que se emplean
        first,last: pjs_CodeNode;             // Primer y ultimo nodo de codigo
        src_start,src_len:cardinal;           // Posicion del codigo (inicio y final) en el archivo origen
        var_names:  pointer;                   // Lista de nombres
        end;
    pJs_Function=^tJs_Function;

    
function  js_program_create:pJS_Program;
function  js_program_free(p:pJS_Program):pJS_Program;
function  js_program_AddCodeString(p:pJS_Program;name:string):longint;
function  js_program_AddCodeInteger(p:pJS_Program;k:cardinal):longint;
function  js_program_AddFunction(p:pJS_Program;name:string):longint;
function  js_program_AddCode(b:pJs_Function;opcode,k1,k2:cardinal;v1,v2:longint):pJs_CodeNode;
function  js_program_DelCode(b:pJs_Function;d:pJs_CodeNode):pJs_CodeNode;
function  js_program_CacheProps(p:pJs_Program):cardinal;

function  js_function_create:pJs_Function;
function  js_function_Free(f:pjs_function):pointer;
function  js_function_move(dest,src:pJs_Function):pJs_Function;

function  js_function_Length(f:pjs_function):cardinal;





function  js_program_PcharByPos(p:pJS_Program;index:cardinal):pchar;
function  js_program_FunctionByIndex(p:pJS_Program;index:cardinal):pJs_Function;

function  js_program_2str(p:pJs_Program):string;
function  js_program_2oCode(p:pJs_Program;filename:pchar;data:pointer;imported_alloc:TSystem_Alloc):pRt_oCode;

function  js_code2str(p:pRT_oCode):string;

function  js_varname(b:pRt_oBlock;index:longint):string;

implementation

type

      Tjs_StrHash=record
            hash,index:cardinal;
            end;
      pJs_StrHash=^Tjs_StrHash;

      INTERNAL_tRt_String=packed record gc:pointer;length:cardinal;chars:array[0..10] of char;end;                                                  // Un string de los usados por la VM


var K_WRONG:INTERNAL_tRt_String=(gc:nil;length:7;chars:'BAD POS'#0);

procedure js_program_addEmptyString(p:pJs_Program);
begin
  b_writeint(p.strings,0);                                               // La longitud es 0
  b_writepointer(p.strings,nil);                                         // No tenemos GCItem
  b_writebyte(p.strings,0);                                              // Escribimos 1 byte
  b_pad(p.strings,4);                                                    // Padeamos

end;

function js_program_create:pJS_Program;
begin
  result:=m_alloc(SizeOf(tJS_Program));

  result.strings:=b_Create(512);                                             // Esto es un buffer donde guardamos los strings
  js_program_addEmptyString(result);                                         // Registramos en la posicion 0 la cadena vacia
  result.integers:=b_Create(64);                                             // Esto es un buffer donde guardamos los enteros

  result.strings_hash:=l_create(SizeOf(Tjs_StrHash),0);                      // Para cada strings que guardamos en el buffer añadimos su hash y su posicion en el buffer
  result.funcs:=l_create(SizeOf(POINTER),0);                                 // La lista de bloques
  result.cache_prop:=l_create(SizeOf(POINTER),0);
end;




function js_program_free(p:pJS_Program):pJS_Program;
begin
  result:=nil;if p=nil then exit;
  while l_count(p.funcs)>0 do js_function_Free(l_pop(p.funcs));l_free(p.funcs);
  while l_count(p.cache_prop)>0 do m_free(l_pop(p.cache_prop));l_free(p.cache_prop);
  b_Free(p.strings);
  b_Free(p.integers);
  l_free(p.strings_hash);
  m_free(p);
end;


function  js_program_AddCodeInteger(p:pJS_Program;k:cardinal):longint;
begin
    result:=b_writeint(p.integers,k);                        
end;



function js_program_AddCodeString(p:pJS_Program;name:string):longint;
var i,h:cardinal;
    s:pJs_StrHash;
begin
  if name='' then begin result:=0;exit;end;
  h:=s_hash(name);i:=0;
  repeat
      s:=l_pointer(p.strings_hash,i);
      i:=i+1;
      until (s=nil) or (s.hash=h);
  if s=nil then begin
              s:=l_pushalloc(p.strings_hash);                           // Creamos un Hash_Item en la hashlist
              b_writepointer(p.strings,nil);                        // No hay referencia a GC_Item ya que se trata de una cadena de texto constante
              s.index:=b_writeint(p.strings,length(name));          // Escribimos la longitud del texto, en el Hash_Item marcamos este como posicion del opt_ExtPchar
              s.hash:=h;                                            // Guardamos en el Hash_Item el hash del ExtPChar
              b_writestring(p.strings,name);                        // Escribimos el texto y padeamos
              b_pad(p.strings,4);
              end;
  result:=s.index;
end;


function js_function_CacheProps(f:pJs_Function):cardinal;
var c:pJs_CodeNode;
    k:cardinal;
    l:pRt_oCacheProp;
begin
  result:=0;if f.programa=nil then exit;
  c:=f.first;
  while c<>nil do begin
     k:=op_NOP;
     if (c.k2=opt_ConstPchar) or (c.k2=opT_ExtPChar) then begin           // Solo usamos la cache cuando las propiedades son constantes, ya que el tipo de cache que implementamos solo es efectiva dentro de bucles 
                //if c.opcode=op_GetProperty then k:=op_GetCache else
                //if c.opcode=op_SetProperty then k:=op_SetCache;
                end;

     if k<>op_NOP then begin
             l:=m_alloc(SizeOf(tRt_oCacheProp));
             l.k1:=c.k1;l.v1:=c.v1;l.k2:=c.k2;l.v2:=c.v2;
             c.opcode:=k;c.k1:=opT_Integer;c.v1:=l_push(f.programa.cache_prop,l);c.k2:=opT_None;c.v2:=0;
             inc(result);
             end;
      c:=c.next;
      end;
end;

function  js_function_Length(f:pjs_function):cardinal;
var c:pJs_CodeNode;
begin
  result:=0;c:=f.first;while c<>nil do begin inc(result);c:=c.next;end;
end;

function js_program_CacheProps(p:pJs_Program):cardinal;
var i:longint;
begin
 result:=0;
 i:=0;
 while (i<l_count(p.funcs)) do begin result:=result+js_function_CacheProps(js_program_FunctionByIndex(p,i));i:=i+1;end;
end;




function  js_function_create:pJs_Function;
begin
  result:=m_alloc(Sizeof(tJs_Function));
  result.first:=nil;result.last:=nil;result.src_start:=0;result.src_len:=0;result.name:=0;
  result.name:=0;
  result.nvars:=0;
  result.nvars_cl:=0;
  result.nvars_p:=0;
  result.nvars_ex:=-1;
  result.nlabels:=0;
  result.programa:=nil;
  result.var_names:=l_create(SizeOf(POINTER),0);
end;


function  js_function_Free(f:pjs_function):pointer;
var c0,c1:pJs_CodeNode;
begin
   c0:=f.first;while c0<>nil do begin c1:=c0.next;m_free(c0);c0:=c1;end;
   l_free(f.var_names);
   m_free(f);result:=nil;
end;

function js_program_AddFunction(p:pJS_Program;name:string):longint;
var b:pJs_Function;
begin
  b:=js_function_create;
  b.name:=js_program_AddCodeString(p,name);
  b.programa:=p;
  result:=l_push(p.funcs,b);
end;



function js_program_AddCode(b:pJs_Function;opcode,k1,k2:cardinal;v1,v2:longint):pJs_CodeNode;
begin
  result:=m_alloc(SizeOf(tJs_CodeNode));
  result.opcode:=opcode;
  result.k1:=k1;result.k2:=k2;
  result.v1:=v1;result.v2:=v2;
  result.next:=nil;
  if b.last<>nil then b.last.next:=result;result.prev:=b.last;b.last:=result;   // Este es el ultimo bloque
  if b.first=nil then b.first:=result;                                          // Si resulta que tambien es el primero
end;

function  js_program_DelCode(b:pJs_Function;d:pJs_CodeNode):pJs_CodeNode;
begin
 if d.prev<>nil then d.prev.next:=d.next;
 if d.next<>nil then d.next.prev:=d.prev;
 if b.last=d  then b.last:=d.prev;
 if b.first=d then b.first:=d.next;
 result:=d.next;
 m_free(d);
end;


function js_program_FunctionByIndex(p:pJS_Program;index:cardinal):pJs_Function;
begin
  result:=l_get(p.funcs,index);
end;

function  js_program_PcharByPos(p:pJS_Program;index:cardinal):pchar;
begin
  result:=b_readstring(p.strings,index);
  if result=nil then result:=@k_Wrong.Chars;
end;


function js_function_move(dest,src:pJs_Function):pJs_Function;
begin
 // Si b esta vacio salimos
 if src.first=nil then begin result:=nil;exit;end;
 // Si a esta vacio copiamos b en a, sino enganchamos la cadena
 if dest.first=nil then dest.first:=src.first else begin dest.last.next:=src.first;src.first.prev:=dest.last;end;
 // En todo caso ahora se cumple que
 dest.last:=src.last;src.first:=nil;src.last:=nil;
 result:=dest;
end;


function js_f2str(k:cardinal):string;
begin
  {result:=vm_f2str(k);
  if result='' then result:='@FLAG_'+s_i2s(k);
  result:=s_rpad(result,9);
  }
end;

function js_v2str(p:pJs_Program;k:cardinal;v:longint):string;
begin
  {if k=opT_Integer      then result:=s_i2s(v)
  else if k=opT_PChar   then begin
                                s:=js_program_PcharByPos(p,v);
                                if length(s)>10 then s:=s_Left(s,18)+'...';
                                result:='"'+s+'"';
                              end
  else if k=opT_Flag    then result:=js_f2str(v)
  else result:=vm_t2str(k);
  if result='' then result:='@TYPE_';
  if result[length(result)]='_' then result:=result+s_i2s(v);
  result:=s_rpad(result,10);
  }
end;

function js_o2str(k:cardinal):string;
begin
  {result:=vm_o2str(k);
  if result='' then result:='@CODE_'+s_i2s(k);
  result:=s_rpad(result,15);
  }
end;

function js_c2str(p:pJs_Program;n:pJs_CodeNode):string;
var l:pRt_oCacheProp;
begin
  result:=js_o2str(n.opcode);
  if n.k1<>opT_None then begin
          result:=result+js_v2str(p,n.k1,n.v1);
          if n.k2<>opT_None then result:=result+',';
          end;
  if n.k2<>opT_None then result:=result+js_v2str(p,n.k2,n.v2);
  if (n.opcode=op_GetCache) or  (n.opcode=op_SetCache) then begin
          s_alert('%js_c2str 01');
          l:=l_get(p.cache_prop,n.v1);
          s_alert('%js_c2str 02');
          result:=result+s_rpad(' ',15)+s_trim(js_v2str(p,l.k1,l.v1))+'.'+js_v2str(p,l.k2,l.v2);
          end;
end;



function  js_program_2str(p:pJs_Program):string;
var i:longint;
    f:pJs_Function;
    n:pJs_CodeNode;
begin
  result:='';

  i:=0;
  while (i<l_count(p.funcs)) do begin
        f:=js_program_FunctionByIndex(p,i);
        result:=result+'BLOCK_'+s_i2s(i)+'  defines ['+js_program_PcharByPos(p,f.name)+']'+#13#10;
        result:=result+'start: '+s_i2s(f.src_start)+'    lenght: '+s_i2s(f.src_len)+#13#10;
        n:=f.first;
        while (n<>nil) do begin
              result:=result+'   '+s_rpad(s_p2s(n),15)+js_c2str(p,n)+#13#10;
              n:=n.next;
              end;
        i:=i+1;
        end;
end;

//if c.k1=opT_Block then  c.v1:=blocks_offset+n.v1*SizeOf(tRt_oBlock) else

function js_CodeNode2oCode(blocks_offset,str_offset:longint;n:pJs_CodeNode;var c:prt_oOpcode):cardinal;
begin
  result:=0;
  while n<>nil do begin
          c.opcode:=n.opcode;c.k1:=n.k1;c.k2:=n.k2;
          if c.k1=opT_ExtPChar  then  c.v1:=str_offset+n.v1 else
          if c.k1=opT_Block     then  c.v1:=blocks_offset+n.v1*SizeOf(tRt_oBlock) else
          c.v1:=n.v1;
          if c.k2=opT_ExtPChar  then  c.v2:=str_offset+n.v2 else
          if c.k2=opT_Block     then  c.v2:=blocks_offset+n.v2*SizeOf(tRt_oBlock) else
          c.v2:=n.v2;


          n:=n.next;
          inc(c);inc(result);
          end;
end;


function  js_program_2oCode(p:pJs_Program;filename:pchar;data:pointer;imported_alloc:TSystem_Alloc):pRt_oCode;
var j,i,name:longint;
    b:pRt_oBlock;
    c:pRt_oOpcode;
    cp:pRt_oCacheProp;
    block_addr,str_addr,str_len,int_len:longint;
    f:pJs_Function;
    l:pRt_oCacheProp;
    nv:pinteger;
begin
  name:=js_program_AddCodeString(p,filename);                                                                           // Antes de empezar guardamos el nombre del archivo fuente
  j:=0;i:=0;                                                                                                            // J es el tamaño en Bytes que ocupara el programa final
  while (i<l_count(p.funcs)) do begin                                                                                   // Recorremos todas las funciones
          f:=js_program_FunctionByIndex(p,i);                                                                           // Para la funcion i
          j:=j+SizeOf(tRt_oOpcode)*js_function_Length(f);                                                               // Sacamos cuantos opcodes tiene
          j:=j+l_count(f.var_names)*SizeOf(pointer);                                                                    // Mas el espacio con los nombres de las variables
          i:=i+1;
          end;              
  j:=j+l_count(p.funcs)*SizeOf(tRt_oBlock);                                                                             // Mas las definiciones de funciones
  j:=j+b_Count(p.strings);                                                                                              // Mas los strings
  j:=j+b_Count(p.integers);                                                                                             // Mas los enteros
  j:=j+SizeOf(tRt_oCacheProp)*l_count(p.cache_prop);                                                                    // Mas la cache de propiedades
  j:=j+SizeOf(tRt_oCode);                                                                                               // Mas el propio objeto
  result:=imported_alloc(nil,data,j);
  result.total_size:=j;
  result.user_data:=nil;
  result.num_regs:=p.n_regs;                                                                                            // Tenemos estos registros
  result.base_address:=result;
  result.filename:=name;
  // Pasamos los strings
  result.strings:=SizeOf(tRt_oCode);                                                                                    // Los strings comienzan justo despues de la estructura del oCode
  result.num_strings:=1+l_count(p.strings_hash);                                                                       // Cuantos strings tenemos, añadimos 1 por ""
  str_len:=b_Count(p.strings);                                                                                          // Cuanto ocupan los strings
  str_addr:=longint(m_memorycopy(p_Inc(result,result.strings),b_pointer(p.strings,0),str_len));                        // Los copiamos
  // Pasamos los enteros
  int_len:=b_Count(p.integers);                                                                                          // Cuanto ocupan los enteros
  m_memorycopy(p_Inc(result,result.strings+str_len),b_pointer(p.integers,0),int_len);                                   // Copiamos los enteros
  
  // Pasamos el codigo
  result.num_blocks:=l_count(p.funcs);                                                                                  // Cuantos bloques tenemos
  result.blocks:=SizeOf(tRt_oCode)+int_len+str_len;                                                                     // Los bloques comienzan justo despues de los strings y de los enteros
  block_addr:=longint(result)+result.blocks;
  b:=p_Inc(result,result.blocks);                                                                                      // Es decir en esta posicion
  c:=p_Inc(b,result.num_blocks*SizeOf(tRt_oBlock));                                                                    // Detras de los bloques comienzan los opcodes
  for i:=0 to result.num_blocks-1 do begin                                                                              // Copiamos la defincion de las funciones y el codigo de las mismas
        b.oCode:=result;b.code_start:=p_Minus(result,c);                                                               // Aqui comienza el codigo
        f:=js_program_FunctionByIndex(p,i);                                                                             // Localizamos la funcion en el JS
        b.name:=f.name;                                                                                                 // Aqui va el nombre
        b.code_len:=js_CodeNode2oCode(block_addr,str_addr,f.first,c);
        b.src_start:=f.src_start;b.src_len:=f.src_len;
        if i=0 then begin
                b.vars_len:=p.n_vars;
                b.vars_captures:=0;
                b.vars_params:=0;
                b.vars_extern_captured:=0;
                end else begin
                b.vars_len:=f.nvars;
                b.vars_captures:=f.nvars_cl;
                b.vars_params:=f.nvars_p;
                if f.nvars_ex=-1 then b.vars_extern_captured:=0 else b.vars_extern_captured:=result.strings+str_len+f.nvars_ex;
                end;
        inc(b);
        end;
  // Pasamos la cache de acceso a propiedades
  result.cache_prop:=p_Minus(result,c);
  result.num_cache_prop:=l_count(p.cache_prop);
  cp:=pointer(c);
  for i:=0 to result.num_cache_prop-1 do begin
          l:=l_get(p.cache_prop,i);
          cp.k1:=l.k1;
          if cp.k1=opT_ExtPChar then  cp.v1:=str_addr+l.v1 else
          if cp.k1=opT_Block    then  cp.v1:=block_addr+l.v1*SizeOf(tRt_oBlock) else cp.v1:=l.v1;
          cp.k2:=l.k2;
          if cp.k2=opT_ExtPChar then  cp.v2:=str_addr+l.v2 else
          if cp.k2=opT_Block    then  cp.v2:=block_addr+l.v2*SizeOf(tRt_oBlock) else cp.v2:=l.v2;
          inc(cp);
          end;
  // Pasamos los nombres definidos por las variables
  nv:=pointer(cp);                                                                                                      // Los nombres de variables iran aqui
  b:=p_Inc(result,result.blocks);                                                                                      // Volvemos al primer bloque
  for i:=0 to result.num_blocks-1 do begin                                                                              // Copiamos la defincion de las funciones y el codigo de las mismas
          b.var_names:=p_Minus(result,nv);
          f:=js_program_FunctionByIndex(p,i);                                                                             // Localizamos la funcion en el JS
          for j:=0 to l_count(f.var_names)-1 do begin nv^:=l_geti(f.var_names,j)+str_addr;inc(nv);end;
          inc(b);
          end;
  
end;


function fw_varnames(num:cardinal;values:ppointer):string;
var xp:pRt_ExPchar;
begin
  result:='';
  while num>0 do begin
        xp:=values^;
        //result:=result+'('+host_i2s(xp.len)+') ';
        result:=result+pchar(@xp.data)+'|';
        inc(values);dec(num);
        end;
  if result<>'' then result:=s_Left(result,length(result)-1);
end;



function fw_split(var r:string):string;
var i:longint;
begin
  result:='';
  i:=40;while (i<length(r)) and (r[i]<>'|') do i:=i+1;
  if i>=length(r) then exit;
  result:=copy(r,i+1,length(r));
  r:=s_Left(r,i);
end;


function  fw_Pchar2String(p:pchar):string;
begin
  result:='';if p=nil then exit;
  result:=#34;
  while p^>=' ' do begin result:=result+p^;inc(p);end;
  if length(result)>12 then result:=s_Left(result,11)+'...';
  result:=result+'"';
end;

function  js_PcharHint(k:cardinal;p:pchar):string;
begin
  if k=opT_ExtPChar then inc(p,SizeOf(cardinal));
  result:='    '+fw_Pchar2String(p);
end;

function  js_varname(b:pRt_oBlock;index:longint):string;
var s:string;
    k:longint;
begin
   result:='';if (b=nil) or (index<0) then exit;
   s:=fw_varnames(b.vars_len,p_Inc(b.oCode,b.var_names));if s='' then exit;
   s:=s+'|';
   repeat
      k:=pos('|',s);
      //s_alert('k: '+s_i2s(k)+'    index: '+s_i2s(index)+'   s: '+s);
      if index=0 then begin result:=copy(s,1,k-1);index:=-1;end
                 else begin index:=index-1;s:=copy(s,k+1,length(s));end;
      until (index<0) or (s='');
end;

function js_code2str(p:pRT_oCode):string;
var  i,j:longint;
     b:pRt_oBlock;
     c:pRt_oOpcode;
     cp:pRt_oCacheProp;
     r,s:string;
     p1:pchar;
     pp:^longint;
begin
  result:=' '+#13#10+'PROGRAM oCode: '+s_p2s(p)+'           # registers: '+s_i2s(p.num_regs)+#13#10;
  p1:=@(prt_ExPchar(p_Inc(p,p.strings+p.filename)).data);
  result:=result+'Filename: '+p1+#13#10;
  b:=p_Inc(p,p.blocks);                            // Aqui comienzan los bloques
  for i:=0 to p.num_blocks-1 do begin
        result:=result+' '+#13#10;
        result:=result+s_rpad(s_p2s(b),18)+'Block '+s_rpad(s_p2s(b.oCode)+'.'+s_i2s(i),20);
        p1:=@(prt_ExPchar(p_Inc(p,p.strings+b.name)).data);
        result:=result+'Name: '+string(p1)+#13#10;
        result:=result+s_rpad(' ',18)     +s_rpad('src_start : '+s_i2s(b.src_start)           ,22)        +'    src_len: '+s_i2s(b.src_len)+#13#10;
        result:=result+s_rpad(' ',18)     +s_rpad('code_start: '+s_i2s(b.code_start)           ,22)        +'  code_len: '+s_i2s(b.code_len)+#13#10;
        result:=result+s_rpad(' ',18)     +s_rpad('vars_len  : '+s_i2s(b.vars_len)             ,22)        +'params_len: '+s_i2s(b.vars_params)+#13#10;

        s:=fw_varnames(b.vars_len,p_Inc(p,b.var_names));
        while s<>'' do begin
                  r:=fw_split(s);
                  result:=result+s_rpad(' ',18)     +'            '+s        +#13#10;
                  s:=r;
                  end;
        result:=result+s_rpad(' ',18)     +s_rpad('Free Vars  : '+s_i2s(b.vars_captures)+' vars',22);

        if b.vars_extern_captured=0 then result:=result+'Captures : 0 vars'
                                    else begin
                                          pp:=p_Inc(p,b.vars_extern_captured);
                                          result:=result+'Captures : '+s_i2s(pp^)+' vars ';
                                          j:=pp^;repeat inc(pp);if pp^>=0 then result:=result+s_i2s(pp^)+' | ' else result:=result+'*'+s_i2s(-pp^-1)+' | ';dec(j);until j=0;
                                         end;
        result:=result+#13#10;
        c:=p_Inc(p,b.code_start);
        for j:=0 to b.code_len-1 do begin
                s:=s_rpad(s_p2s(c),15)+'   '+s_rpad(vm_o2str(c.opcode),15);
                if c.k1<>opT_None then begin s:=s+vm_tv2str(c.k1,c.v1);if c.k2<>opT_None then s:=s+',';end;
                if c.k2<>opT_None then       s:=s+vm_tv2str(c.k2,c.v2);
                if (c.k2=opT_ExtPChar) or (c.k2=opt_ConstPchar) then s:=s+js_PcharHint(c.k2,pointer(c.v2));
                result:=result+s+#13#10;
                inc(c);
                end;
        inc(b);
      end;

  i:=p.num_strings;p1:=p_Inc(p,p.strings);
  result:=result+s_rpad(' ',18)     +s_rpad('# strings: '+s_i2s(i),22)+'starting at: '+s_i2s(p.strings)+'  ('+s_p2s(p1)+')'+#13#10;
  while i>0 do begin
            inc(p1,SizeOf(pointer));                                                      // Referencia al GC
            result:=result+s_rpad(s_p2s(p1),15)+'   ';
            result:=result+s_rpad('('+s_i2s( pcardinal(p1)^)+')',8);                             // Longitud del string
            inc(p1,SizeOf(cardinal));
            result:=result+fw_Pchar2String(p1)+#13#10;    // Imprimimos
            while p1^<>#0 do inc(p1);                                                     // Avanzamos hasta el #0 de cierre del pchar
            inc(p1);                                                                      // Lo consumimos
            while (cardinal(p1) mod 4<>0) do inc(p1);                                     // Vamos a la siguiente alineacion de 4
            i:=i-1;
            end;

  i:=0;cp:=p_Inc(p,p.cache_prop);
  result:=result+s_rpad(' ',18)     +s_rpad('# cache_prop: '+s_i2s(p.num_cache_prop),22)+'starting at: '+s_i2s(p.cache_prop)+'  ('+s_p2s(cp)+')'+#13#10;

  while (i<p.num_cache_prop) do begin
            result:=result+s_rpad(s_p2s(cp),15)+'  cache_prop_'+s_rpad(s_i2s(i),5)+'   '+vm_tv2str(cp.k1,cp.v1)+' ,  '+vm_tv2str(cp.k2,cp.v2);
            if (cp.k2=opT_ExtPChar) or (cp.k2=opt_ConstPchar) then result:=result+js_PcharHint(cp.k2,pointer(cp.v2));
            result:=result+#13#10;
            inc(cp);inc(i);
            end;
end;


end.

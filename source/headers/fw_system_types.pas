unit fw_system_types;

interface

type
    
    // ----------------------------------------------- BEGIN OBJECT FILE FORMAT
    
    prt_oCode=^tRt_oCode;                                                       // Esto es un programa compilado
    tRt_oCode=record
                total_size      :cardinal;                                      // Tamaño total en bytes de todo el oCode
                user_data       :pointer;                                       // Por defecto esta a nil
                cache_prop      :cardinal;                                      // Offset donde empieza la cache de propiedades: tRt_oCacheProp
                num_cache_prop  :cardinal;                                      // Tanaño de la cache de propiedades
                num_regs        :cardinal;                                      // Nº de registros
                blocks          :cardinal;                                      // Donde empiezan la lista: tRt_OBlock
                num_blocks      :cardinal;                                      // Nº de bloques
                strings         :cardinal;                                      // Donde empiezan los strings: pCharEx
                num_strings     :cardinal;                                      // Nº de strings
                base_address    :pointer;                                       // Direccion base
                filename        :cardinal;                                      // Offset en la tabla de strings con el nombre del archivo
                end;

    pRt_oBlock=^tRt_oBlock;
    tRt_OBlock=record                                                           // Esta es la definicion de un bloque
                oCode:pRt_oCode;                                                // Codigo al que pertenece el bloque
                code_start:cardinal;                                            // Posicion donde comienza el codigo del bloque tRt_Opcode
                code_len:cardinal;                                              // Longitud de todo el bloque
                vars_len:cardinal;                                              // Nº de variables que se declaran
                vars_captures:cardinal;                                         // Nº de variables que el bloque captura
                vars_extern_captured:cardinal;                                  // Nº de variables capturadas a las que accede
                vars_params:cardinal;                                           // Cuantas de las variables locales son parametros
                name:cardinal;                                                  // Offset al nombre del bloque
                var_names:cardinal;                                             // Offset a una lista con los nombres de las variables
                src_start,src_len:cardinal;                                     // Posiciones en el codigo fuente donde empieza y longitud de la funcion que define el bloque
                end;
    pRt_oOpcode=^tRt_oOpcode;
    tRt_oOpcode=record
                opcode:cardinal;
                k1,k2:cardinal;
                v1,v2:longint;
                end;

    tRt_oCacheProp=record
                k1,k2:cardinal;
                v1,v2:longint;
                end;
    pRt_oCacheProp=^tRt_oCacheProp;
    // ----------------------------------------------- END OBJECT FILE FORMAT


    // ----------------------------------------------- BEGIN Compiler support
    // La memoria con el codigo la liberara el host, asi pues el compilador necesita una funcion para asignar la memoria.
    // El compilador necesita una funcion para hacer log de los mensajes, los niveles de Log Son
    const
    LOG_DEBUG   =0;
    LOG_WARNING =1;
    LOG_ERROR   =4;
    // El compilador exporta la funcionSystem_Compile
    type
    TSystem_Alloc   =function (sender,host_data:pointer;size:cardinal):pointer;                                                stdcall;
    TSystem_Log     =function (sender,host_data:pointer;level:cardinal;msg:pchar):longint;                                     stdcall;
    TSystem_Compile =function (src,filename:pchar;host_data:pointer;alloc:TSystem_Alloc;log:TSystem_Log):pRT_oCode;            stdcall;
    // ----------------------------------------------- END System Provided routines !!!!


implementation

end.

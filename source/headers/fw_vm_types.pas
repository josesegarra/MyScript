
unit fw_vm_types;

interface

uses fw_system_types;

const
    FW_OK=1;
    FW_FAIL=0;
    FW_DETOUR=2;
    FW_HALT=3;
    CTX_UNREADY=  nil;
    CTX_READY=    pointer(-1);
    //CTX_RUNNING=


    // Context Core Objects
    FW_Object     = 00;
    FW_Function   = 01;
    FW_Array      = 02;
    FW_Number     = 03;
    FW_Error      = 04;
    FW_String     = 05;
    FW_Boolean    = 06;
    FW_Date       = 07;
    FW_Native     = 08;
    FW_MAX_CORE   = 10;

type
  pRt_Object=^tRt_Object;
  pRt_Context=^tRt_Context;
  pRt_Runner=^tRt_Runner;
  pRt_Var=^tRt_Var;
  tFw_Function    =function (vm,this,params,return:pRt_Var):cardinal;                                                   stdcall;         // Funcion del framework


  // Los objetos Host son de la clase FW_NATIVE y tienen dos propiedades internas k_host_data y k_host_control
   TRt_Host=record
        on_set,on_get,on_destroy:      TFw_Function;                 // Cuando desde un script se intenta modificar una propiedad se llama a estas funciones
        end;
   pRt_Host=^tRt_Host;


  tRt_Var=record
            kind:cardinal;
            case cardinal of
                 0: (v_i: longint);
                    1: (v_pc: pchar);
                    2: (v_v: pRt_Var);
                    3: (v_c: cardinal);
                    4: (v_p: pointer);
                    5: (v_o: pRt_Object);
                    6: (v_ct: pRt_Context);    // Esta es interna
                    7: (v_r:  pRt_Runner);     // Esta es interna
                    8: (v_f:  tFW_Function);   // Esta es interna
                    9: (v_hc: pRt_Host);       // Esta es interna solo para host objects 

            end;

  p_Object=pointer;     // Puntero a un Objeto
  p_pointer=^pointer;   // Puntero a una zona de memoria
  p_cardinal=^cardinal;
  p_integer=^integer;
  p_pchar  =^pchar;

  TOpcode_Func    =function (p:pRt_Runner;o:pRt_oOpcode):pRt_oOpcode;

  

  tRt_ExPchar=record len:cardinal;data:record end;end;
  pRt_ExPchar=^tRt_ExPchar;

  /// ----------------------------- Begin GC
  p_GC=pointer;                           // Puntero a un Garbage Collector
  tRt_GCOnFree=function (value:pointer):cardinal;          stdcall;                         // Esta funcion se llama por el GC cuando eliminamos un Objeto

  /// ----------------------------- End GC Info
  /// ----------------------------- Begin Contexto
  

  tRt_VarList=record
          size,count:cardinal;
          list: pRt_Var;
          end;
  pRt_VarList=^tRt_VarList;

  tRt_CoreObject=record c,p:tRt_Var;end;    // Un Core Object tiene un constructor, prototipo y un nombre
  tRt_CoreObjects=array[0..FW_MAX_CORE] of tRt_CoreObject;


  /// ----------------------------- End Contexto

  /// ----------------------------- Begin VM
  

    // Implementa un FrameDetour. Se utiliza para que el codigo de un Extern se "desvie" a un Object.Function
    tRt_OnFree=function (data:pointer):boolean;
    tRt_FrameDetour=record
           frame_id:     tRt_Var;           // Tipo de frame
           this,func:    tRt_Var;           // El nuevo this y el nuevo cur_func
           params:       pRt_Var;           // Los nuevos parametros
           block:        pRt_oBlock;        // El nuevo bloque
           end;
    pRt_FrameDetour=^tRt_FrameDetour;



  // Implementa una CacheProp, que es una cache de acceso a la propiedad de un objeto
  tRt_PropertyCachedIndex=record
          for_object: pointer;                                          // Este es el objeto. PropertyCahce.the_Object se ha de evaluar a este objeto para que la CachedIndex sea valida
          fast_value: pointer;                                          // Lo asigna el sistema de objetos y es el acceso rapido
          generation: cardinal;                                          // Lo asigna el sistema de objetos, indica para que version del objeto el acceso rapido es valido. Cuando se añade/elimina una propiedad cambia la version del objeto
          end;
  pRt_PropertyCachedIndex=^tRt_PropertyCachedIndex;
  tRt_PropertyCache=record
           the_object  :   tRt_Var;                                      // Esto es lo que pone en el codigo. Pej. Si tenemos SET GVAR_1.NOMBRE:='Hola'
           the_property:   tRt_Var;                                      // Se traduce en: MOVE RESULT,"Hola" | SETCACHE x | y en la posicion x tendremos the_Object.kind=GVAR | the_Object.v_i=1 | the_Property.kind=pChar | the_Property.v_i='Hola'
           position:  array [0..2] of tRt_PropertyCachedIndex;           // Aqui guardaremos el indice para el objeto !!
           end;
  pRt_PropertyCache=^tRt_PropertyCache;




   // Ver pagina 33 del standard  y 99
   TRt_Object=record
            clase: cardinal;            // Es un indice al array de Definicion de Clases. Una Definicion de clases guarda el nombre y el metodo Put de la Clase
            t_Flags:cardinal;           // Flags
            proto: pRt_Object;          // De donde hereda
            size,count:cardinal;        // Nº de propiedades del objeto
            props: pointer;             // Propiedades del objeto
            generation: cardinal;       // Cada vez que hay un cambio en la estructura del objeto: ej se añade/quita una propiedad. Sirve para saber cuando un Property Cache ha caducado
            end;


  // Desaparece la VM. En su lugar tenemos Function_Create que recibe un prt_oCode y genera un p_Object de tipo function
  // ese p_Object ademas tiene asociado un "estado" donde se guarda el stack,las variables globales, la cache, el objeto global, names y next_call
  // a medida que p_Object se va ejecutando se van creando mas Funciones que comparten el estado
  // Cada funcion tiene sus locals, regs, ..

  // Tendremos ademas el metodo Function_Run    que debe recibir un Objeto Function y ejecutarlo
  // Tendremos ademas el metodo Function_Query  que devuelve valores de una funcion



  // El contexto es el estado. En el se guardan el estado comun entre varias ejecuciones
  // Incluye el el codigo, el core, los nombres, property_cache, global 
  // El Core Son propiedades de Global !!!!
  // Las variables Globales son propiedades de Global
  // Una funcion tiene un campo Context !!!

  TRt_Context=record
        self:                         tRt_Var;                      // El propio contexto
        owner:                        tRt_Var;                      // El dueño del contexto. Puede ser None
        code:                         pRt_oCode;                    // Codigo a ejecutar
        runners:                      pRt_Runner;                   // Primer runner en ejecucion del contexto
        global:                       tRt_Var;                      // Objeto global. Es una funcion !!!
        GC:                           p_GC;                         // El GC global
        lock:                         cardinal;                     // Variable de lock
        p_cache:                      pRt_PropertyCache;
        core:                         tRt_CoreObjects;
        helper:                       pointer;                      // Helper incluye funciones comunes: debug, diccionario
        lib_create,lib_destroy,
        lib_set,lib_get,lib_run:      TFw_Function;                 // Funciones importadas de la libreria
        data:                         pointer;
        end;


  // El Runner equivale a una ejecucion concreta de una Contexto
  tRt_Runner=record
        context:                      pRt_Context;
        prev_runner,next_runner:      pRt_Runner;
        self:                         tRt_Var;
        this:                         tRt_Var;
        cur_func:                     tRt_Var;                      // Funcion actual. Solo tiene sentido en tiempo de ejecucion
        stack:                        pointer;                       // Stack de FrameCalls, FrameTry, FrameCatch y FrameFinally
        locals:                       pRt_VarList;                  // Variables locales
        regs:                         pRt_VarList;
        cur_TryFrame:                 pointer;
        next_call:                    prt_Var;
        own_call:                     pRt_Var;
        flags:                        cardinal;
        lock:                         cardinal;
        result:                       tRt_Var;
        error:                        tRt_Var;
        error_address:                pRt_oOpcode;
        helper:                       pointer;                      // Helper incluye funciones comunes: debug, diccionario
        data:                         pointer;
        end;

  /// ----------------------------- End Context



var alerta:string='';

implementation



end.

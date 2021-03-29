SET COLOR TO
CLEAR
CREATE VIEW adduser
DO cleanup
PUBLIC keyword[ 8], loc[ 6],  ;
       locopts, bootdisk, savedir,  ;
       cr, maxpath
PUBLIC defdir, extended, memcheck,  ;
       memval, ems64, memopts,  ;
       savopt
PUBLIC savescheme
DO setup
success = .F.
IF proceed()
     IF config()
          FOR option = 1 TO  ;
              locopts
               loc[ option] =  ;
                  ALLTRIM(loc(option))
          ENDFOR
          IF INLIST(bootdisk, 'A',  ;
             'B')
               DO floppy
          ENDIF
          IF autoexec()
               IF config_fp()
                    DO resource
                    success = .T.
               ENDIF
          ENDIF
          DO final WITH success
     ENDIF
ENDIF
RELEASE MODULE isdiskin
ON ERROR
set color of scheme 1 to &savescheme
DO cleanup
SET VIEW TO adduser
IF SET('TALK') = 'ON'
     SET TALK OFF
     DELETE FILE adduser.vue
     SET TALK ON
ELSE
     DELETE FILE adduser.vue
ENDIF
RETURN
*
PROCEDURE setup
SET TALK OFF
SET ESCAPE OFF
SET MESSAGE TO 24 CENTER
SET STATUS OFF
SET SYSMENU OFF
SET DECIMALS TO 0
SET SAFETY ON
DO WHILE .T.
     wname = WONTOP()
     IF LEN(wname) = 0
          EXIT
     ENDIF
     release window &wname
ENDDO
STORE SCHEME(1) TO myscheme,  ;
      savescheme
spos = AT('/', myscheme) + 1
invis = SUBSTR(myscheme, spos,  ;
        AT(',', myscheme) -  ;
        spos)
invis = invis + '/' + invis
spos = AT(',', myscheme, 4) + 1
len = AT(',', myscheme, 5) - spos
myscheme = STUFF(myscheme, spos,  ;
           len, SCHEME(1, 4))
spos = AT(',', myscheme, 8) + 1
len = AT(',', myscheme, 9) - spos
myscheme = STUFF(myscheme, spos,  ;
           len, SCHEME(1, 1))
spos = AT(',', myscheme, 9) + 1
len = AT(',', myscheme, 10) -  ;
      spos
myscheme = STUFF(myscheme, spos,  ;
           len, invis)
set color of scheme 1 to &myscheme
CLEAR
LOAD isdiskin
extended = '(X)' $ VERSION()
locopts = IIF(extended, 5, 6)
memopts = IIF(extended, 1, 2)
keyword[ 1] = ''
keyword[ 2] = 'RESOURCE'
keyword[ 3] = 'OVERLAY'
keyword[ locopts - 2] =  ;
       'EDITWORK'
keyword[ locopts - 1] =  ;
       'SORTWORK'
keyword[ locopts] = 'PROGWORK'
IF extended
     keyword[ locopts + 1] =  ;
            'DOSMEM'
ELSE
     keyword[ locopts + 1] =  ;
            'EMS'
     keyword[ locopts + 2] =  ;
            'EMS64'
ENDIF
bootdisk = UPPER(LEFT(GETENV('comspec'),  ;
           1))
savedir = SYS(5) + SYS(2003)
cr = CHR(13) + CHR(10) + CHR(13) +  ;
     CHR(10)
maxpath = 130
savopt = -1
defdir = GETENV('FOXPROCFG')
IF LEN(defdir) <> 0
     defdir = FULLPATH(defdir)
     spos = RAT('\', defdir)
     defdir = PADR(LEFT(defdir,  ;
              spos), maxpath)
ELSE
     IF  .NOT. INLIST(bootdisk,  ;
         'A', 'B')
          SET DEFAULT TO (bootdisk)
          STORE PADR(bootdisk +  ;
                ':' + SYS(2003),  ;
                maxpath) TO  ;
                defdir
          SET DEFAULT TO (savedir)
     ELSE
          defdir = SPACE(maxpath)
     ENDIF
ENDIF
RETURN
*
PROCEDURE getdefault
IF RIGHT(defdir, 1) <> '\'
     defdir = defdir + '\'
ENDIF
loc[ 1] = GETENV('FOXPROCFG')
IF LEN(loc(1)) = 0
     loc[ 1] = defdir +  ;
        'CONFIG.FP'
ENDIF
FOR option = 2 TO locopts
     loc[ option] = defdir
ENDFOR
loc[ 2] = loc(2) + 'FOXUSER.DBF'
cfgdef = FOPEN(SYS(2004) +  ;
         'CONFIG.DEF', 0)
IF cfgdef <> -1
     filesize = FSEEK(cfgdef, 0,  ;
                2)
     = FSEEK(cfgdef, 0, 0)
     contents = FREAD(cfgdef,  ;
                filesize)
     = FCLOSE(cfgdef)
     FOR option = 2 TO locopts
          spos = ATC(keyword(option),  ;
                 contents)
          IF spos <> 0
               spos = spos + AT( ;
                      '=',  ;
                      SUBSTR(contents,  ;
                      spos))
               len = AT(CHR(13) +  ;
                     CHR(10),  ;
                     SUBSTR(contents,  ;
                     spos)) - 1
               loc[ option] =  ;
                  ALLTRIM(SUBSTR(contents,  ;
                  spos, IIF(len <>  ;
                  0, len,  ;
                  filesize)))
               IF  .NOT. extended  ;
                   .AND. option =  ;
                   3
                    spos = ATC( ;
                           ' OVERWRITE',  ;
                           loc(3))
                    IF spos <> 0
                         loc[ 3] =  ;
                            LEFT(loc(3),  ;
                            spos)
                    ENDIF
               ENDIF
          ENDIF
     ENDFOR
ENDIF
FOR option = 1 TO locopts
     IF LEN(loc(option)) <  ;
        maxpath
          loc[ option] =  ;
             PADR(loc(option),  ;
             maxpath)
     ENDIF
ENDFOR
RETURN
*
FUNCTION config
DIMENSION say[ 6]
say[ 1] =  ;
   'Ruta para CONFIG.FP        '
say[ 2] =  ;
   'Ruta para FOXUSER          '
say[ 3] =  ;
   'Directorio de los .OVL     '
say[ locopts - 2] =  ;
   'Temp. de trabajo - Editor  '
say[ locopts - 1] =  ;
   'Temp. de trabajo - Ordenar '
say[ locopts] =  ;
   'Temp. de trabajo - Programa'
DEFINE WINDOW explain FROM 11, 4  ;
       TO 22, 75 TITLE  ;
       'Opciones de Configuraci¢n'  ;
       DOUBLE
ok = .F.
@ 1, 0 CLEAR TO 24, 79
@ 1, 62 SAY 'Espacio libre'
memval = IIF(extended, MEMORY(),  ;
         VAL(SYS(23)))
ems64 = .F.
FOR option = 1 TO locopts
     row = option + IIF(option <  ;
           3, 1, 2)
     @ row, 3 SAY say(option) GET  ;
       loc[ option] FUNCTION  ;
       'S30' VALID validloc()  ;
       WHEN whenloc() ERROR  ;
       'Invalid directory or pathname'
ENDFOR
IF extended
     @ 9, 3 GET memcheck DEFAULT  ;
       .F. FUNCTION  ;
       '*C Limit the memory reserved for Non-FoxPro functions '  ;
       VALID validmem() WHEN  ;
       whenmem()
     @ 9, 59 GET memval RANGE 0, ;
       MEMORY() PICTURE  ;
       '999,999 KB' DISABLE
ELSE
     @ 9, 3 GET memcheck DEFAULT  ;
       .F. FUNCTION  ;
       '*C EMS m x. para SISCONOT '  ;
       VALID validmem() WHEN  ;
       whenmem()
     @ 9, 38 GET memval RANGE 0, ;
       VAL(SYS(23)) PICTURE  ;
       '999,999 KB' DISABLE
     @ 9, 50 GET ems64 FUNCTION  ;
       '*C No usar 1os. 64KB de EMS'  ;
       DISABLE
ENDIF
@ 23, 18 GET procbut DEFAULT 0  ;
  SIZE 1, 10 FUNCTION  ;
  '*T \!Aceptar' MESSAGE  ;
  'Su AUTOEXEC.BAT se modificar , y se Crear  un CONFIG.FP'
@ 23, 36 GET filerbut DEFAULT 0  ;
  SIZE 1, 10 FUNCTION  ;
  '*N Archivos' VALID  ;
  validfiler() MESSAGE  ;
  'Examinar los directorios de su disco, presione Escape para Salir'
@ 23, 54 GET abortbut DEFAULT 0  ;
  SIZE 1, 10 FUNCTION  ;
  '*T \?Cancelar' MESSAGE  ;
  'Cancela la configuraci¢n sin hacer ningun cambio.'
READ CYCLE
@ 23, 0
RELEASE WINDOW explain
RETURN IIF(procbut = 1, .T., .F.)
*
FUNCTION whenloc
error = .F.
IF BETWEEN(option, 1, locopts)
     @ option + IIF(option < 3, 1,  ;
       2), 64 SAY SPACE(11)
ENDIF
getvar = VARREAD()
option = VAL(SUBSTR(getvar, 5))
pathname = loc(option)
if len(alltrim(&getvar)) <> 0
     ON ERROR error = .t.
     SET DEFAULT TO LEFT(pathname, 1)
     ON ERROR do abort
     IF  .NOT. error .AND.  .NOT.  ;
         INLIST(SYS(5), 'A:',  ;
         'B:')
          @ option + IIF(option <  ;
            3, 1, 2), 64 SAY  ;
            TRANSFORM(DISKSPACE(),  ;
            '999,999,999')
     ENDIF
     SET DEFAULT TO (savedir)
     IF error
          DO abort
     ENDIF
ENDIF
DO explanatio
RETURN .T.
*
FUNCTION validloc
ok = .T.
getvar = VARREAD()
option = VAL(SUBSTR(getvar, 5))
pathname = UPPER(ALLTRIM(loc(option)))
DO CASE
     CASE option = 1
          basename = 'CONFIG.FP'
     CASE option = 2
          basename = 'FOXUSER.DBF'
     OTHERWISE
          basename = ''
ENDCASE
ok = validparm()
IF ok
     loc[ option] =  ;
        IIF(LEN(pathname) >  ;
        maxpath, pathname,  ;
        PADR(pathname, maxpath))
ENDIF
RETURN ok
*
FUNCTION whenmem
IF BETWEEN(option, 1, locopts)
     @ option + IIF(option < 3, 1,  ;
       2), 64 SAY SPACE(11)
ENDIF
option = 7
DO explanatio
RETURN .T.
*
FUNCTION validmem
FOR i = 1 TO memopts
     IF memcheck
          SHOW OBJECT _CUROBJ + i  ;
               ENABLE
     ELSE
          SHOW OBJECT _CUROBJ + i  ;
               DISABLE
     ENDIF
ENDFOR
RETURN .T.
*
FUNCTION validfiler
FILER
IF WEXIST('Filer')
     DEACTIVATE WINDOW filer
ENDIF
DO WHILE .T.
     wname = WONTOP()
     IF UPPER(wname) <> 'EXPLAIN'
          release window &wname
     ELSE
          EXIT
     ENDIF
ENDDO
RETURN 0
*
PROCEDURE abort
ON ERROR
CLEAR TYPEAHEAD
WAIT WINDOW TIMEOUT 20 MESSAGE()
IF ERROR() = 1001
     ON ERROR do abort
     RETURN
ENDIF
DO cleanup
RETURN
*
PROCEDURE cleanup
RELEASE WINDOW
RELEASE WINDOW
SET SAFETY OFF
CLEAR
RETURN
*
FUNCTION checkdisk
PARAMETER drive, disktype
diskin = .F.
checking = .T.
dletter = UPPER(drive)
DO WHILE  .NOT. diskin .AND.  ;
   checking
     CALL isdiskin WITH dletter
     IF dletter = '0'
          dletter = UPPER(drive)
          WAIT WINDOW  ;
               'Insert your ' +  ;
               IIF(LEN(disktype) =  ;
               0, 'work files',  ;
               disktype) +  ;
               ' disk in Drive ' +  ;
               dletter +  ;
               '.  Press any key to continue.'
          IF LASTKEY() = 27
               checking = .F.
          ENDIF
     ELSE
          diskin = .T.
     ENDIF
ENDDO
RETURN diskin
*
FUNCTION autoexec
IF  .NOT. checkdisk(bootdisk, ;
    'boot')
     RETURN .F.
ENDIF
filename = bootdisk +  ;
           ':\AUTOEXEC.BAT'
IF  .NOT. confirm(filename)
     RETURN .F.
ENDIF
setcmd = 'SET FOXPROCFG=' +  ;
         loc(1) + CHR(13) +  ;
         CHR(10)
autoexec = FOPEN(filename, 2)
IF autoexec == -1
     autoexec = FCREATE(filename,  ;
                0)
     IF autoexec == -1
          RETURN .F.
     ELSE
          = FWRITE(autoexec,  ;
            setcmd)
          = FCLOSE(autoexec)
          RETURN .T.
     ENDIF
ELSE
     filesize = FSEEK(autoexec, 0,  ;
                2)
     = FSEEK(autoexec, 0, 0)
     contents = FREAD(autoexec,  ;
                filesize)
     IF RIGHT(contents, 2) <>  ;
        CHR(13) + CHR(10)
          contents = contents +  ;
                     CHR(13) +  ;
                     CHR(10)
          filesize = filesize + 2
     ENDIF
     spos = ATC('SET FOXPROCFG',  ;
            contents)
     IF spos <> 0
          len = AT(CHR(13) +  ;
                CHR(10),  ;
                SUBSTR(contents,  ;
                spos)) + 1
          IF len = 0
               len = LEN(SUBSTR(contents,  ;
                     spos))
          ENDIF
     ELSE
          len = AT(CHR(26),  ;
                contents)
          IF len <> 0
               contents = LEFT(contents,  ;
                          len -  ;
                          1)
               len = 0
          ENDIF
          spos = RAT(CHR(13) +  ;
                 CHR(10),  ;
                 contents,  ;
                 IIF(RIGHT(contents,  ;
                 2) <> CHR(13) +  ;
                 CHR(10), 1, 2))
          spos = spos + IIF(spos =  ;
                 0, 1, 2)
     ENDIF
     contents = STUFF(contents,  ;
                spos, len,  ;
                setcmd)
     = FSEEK(autoexec, 0, 0)
     = FWRITE(autoexec, contents)
     = FCHSIZE(autoexec,  ;
       LEN(contents))
     = FCLOSE(autoexec)
ENDIF
RETURN .T.
*
FUNCTION config_fp
IF  .NOT. checkdisk(LEFT(loc(1),  ;
    1),loc(1))
     RETURN .F.
ENDIF
IF  .NOT. confirm(loc(1))
     RETURN .F.
ENDIF
config = FOPEN(loc(1), 2)
IF config == -1
     config = FCREATE(loc(1), 0)
     IF config == -1
          RETURN .F.
     ELSE
          cfgdef = FOPEN(SYS(2004) +  ;
                   'CONFIG.DEF',  ;
                   0)
          IF cfgdef <> -1
               filesize = FSEEK(cfgdef,  ;
                          0, 2)
               = FSEEK(cfgdef, 0,  ;
                 0)
               contents = FREAD(cfgdef,  ;
                          filesize)
               = FCLOSE(cfgdef)
               IF RIGHT(contents,  ;
                  2) <> CHR(13) +  ;
                  CHR(10)
                    contents = contents +  ;
                               CHR(13) +  ;
                               CHR(10)
               ENDIF
               FOR option = 2 TO  ;
                   locopts +  ;
                   memopts
                    spos = ATC(keyword(option),  ;
                           contents)
                    IF spos <> 0
                         len = AT(CHR(13) +  ;
                               CHR(10),  ;
                               SUBSTR(contents,  ;
                               spos)) +  ;
                               1
                         contents =  ;
                          STUFF(contents,  ;
                          spos,  ;
                          len,  ;
                          '')
                    ENDIF
               ENDFOR
          ELSE
               contents = ''
          ENDIF
          FOR option = 2 TO  ;
              locopts + memopts
               contents = contents +  ;
                          makeoption(option)
          ENDFOR
          = FWRITE(config,  ;
            contents)
          = FCLOSE(config)
          RETURN .T.
     ENDIF
ELSE
     filesize = FSEEK(config, 0,  ;
                2)
     = FSEEK(config, 0, 0)
     contents = FREAD(config,  ;
                filesize)
     IF RIGHT(contents, 2) <>  ;
        CHR(13) + CHR(10)
          contents = contents +  ;
                     CHR(13) +  ;
                     CHR(10)
          filesize = filesize + 2
     ENDIF
     FOR option = 2 TO locopts +  ;
         memopts
          spos = ATC(keyword(option),  ;
                 contents)
          IF spos <> 0
               len = AT(CHR(13) +  ;
                     CHR(10),  ;
                     SUBSTR(contents,  ;
                     spos)) + 1
               IF len = 0
                    len = LEN(SUBSTR(contents,  ;
                          spos))
               ENDIF
          ELSE
               spos = filesize +  ;
                      1
               len = 0
          ENDIF
          contents = STUFF(contents,  ;
                     spos, len,  ;
                     makeoption(option))
     ENDFOR
     = FSEEK(config, 0, 0)
     = FWRITE(config, contents)
     = FCHSIZE(config,  ;
       LEN(contents))
     = FCLOSE(config)
ENDIF
RETURN .T.
*
FUNCTION makeoption
PARAMETER option
configcmd = ''
IF option <= locopts
     configcmd = keyword(option) +  ;
                 '=' +  ;
                 loc(option) +  ;
                 IIF(option = 3  ;
                 .AND.  .NOT.  ;
                 extended,  ;
                 ' OVERWRITE',  ;
                 '')
ELSE
     IF memcheck .AND. memval =  ;
        IIF(extended, MEMORY(),  ;
        VAL(SYS(23)))
          memcheck = .F.
     ENDIF
     IF memcheck
          IF option = locopts + 1
               chkval = IIF(extended,  ;
                        'ON',  ;
                        'OFF')
               configcmd = keyword(option) +  ;
                           '=' +  ;
                           IIF(memval =  ;
                           0,  ;
                           chkval,  ;
                           ALLTRIM(STR(memval)))
          ELSE
               IF ems64
                    configcmd = keyword(option) +  ;
                                '=OFF'
               ENDIF
          ENDIF
     ENDIF
ENDIF
RETURN configcmd +  ;
       IIF(LEN(configcmd) <> 0,  ;
       CHR(13) + CHR(10), '')
*
PROCEDURE resource
deffile = SYS(2004) +  ;
          'DEFUSER.DBF'
IF  .NOT. FILE(deffile)
     RETURN
ENDIF
IF  .NOT. UPPER(RIGHT(loc(2), 4)) =  ;
    '.DBF'
     loc[ 2] = loc(2) + '.DBF'
ENDIF
IF FILE(loc(2))
     RETURN
ENDIF
USE (deffile)
COPY TO (loc(2))
USE
RETURN
*
FUNCTION validparm
IF  .NOT. checkdisk(LEFT(pathname,  ;
    1),basename)
     RETURN .F.
ENDIF
needdir = IIF(LEN(basename) = 0,  ;
          .T., .F.)
IF  .NOT. needdir
     extension = SUBSTR(basename,  ;
                 AT('.',  ;
                 basename))
ENDIF
STORE .T. TO ok
isdir = isdir(pathname)
IF  .NOT. isdir
     IF needdir
          ok = mkdir(pathname)
     ELSE
          lastpos = RAT('\',  ;
                    pathname)
          IF lastpos > 3
               IF lastpos <>  ;
                  LEN(pathname)
                    isdir = isdir(LEFT(pathname,  ;
                            lastpos))
               ENDIF
               IF  .NOT. isdir
                    ok = mkdir(LEFT(pathname,  ;
                         lastpos -  ;
                         1))
               ENDIF
          ENDIF
          IF ok .AND. lastpos <>  ;
             LEN(pathname)
               isdir = .F.
               IF AT('.',  ;
                  pathname) = 0
                    pathname = pathname +  ;
                               extension
               ENDIF
          ENDIF
     ENDIF
ENDIF
IF isdir
     IF RIGHT(pathname, 1) <> '\'
          pathname = pathname +  ;
                     '\'
     ENDIF
     IF  .NOT. needdir
          pathname = pathname +  ;
                     basename
     ENDIF
ENDIF
RETURN ok
*
FUNCTION mkdir
PARAMETER directory
ans = 2
DEFINE WINDOW alert FROM 13, 14  ;
       TO 18, 66 DOUBLE COLOR  ;
       SCHEME 7
ACTIVATE WINDOW alert
?? CHR(7)
?? directory +  ;
   ' NO existe.  Usted desea crearlo ?'  ;
   FUNCTION 'v47' AT 2
@ 3, 10 GET ans SIZE 1, 12, 8  ;
  FUNCTION '*H \!\<Si;\?\<No'
READ CYCLE OBJECT 2
RELEASE WINDOW alert
IF ans <> 1
     ok = .F.
ELSE
     next = 2
     ok = checkdisk(LEFT(directory,  ;
          1),basename)
     DO WHILE ok
          pos = AT('\', directory,  ;
                next)
          work = IIF(pos <> 0,  ;
                 LEFT(directory,  ;
                 pos - 1),  ;
                 directory)
          isdir = FILE(work +  ;
                  '\NUL')
          IF  .NOT. isdir
               command = 'RUN mkdir ' +  ;
                         work
               &command
               ok = FILE(work +  ;
                    '\NUL')
          ENDIF
          IF pos = LEN(directory)
               pos = 0
          ENDIF
          IF pos = 0 .OR.  .NOT.  ;
             ok
               EXIT
          ELSE
               next = next + 1
          ENDIF
     ENDDO
ENDIF
RETURN ok
*
FUNCTION confirm
PARAMETER fname
PRIVATE ALL
IF PARAMETERS() = 0
     fname = ''
ENDIF
width = 20 + LEN(m.fname)
okbut = 1
IF SET('SAFETY') = 'ON' .AND.   ;
   .NOT. EMPTY(fname)
     DEFINE WINDOW confirm FROM  ;
            SROWS() / 2 - 3,  ;
            SCOLS() / 2 - m.width /  ;
            2 TO SROWS() / 2 + 3,  ;
            SCOLS() / 2 + m.width /  ;
            2 NOGROW FLOAT  ;
            NOCLOSE NOZOOM DOUBLE  ;
            COLOR SCHEME 7
     ACTIVATE WINDOW confirm
     @ 1, 3 SAY 'Actualiza ' +  ;
       m.fname + '?'
     @ 3, width / 2 - 11 GET  ;
       okbut SIZE 1, 10, 2  ;
       FUNCTION  ;
       '*H \!Aceptar;\?Cancelar'  ;
       COLOR SCHEME 7
     READ CYCLE
     RELEASE WINDOW confirm
     RETURN (okbut = 1)
ELSE
     RETURN .T.
ENDIF
*
FUNCTION proceed
lmarg = 4
@ 0, 0 SAY PADC( ;
  'Configurar su estaci¢n SISCONOT ' +  ;
  IIF(extended, ' (X)', '') + ' ',  ;
  80)
? 'La utilidad ADDUSER permite personalizar su estaci¢n, as¡ como' +  ;
  ' configurarla para obtener el m ximo rendimiento en un sistema' +  ;
  ' multiusuario.' FUNCTION 'v72'  ;
  AT lmarg
?
? 'Cada estaci¢n debe tener sus propios archivos CONFIG.FP y FOXUSER. El ' +  ;
  ' archivo CONFIG.FP contiene un conjunto de opciones que determinan el ' +  ;
  ' entorno predeterminado para SISCONOT. El archivo FOXUSER tiene diversos ' +  ;
  ' datos que afectan al entorno de operaci¢n, incluyendo tama¤os/posiciones ' +  ;
  ' de ventanas, conjuntos de colores, entradas de agenda, etc. ADDUSER ' +  ;
  ' asegura que estos archivos est‚n en su unidad local o en un directorio ' +  ;
  ' de red asignado a su estaci¢n de trabajo.'  ;
  FUNCTION 'v72' AT lmarg
?
? 'Hay otros archivos de SISCONOT que deben residir en su disco duro local ' +  ;
  ' (si es posible) para reducir el tr fico entre el servidor de la red y su ' +  ;
  ' estaci¢n de trabajo. ADDUSER le permite especificar la ubicaci¢n de ' +  ;
  ' estos archivos y almacena esta informaci¢n en el archivo CONFIG.FP. '  ;
  FUNCTION 'v72' AT lmarg
? 'A continuaci¢n se muestra el directorio predeterminado que se usar  como ' +  ;
  'origen para configurar su estaci¢n. C mbielo si es preciso, y presione ' +  ;
  'ENTRAR para seguir.' FUNCTION  ;
  'v72' AT lmarg
@ 22, 3 TO 24, 74 DOUBLE
@ 23, 6 GET defdir FUNCTION 's65'
READ
defdir = ALLTRIM(defdir)
DO getdefault
@ 23, 6 CLEAR TO 23, 73
@ 23, 5 GET ans DEFAULT 1 SIZE 1,  ;
  33, 2 FUNCTION  ;
  '*H \!Seguir con la configuraci¢n;\?Cancelar la configuraci¢n'
READ CYCLE
RETURN IIF(ans = 1, .T., .F.)
*
PROCEDURE explanatio
IF savopt = option
     RETURN
ENDIF
ACTIVATE WINDOW explain
CLEAR
DO CASE
     CASE option = 1
          msg = 'En CONFIG.FP hay opciones que indican el entorno predeterminado ' +  ;
                'cuando SISCONOT se inicia. Normalmente es peque¤o (menos de 1024 ' +  ;
                'bytes) pero puede llegar a miles de bytes si contiene muchas ' +  ;
                'opiones. Puesto que solo se accede a ‚l al iniciar SISCONOT, no se ' +  ;
                'deteriora el rendimiento si se ubica en un directorio de la red. ' +  ;
                'Sin embargo, debe tener una ubicaci¢n £nica o un nombre de archivo ' +  ;
                '£nico, asignado por su propia estaci¢n. ' +  ;
                cr + ' ' +  ;
                'AVISO: se agregar  a su archivo AUTOEXEC.BAT una nueva l¡nea SET ' +  ;
                'indic ndole a SISCONOT d¢nde puede encontrar su CONFIG.FP.'
     CASE option = 2
          msg = 'FOXUSER, que es una tabla normal de SISCONOT y que contiene los ' +  ;
                'recursos, se encuentran diversos datos que afectan a su entorno ' +  ;
                '(tama¤os/posiciones de ventanas, conjuntos de colores, entradas en ' +  ;
                'la agenda, etc. Este archivo en principio s¢lo ocupa algunos KB en ' +  ;
                'disco. Sin embargo, si usa SISCONOT de de forma interactiva, se ' +  ;
                'agregan registros. ' +  ;
                cr + ' ' +  ;
                'AVISO: Si no especifica un directorio o un archivo £nico para su ' +  ;
                'estaci¢n, debe asegurarse de que el archivo FOXUSER situado en el ' +  ;
                'directorio de inicio de SISCONOT sea un archivo de s¢lo lectura.'
     CASE option = IIF(extended,  ;
          0, 3)
          msg = 'El overlay de SISCONOT necesita unos 1,3 MB de disco. Este archivo es ' +  ;
                'accedido muy frecuentemente cuando se ejecuta SISCONOT, de forma que ' +  ;
                'conviene que est‚ ubicado en el disco de mayor velocidad posile: o ' +  ;
                'bien en su disco local, o incluso en disco RAM. ' +  ;
                cr + ' ' +  ;
                'Al iniciarse, FoxPro siempre se asegura de que haya una copia ' +  ;
                'v lida de este archivo en el directorio que haya especificado. Por ' +  ;
                'tanto, en general no es necesario que lo copie usted. '
     CASE option = IIF(extended,  ;
          3, 4)
          msg = 'Para cada sesi¢n de editor, SISCONOT crea un archivo temporal de ' +  ;
                'trabajo que se puede hacer tan grande como el archivo original ' +  ;
                'editado. Adem s, es posible que tenga varias sesiones de edici¢n ' +  ;
                'simult neas. ' +  ;
                cr + ' ' +  ;
                'Para conseguir un buen tiempo de respuesta, conviene que ubique ' +  ;
                'estos archivos en su unidad de disco local (si es posible). En ' +  ;
                'cualquier caso, debe asegurarse de tener suficiente espacio en la ' +  ;
                'unidad que especifique para sus sesiones de edici¢n. '
     CASE option = IIF(extended,  ;
          4, 5)
          msg = 'SISCONOT crea archivos temporales de trabajo al hacer SORT o INDEX. ' +  ;
                'Los usados para SORT pueden tener hasta 3 veces el tama¤o de la ' +  ;
                'tabla original. Los usados para INDEX pueden tener hasta 3 veces el ' +  ;
                'tama¤o del archivo ¡ndice resultante. ' +  ;
                cr + ' ' +  ;
                'Disponer de gran velocidad de acceso a estos archivos es ' +  ;
                'fundamental para que SORT e INDEX se hagan r pidamente. Por tanto, ' +  ;
                'estos archivos deber¡an estar en su unidad local si es posible. En ' +  ;
                'cualquier caso, debe asegurarse de tener suficientes espacio en la ' +  ;
                'unidad que especifique para hacer SORT o INDEX de sus tablas.'
     CASE option = IIF(extended,  ;
          5, 6)
          msg = 'SISCONOT usa un archivo especial de trabajo para los programas que ' +  ;
                'uste llama mientras se ejecutan sus aplicaciones. Este archivo no ' +  ;
                'suele ser mayor que 256 KB, y ser¡a conveniente ubicarlo en un ' +  ;
                'disco RAM si dispone de uno. '
     CASE option = 7
          IF extended
               msg = 'By default, FoxPro (X) will use only 60KB of DOS memory ' +  ;
                     '(i.e., memory in the first 640KB) and reserve ' +  ;
                     'the rest for programs you execute with the RUN command and for ' +  ;
                     'your C and assembler language library routines.  If you ' +  ;
                     'are certain that you will not require these features, or if you know ' +  ;
                     'in advance that they will require some specific amount of memory, ' +  ;
                     'you can tell FoxPro how much memory to reserve for this purpose ' +  ;
                     'so it can use whatever is left over.'
          ELSE
               msg = 'De forma predeterminada, SISCONOT aprovecha toda la memoria expandida ' +  ;
                     '(EMS) que encuentre. Si desea reservar algo o toda la memoria EMS ' +  ;
                     'para otros programas, puede limitar la memoria EMS que se pone a ' +  ;
                     'disposici¢n de SISCONOT. ' +  ;
                     cr + ' ' +  ;
                     'AVISO: Algunos emuladores EMS antiguos pueden causar problemas a ' +  ;
                     'SISCONOT. Si tiene problemas con su emulador EMS, puede que se ' +  ;
                     'solucionen haciendo que SISCONOT no utilice los primeros 64 KB de ' +  ;
                     'memoria expandida. '
          ENDIF
ENDCASE
?? msg FUNCTION 'v67' AT 1
savopt = option
RETURN
*
PROCEDURE floppy
DEFINE WINDOW explain FROM 10, 10  ;
       TO 17, 70 DOUBLE
ACTIVATE WINDOW explain
msg = 'Your AUTOEXEC.BAT will be modified to tell FoxPro where to ' +  ;
      'find your CONFIG.FP file.  Please insert your boot disk in Drive ' +  ;
      bootdisk +  ;
      ': and make sure that it is not write-protected.' +  ;
      cr +  ;
      'Press any key to continue...'
?? msg FUNCTION 'v57' AT 1
WAIT ''
RETURN
*
PROCEDURE final
PARAMETER success
DEFINE WINDOW explain FROM 10, 10  ;
       TO 17, 70 DOUBLE
ACTIVATE WINDOW explain
IF success
     msg = 'Su AUTOEXEC.BAT se ha modificado para indicarle a SISCONOT d¢nde ' +  ;
           'encontrar el CONFIG.FP.  Salga de SISCONOT, y reinicie su sistema ' +  ;
           'de nuevo (para que los cambios se activen)' +  ;
           cr +  ;
           'Presione Cualquier tecla para continuar...'
ELSE
     msg = 'Unable to create or modify your AUTOEXEC.BAT and/or your ' +  ;
           'CONFIG.FP file.  Please consult your network administrator.' +  ;
           cr +  ;
           'Press any key to continue...'
ENDIF
?? msg FUNCTION 'v57' AT 1
WAIT ''
RETURN
*
FUNCTION isdir
PARAMETER dir
IF RIGHT(dir, 1) <> '\'
     dir = dir + '\'
ENDIF
dir = dir + IIF(RIGHT(dir, 1) =  ;
      '\', '', '\') + SYS(3)
chan = FCREATE(dir)
IF chan >= 0
     = FCLOSE(chan)
     ERASE (dir)
ENDIF
RETURN IIF(chan >= 0, .T., .F.)
*

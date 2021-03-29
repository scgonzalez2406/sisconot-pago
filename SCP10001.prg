PRIVATE wzfields, wztalk
IF SET('TALK') = 'ON'
     SET TALK OFF
     m.wztalk = 'ON'
ELSE
     m.wztalk = 'OFF'
ENDIF
m.wzfields = SET('FIELDS')
SET FIELDS OFF
IF m.wztalk = 'ON'
     SET TALK ON
ENDIF
PRIVATE m.currarea, m.talkstat,  ;
        m.compstat
IF SET('TALK') = 'ON'
     SET TALK OFF
     m.talkstat = 'ON'
ELSE
     m.talkstat = 'OFF'
ENDIF
m.compstat = SET('COMPATIBLE')
SET COMPATIBLE OFF
m.currarea = SELECT()
SELECT 1
USE concepto AGAIN ALIAS concepto  ;
    ORDER codigo
IF  .NOT. WEXIST('_r7a0vjj0c')
     DEFINE WINDOW _r7a0vjj0c  ;
            FROM 08, 0 TO 19, 79  ;
            GROW FLOAT CLOSE ZOOM  ;
            SHADOW TITLE  ;
            'Actualizaciýn de Conceptos de Control de Pagos'  ;
            MINIMIZE SYSTEM COLOR  ;
            SCHEME 5
ENDIF
MOVE WINDOW '_r7a0vjj0c' CENTER
PRIVATE isediting, isadding,  ;
        wztblarr
PRIVATE wzolddelet, wzolderror,  ;
        wzoldesc
PRIVATE wzalias, tempcurs,  ;
        wzlastrec
PRIVATE isreadonly, find_drop,  ;
        is2table
PUBLIC m.swt
IF EMPTY(ALIAS())
     WAIT WINDOW  ;
          'No table selected. Open table or run query.'
     RETURN
ENDIF
m.wztblarr = ''
m.wzalias = SELECT()
m.isediting = .F.
m.isadding = .F.
m.is2table = .F.
m.wzolddelet = SET('DELETE')
SET DELETED ON
m.tempcurs = SYS(2015)
m.wzlastrec = 1
m.wzolderror = ON('error')
wzoldesc = ON('KEY', 'ESCAPE')
ON KEY LABEL escape
m.find_drop = IIF(_DOS, 0, 2)
m.swt = 0
m.isreadonly = IIF(ISREADONLY(),  ;
               .T., .F.)
IF m.isreadonly
     WAIT WINDOW TIMEOUT 1  ;
          'Table is read-only. No editing allowed.'
ENDIF
SELECT 1
COUNT TO aa
IF aa = 0 .AND.  .NOT.  ;
   m.isreadonly .AND.  ;
   fox_alert( ;
   'El Archivo esta Vacio , Adiciono Registros?' ;
   )
     APPEND BLANK
ENDIF
GOTO TOP
SCATTER MEMO MEMVAR
IF WVISIBLE('_r7a0vjj0c')
     ACTIVATE WINDOW SAME  ;
              _r7a0vjj0c
ELSE
     ACTIVATE WINDOW NOSHOW  ;
              _r7a0vjj0c
ENDIF
@ 00, 00 TO WROWS(), WCOLS()
@ 02, 02 SAY 'Cýdigo : ' COLOR  ;
  SCHEME 5 GET m.codigo SIZE 1, 2,  ;
  2 PICTURE '@!' WHEN isediting
@ 04, 02 SAY 'Descripciýn : '  ;
  COLOR SCHEME 5 GET m.descripcio  ;
  SIZE 1, 43, 2 PICTURE '@!' WHEN  ;
  isediting
@ 05, 0 SAY 'ý' + REPLICATE('ý',  ;
  WCOLS() - 2) + 'ý'
@ 06, 2 GET m.top_btn DEFAULT 1  ;
  SIZE 1, 10, 2 PICTURE  ;
  '@*HN \<Primero   ' VALID  ;
  btn_val('TOP') MESSAGE  ;
  'Ir al Comienzo de Archivo.'  ;
  COLOR SCHEME 5
@ 06, 17 GET m.prev_btn DEFAULT 1  ;
  SIZE 1, 10, 2 PICTURE  ;
  '@*HN \<Anterior  ' VALID  ;
  btn_val('PREV') MESSAGE  ;
  'Ir al registro anterior' COLOR  ;
  SCHEME 5
@ 06, 32 GET m.next_btn DEFAULT 1  ;
  SIZE 1, 10, 2 PICTURE  ;
  '@*HN \<Siguiente ' VALID  ;
  btn_val('NEXT') MESSAGE  ;
  'Ir al registro siguiente'  ;
  COLOR SCHEME 5
@ 06, 47 GET m.end_btn DEFAULT 1  ;
  SIZE 1, 10, 2 PICTURE  ;
  '@*HN \<Final     ' VALID  ;
  btn_val('END') MESSAGE  ;
  'Ir al final del Archivo' COLOR  ;
  SCHEME 5
@ 06, 62 GET m.loc_btn DEFAULT 1  ;
  SIZE 1, 10, 2 PICTURE  ;
  '@*HN \<Buscar    ' VALID  ;
  btn_val('LOCATE') MESSAGE  ;
  'Buscar Registros' COLOR SCHEME  ;
  5
@ 07, 02 GET m.add_btn DEFAULT 1  ;
  SIZE 1, 10, 2 PICTURE  ;
  '@*HN \<Nuevo     ' VALID  ;
  btn_val('ADD') MESSAGE  ;
  'Ingresar un nuevo registro'  ;
  COLOR SCHEME 5
@ 07, 17 GET m.edit_btn DEFAULT 1  ;
  SIZE 1, 10, 2 PICTURE  ;
  '@*HN \<Modificar ' VALID  ;
  btn_val('EDIT') MESSAGE  ;
  'Modifica el Presente Registro.'  ;
  COLOR SCHEME 5
@ 07, 32 GET m.del_btn DEFAULT 1  ;
  SIZE 1, 10, 2 PICTURE  ;
  '@*HN \<Eliminar  ' VALID  ;
  btn_val('DELETE') MESSAGE  ;
  'Elimina el Registro Actual'  ;
  COLOR SCHEME 5
@ 07, 47 GET m.prnt_btn DEFAULT 1  ;
  SIZE 1, 10, 2 PICTURE  ;
  '@*HN \<Imprimir  ' VALID  ;
  btn_val('PRINT') MESSAGE  ;
  'Registro de Impresiýn.' COLOR  ;
  SCHEME 5
@ 07, 62 GET m.exit_btn DEFAULT 1  ;
  SIZE 1, 10, 2 PICTURE  ;
  '@*HN Sa\<lir     ' VALID  ;
  btn_val('EXIT') MESSAGE  ;
  'Finaliza la Actualizaciýn de Concepto de Pagos y Descuentos'  ;
  COLOR SCHEME 5
IF  .NOT. WVISIBLE('_r7a0vjj0c')
     ACTIVATE WINDOW _r7a0vjj0c
ENDIF
READ CYCLE ACTIVATE readact()  ;
     DEACTIVATE readdeac()  ;
     NOLOCK
RELEASE WINDOW _r7a0vjj0c
CLOSE DATABASES
SELECT (m.currarea)
IF m.talkstat = 'ON'
     SET TALK ON
ENDIF
IF m.compstat = 'ON'
     SET COMPATIBLE ON
ENDIF
set deleted &wzolddelete
set fields &wzfields
on error &wzolderror
on key label escape &wzoldesc
DO CASE
     CASE _DOS .AND.  ;
          SET('DISPLAY') =  ;
          'VGA25'
          @ 24, 0 CLEAR TO 24, 79
     CASE _DOS .AND.  ;
          SET('DISPLAY') =  ;
          'VGA50'
          @ 49, 0 CLEAR TO 49, 79
     CASE _DOS
          @ 24, 0 CLEAR TO 24, 79
ENDCASE
*
FUNCTION readdeac
IF isediting
     ACTIVATE WINDOW '_r7a0vjj0c'
     WAIT WINDOW NOWAIT  ;
          'Presione Cualquier tecla para confirmar los cambios.'
ENDIF
IF  .NOT. WVISIBLE(WOUTPUT())
     CLEAR READ
     RETURN .T.
ENDIF
RETURN .F.
*
PROCEDURE readact
IF  .NOT. isediting
     SELECT (m.wzalias)
     SHOW GETS
ENDIF
DO refresh
RETURN
*
PROCEDURE wizerrorha
WAIT WINDOW MESSAGE()
RETURN
*
PROCEDURE printrec
PRIVATE solderror, wizfname,  ;
        saverec, savearea,  ;
        tmpcurs, tmpstr
PRIVATE prnt_btn, p_recs,  ;
        p_output, pr_out,  ;
        pr_record
STORE 1 TO p_recs, p_output
STORE 0 TO prnt_btn
STORE RECNO() TO saverec
m.solderror = ON('error')
DO pdialog
IF m.prnt_btn = 2
     RETURN
ENDIF
IF  .NOT. FILE(ALIAS() + '.FRX')
     m.wizfname = SYS(2004) +  ;
                  'WIZARDS\' +  ;
                  'WIZARD.APP'
     IF  .NOT. FILE(m.wizfname)
          ON ERROR *
          m.wizfname = LOCFILE('WIZARD.APP',  ;
                       'APP',  ;
                       'Locate WIZARD.APP:' ;
                       )
          on error &solderror
          IF  .NOT. 'WIZARD.APP' $  ;
              UPPER(m.wizfname)
               WAIT WINDOW  ;
                    'Wizard application is not available.'
               RETURN
          ENDIF
     ENDIF
     WAIT WINDOW NOWAIT  ;
          'Creating report with Report Wizard.'
     m.savearea = SELECT()
     m.tmpcurs = '_' +  ;
                 LEFT(SYS(3), 7)
     CREATE CURSOR (m.tmpcurs)  ;
            (comment M)
     m.tmpstr = '* LAYOUT = COLUMNAR' +  ;
                CHR(13) +  ;
                CHR(10)
     INSERT INTO (m.tmpcurs)  ;
            VALUE (m.tmpstr)
     SELECT (m.savearea)
     DO (m.wizfname) WITH '',  ;
        'WZ_QREPO',  ;
        'NOSCRN/CREATE', ALIAS(),  ;
        m.tmpcurs
     USE IN (m.tmpcurs)
     WAIT CLEAR
     IF  .NOT. FILE(ALIAS() +  ;
         '.FRX')
          WAIT WINDOW  ;
               'Could not create report.'
          RETURN
     ENDIF
ENDIF
m.pr_out = IIF(m.p_output = 1,  ;
           'TO PRINT NOCONSOLE',  ;
           'PREVIEW')
m.pr_record = IIF(m.p_recs = 1,  ;
              'NEXT 1', 'ALL')
report form (alias()) &pr_out &pr_record
GOTO m.saverec
RETURN
*
FUNCTION btn_val
PARAMETER m.btnname
m.swt = 0
DO CASE
     CASE m.btnname = 'TOP'
          GOTO TOP
          WAIT WINDOW NOWAIT  ;
               'Inicio del Archivo.'
     CASE m.btnname = 'PREV'
          IF  .NOT. BOF()
               SKIP -1
          ENDIF
          IF BOF()
               WAIT WINDOW NOWAIT  ;
                    'Inicio del Archivo.'
               GOTO TOP
          ENDIF
     CASE m.btnname = 'NEXT'
          IF  .NOT. EOF()
               SKIP 1
          ENDIF
          IF EOF()
               WAIT WINDOW NOWAIT  ;
                    'Final del Archivo.'
               GOTO BOTTOM
          ENDIF
     CASE m.btnname = 'END'
          GOTO BOTTOM
          WAIT WINDOW NOWAIT  ;
               'Final del Archivo.'
     CASE m.btnname = 'LOCATE'
          DO loc_dlog
     CASE m.btnname = 'ADD' .AND.   ;
          .NOT. isediting
          isediting = .T.
          isadding = .T.
          = edithand('ADD')
          _CUROBJ = 1
          DO refresh
          SHOW GETS
          SHOW GET m.codigo  ;
               ENABLE
          RETURN
     CASE m.btnname = 'EDIT'  ;
          .AND.  .NOT. isediting
          IF EOF() .OR. BOF()
               WAIT WINDOW NOWAIT  ;
                    'Final del Archivo.'
               RETURN
          ENDIF
          IF RLOCK()
               isediting = .T.
               _CUROBJ = OBJNUM(m.codigo)
               DO refresh
               SHOW GET m.codigo  ;
                    DISABLE
               RETURN
          ELSE
               WAIT WINDOW  ;
                    'Sorry, could not lock record -- try again later.'
          ENDIF
     CASE m.btnname = 'EDIT'  ;
          .AND. isediting
          IF isadding
               = edithand('SAVE')
               IF m.swt = 1
                    m.swt = 0
                    RETURN .F.
               ENDIF
          ELSE
               IF EMPTY(m.descripcio)
                    = msgerro( ;
                      'El Nombre de la Asignatura esta en Blanco' ;
                      )
                    _CUROBJ = OBJNUM(m.descripcio)
                    RETURN .F.
               ENDIF
               GATHER MEMVAR MEMO
          ENDIF
          UNLOCK
          isediting = .F.
          isadding = .F.
          DO refresh
     CASE m.btnname = 'DELETE'  ;
          .AND. isediting
          IF isadding
               = edithand('CANCEL')
          ENDIF
          isediting = .F.
          isadding = .F.
          UNLOCK
          WAIT WINDOW NOWAIT  ;
               'Modificaciýn Cancelada.'
          DO refresh
     CASE m.btnname = 'DELETE'
          IF EOF() .OR. BOF()
               WAIT WINDOW NOWAIT  ;
                    'Final del Archivo.'
               RETURN
          ENDIF
          IF fox_alert( ;
             'Elimino el Registro Seleccionado?' ;
             )
               DELETE
               IF  .NOT. EOF()  ;
                   .AND.  ;
                   DELETED()
                    SKIP 1
               ENDIF
               IF EOF()
                    WAIT WINDOW  ;
                         NOWAIT  ;
                         'Final del Archivo.'
                    GOTO BOTTOM
               ENDIF
          ENDIF
     CASE m.btnname = 'PRINT'
          DO printrec
          RETURN
     CASE m.btnname = 'EXIT'
          m.bailout = .T.
          CLEAR READ
          RETURN
ENDCASE
SCATTER MEMO MEMVAR
SHOW GETS
SHOW GET m.codigo
RETURN
*
PROCEDURE refresh
DO CASE
     CASE m.isreadonly .AND.  ;
          RECCOUNT() = 0
          SHOW GETS DISABLE
          SHOW GET exit_btn  ;
               ENABLE
     CASE m.isreadonly
          SHOW GET add_btn  ;
               DISABLE
          SHOW GET del_btn  ;
               DISABLE
          SHOW GET edit_btn  ;
               DISABLE
     CASE (RECCOUNT() = 0 .OR.  ;
          EOF()) .AND.  .NOT.  ;
          m.isediting
          SHOW GETS DISABLE
          SHOW GET add_btn ENABLE
          SHOW GET exit_btn  ;
               ENABLE
     CASE m.isediting
          SHOW GET find_drop  ;
               DISABLE
          SHOW GET top_btn  ;
               DISABLE
          SHOW GET prev_btn  ;
               DISABLE
          SHOW GET loc_btn  ;
               DISABLE
          SHOW GET next_btn  ;
               DISABLE
          SHOW GET end_btn  ;
               DISABLE
          SHOW GET add_btn  ;
               DISABLE
          SHOW GET prnt_btn  ;
               DISABLE
          SHOW GET exit_btn  ;
               DISABLE
          SHOW GET edit_btn, 1  ;
               PROMPT '\<Salvar'
          SHOW GET del_btn, 1  ;
               PROMPT  ;
               '\<Cancelar'
          RETURN
     OTHERWISE
          SHOW GET edit_btn, 1  ;
               PROMPT  ;
               '\<Modificar'
          SHOW GET del_btn, 1  ;
               PROMPT  ;
               '\<Eliminar'
          SHOW GETS ENABLE
ENDCASE
IF m.is2table
     SHOW GET add_btn DISABLE
ENDIF
ON KEY LABEL escape
RETURN
*
FUNCTION edithand
PARAMETER m.paction
DO CASE
     CASE m.paction = 'ADD'
          SCATTER BLANK MEMO  ;
                  MEMVAR
          m.swt = 1
          SHOW GET m.codigo  ;
               ENABLE
     CASE m.paction = 'SAVE'
          m.swt = 1
          SELECT 1
          SET ORDER TO codigo
          SEEK m.codigo
          IF FOUND()
               p = msgerro( ;
                   'Este Concepto de Control de Pago ya fue registrado, verifique' ;
                   )
               _CUROBJ = OBJNUM(m.codigo)
               RETURN .F.
          ENDIF
          m.swt = 0
          DO WHILE .T.
               IF FLOCK()
                    INSERT INTO  ;
                           (ALIAS())  ;
                           FROM  ;
                           MEMVAR
                    UNLOCK
                    EXIT
               ENDIF
          ENDDO
     CASE m.paction = 'CANCEL'
ENDCASE
RETURN
*
FUNCTION fox_alert
PARAMETER wzalrtmess
PRIVATE alrtbtn
m.alrtbtn = 2
DEFINE WINDOW _qec1ij2t7 AT 0, 0  ;
       SIZE 4, 50 FONT  ;
       'MS Sans Serif', 10 STYLE  ;
       'B' FLOAT NOCLOSE SHADOW  ;
       TITLE WTITLE() NOMINIMIZE  ;
       COLOR SCHEME 5
MOVE WINDOW _qec1ij2t7 CENTER
ACTIVATE WINDOW NOSHOW _qec1ij2t7
@ 1, (50 - TXTWIDTH(wzalrtmess)) /  ;
  2 SAY wzalrtmess FONT  ;
  'MS Sans Serif', 10 STYLE 'B'
@ 2, 0 TO 2, WCOLS() DOUBLE
@ 3, 18 GET m.alrtbtn FONT  ;
  'MS Sans Serif', 8 STYLE 'B'  ;
  SIZE 1.769 , 8.667 , 1.333   ;
  PICTURE  ;
  '@*HT \<OK;\?\!\<Cancel'
ACTIVATE WINDOW _qec1ij2t7
READ CYCLE MODAL
RELEASE WINDOW _qec1ij2t7
RETURN m.alrtbtn = 1
*
PROCEDURE pdialog
DEFINE WINDOW _qjn12zbvh AT    ;
       0.000 ,   0.000  SIZE    ;
       7.231 ,  54.800  FONT  ;
       'MS Sans Serif', 8 FLOAT  ;
       NOCLOSE SHADOW TITLE  ;
       'Registro de Impresiýn'  ;
       MINIMIZE SYSTEM COLOR  ;
       SCHEME 5
MOVE WINDOW _qjn12zbvh CENTER
ACTIVATE WINDOW NOSHOW _qjn12zbvh
@   1.846 ,  33.600  SAY  ;
    'Salida:' FONT  ;
    'MS Sans Serif', 8 STYLE 'BT'  ;
    COLOR SCHEME 5
@   1.846 ,   4.800  SAY  ;
    'Imprimir:' FONT  ;
    'MS Sans Serif', 8 STYLE 'BT'  ;
    COLOR SCHEME 5
@   2.692 ,   7.200  GET m.p_recs  ;
    DEFAULT 1 FONT  ;
    'MS Sans Serif', 8 STYLE 'BT'  ;
    SIZE 1.308 , 18.500 , 0.308   ;
    PICTURE  ;
    '@*RVN \<El Presente Registro;\<Todos los Registros'  ;
    COLOR SCHEME 5
@   2.692 ,  33.000  GET  ;
    m.p_output DEFAULT 1 FONT  ;
    'MS Sans Serif', 8 STYLE 'BT'  ;
    SIZE 1.308 , 12.000 , 0.308   ;
    PICTURE  ;
    '@*RVN \<Impresora;\<Pantalla'  ;
    COLOR SCHEME 5
@ 4, 0 TO 4, WCOLS() DOUBLE
@   5.154 ,  16.600  GET  ;
    m.prnt_btn DEFAULT 1 FONT  ;
    'MS Sans Serif', 8 STYLE 'B'  ;
    SIZE 1.769 , 8.667 , 1.667   ;
    PICTURE  ;
    '@*HT \<Imprimir;Ca\<ncelar'
ACTIVATE WINDOW _qjn12zbvh
READ CYCLE MODAL
RELEASE WINDOW _qjn12zbvh
RETURN
*
PROCEDURE loc_dlog
PRIVATE gfields, i
DEFINE WINDOW wzlocate FROM 1, 1  ;
       TO 20, 40 FONT  ;
       'MS Sans Serif', 8 GROW  ;
       FLOAT CLOSE ZOOM SHADOW  ;
       SYSTEM COLOR W+/BG,W+/B,GR+/ ;
       W,GR+/W,GR+/N,GR+/N,W+/B,W+/ ;
       B 
MOVE WINDOW wzlocate CENTER
m.gfields = SET('FIELDS', 2)
IF  .NOT. EMPTY(RELATION(1))
     SET FIELDS ON
     IF m.gfields <> 'GLOBAL'
          SET FIELDS GLOBAL
     ENDIF
     IF EMPTY(FLDLIST())
          m.i = 1
          DO WHILE  .NOT.  ;
             EMPTY(OBJVAR(m.i))
               IF ATC('M.',  ;
                  OBJVAR(m.i)) =  ;
                  0
                    SET FIELDS TO (OBJVAR(m.i))
               ENDIF
               m.i = m.i + 1
          ENDDO
     ENDIF
ENDIF
ON KEY LABEL enter keyboard chr(23)
BROWSE NOMENU NOEDIT NODELETE  ;
       WINDOW wzlocate TITLE  ;
       'Buscar Registros'
ON KEY LABEL enter
set fields &gfields
SET FIELDS OFF
RELEASE WINDOW wzlocate
RETURN
*

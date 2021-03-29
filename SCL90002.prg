CLOSE DATABASES
SET EXCLUSIVE OFF
SET TALK OFF
DEFINE WINDOW indices FROM 11, 10  ;
       TO 19, 60 GROW FLOAT  ;
       SHADOW NONE COLOR SCHEME  ;
       7
ACTIVATE WINDOW indices
MOVE WINDOW indices CENTER
@ 00, 00 TO 08, 51
@ 02, 00 SAY 'Ã' + REPLICATE('Ä',  ;
  49) + '´'
@ 01, 12 SAY  ;
  'Busqueda de Archivos Da¤ados'  ;
  COLOR W+/R 
@ 04, 04 SAY 'ARCHIVO : '
@ 07, 15 SAY '...' COLOR W+/R* 
@ 07, COL() + 2 SAY 'Procesando'  ;
  COLOR W+/R 
@ 07, COL() + 2 SAY '...' COLOR W+/ ;
  R* 
ON ERROR do errores with message(),error(),program(),lineno()
PUBLIC corecto, yarchivo
corecto = .T.
corecto1 = 0
SELECT 1
USE EXCLUSIVE control
DO WHILE .T.
     IF FLOCK()
          ZAP
          UNLOCK
          EXIT
     ENDIF
ENDDO
SELECT 1
yarchivo = 'PLANCOBR.DBF   '
@ 4, 15 SAY 'PLANCOBR.DBF   '
USE EXCLUSIVE plancobr
IF  .NOT. corecto
     USE control
     DO WHILE .T.
          IF FLOCK()
               APPEND BLANK
               REPLACE archivo  ;
                       WITH  ;
                       'PLANCOBR.DBF'
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     corecto1 = 1
ENDIF
SELECT 1
yarchivo = 'DETACOBR.DBF   '
@ 4, 15 SAY 'DETACOBR.DBF   '
USE EXCLUSIVE detacobr
IF  .NOT. corecto
     USE control
     DO WHILE .T.
          IF FLOCK()
               APPEND BLANK
               REPLACE archivo  ;
                       WITH  ;
                       'DETACOBR.DBF'
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     corecto1 = 1
ENDIF
SELECT 1
yarchivo = 'CONCEPTO.DBF   '
@ 4, 15 SAY 'CONCEPTO.DBF   '
USE EXCLUSIVE concepto
IF  .NOT. corecto
     USE control
     DO WHILE .T.
          IF FLOCK()
               APPEND BLANK
               REPLACE archivo  ;
                       WITH  ;
                       'CONCEPTO.DBF'
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     corecto1 = 1
ENDIF
SELECT 1
yarchivo = 'COBRADOR.DBF    '
@ 4, 15 SAY 'COBRADOR.DBF    '
USE EXCLUSIVE cobrador
IF  .NOT. corecto
     USE control
     DO WHILE .T.
          IF FLOCK()
               APPEND BLANK
               REPLACE archivo  ;
                       WITH  ;
                       'COBRADOR.DBF'
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     corecto1 = 1
ENDIF
SELECT 1
yarchivo = 'BANCOS.DBF    '
@ 4, 15 SAY 'BANCOS.DBF    '
USE EXCLUSIVE bancos
IF  .NOT. corecto
     USE control
     DO WHILE .T.
          IF FLOCK()
               APPEND BLANK
               REPLACE archivo  ;
                       WITH  ;
                       'BANCOS.DBF'
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     corecto1 = 1
ENDIF
SELECT 1
yarchivo = 'DATOS.DBF    '
@ 4, 15 SAY 'DATOS.DBF    '
USE EXCLUSIVE datos
IF  .NOT. corecto
     USE control
     DO WHILE .T.
          IF FLOCK()
               APPEND BLANK
               REPLACE archivo  ;
                       WITH  ;
                       'DATOS.DBF'
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     corecto1 = 1
ENDIF
SELECT 1
yarchivo = 'RECORD.DBF    '
@ 4, 15 SAY 'RECORD.DBF    '
USE EXCLUSIVE record
IF  .NOT. corecto
     USE control
     DO WHILE .T.
          IF FLOCK()
               APPEND BLANK
               REPLACE archivo  ;
                       WITH  ;
                       'RECORD.DBF'
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     corecto1 = 1
ENDIF
SELECT 1
yarchivo = 'REGPAGOS.DBF    '
@ 4, 15 SAY 'REGPAGOS.DBF    '
USE EXCLUSIVE regpagos
IF  .NOT. corecto
     USE control
     DO WHILE .T.
          IF FLOCK()
               APPEND BLANK
               REPLACE archivo  ;
                       WITH  ;
                       'REGPAGOS.DBF'
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     corecto1 = 1
ENDIF
SELECT 1
yarchivo = 'INGRESOS.DBF    '
@ 4, 15 SAY 'INGRESOS.DBF    '
USE EXCLUSIVE ingresos
IF  .NOT. corecto
     USE control
     DO WHILE .T.
          IF FLOCK()
               APPEND BLANK
               REPLACE archivo  ;
                       WITH  ;
                       'ingresos.DBF'
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     corecto1 = 1
ENDIF
SELECT 1
yarchivo = 'MP' + lapmat + '.DBF'
@ 4, 15 SAY 'MP' + lapmat +  ;
  '.DBF'
use mp&lapmat exclu
IF  .NOT. corecto
     USE control
     DO WHILE .T.
          IF FLOCK()
               APPEND BLANK
               REPLACE archivo  ;
                       WITH 'MP' +  ;
                       lapmat +  ;
                       '.DBF'
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     corecto1 = 1
ENDIF
SELECT 1
yarchivo = 'AD' + lapmat + '.DBF'
@ 4, 15 SAY 'AD' + lapmat +  ;
  '.DBF'
use ad&lapmat exclu
IF  .NOT. corecto
     USE control
     DO WHILE .T.
          IF FLOCK()
               APPEND BLANK
               REPLACE archivo  ;
                       WITH 'AD' +  ;
                       lapmat +  ;
                       '.DBF'
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     corecto1 = 1
ENDIF
SELECT 1
yarchivo = 'NROATRAS.DBF    '
@ 4, 15 SAY 'NROATRAS.DBF    '
USE EXCLUSIVE nroatras
IF  .NOT. corecto
     USE control
     DO WHILE .T.
          IF FLOCK()
               APPEND BLANK
               REPLACE archivo  ;
                       WITH  ;
                       'NROATRAS.DBF'
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     corecto1 = 1
ENDIF
SELECT 1
yarchivo = 'ATRASADO.DBF    '
@ 4, 15 SAY 'ATRASADO.DBF    '
USE EXCLUSIVE atrasado
IF  .NOT. corecto
     USE control
     DO WHILE .T.
          IF FLOCK()
               APPEND BLANK
               REPLACE archivo  ;
                       WITH  ;
                       'ATRASADO.DBF'
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     corecto1 = 1
ENDIF
IF corecto1 = 0
     = msgerro( ;
       'No Existen archivo con estructuras da¤adas' ;
       )
ELSE
     = msgerro( ;
       'Existen Archivos Da¤ados Ejecuta la Aplicaci¢n Corregir, para repararlos' ;
       )
ENDIF
ON ERROR do errorhandler with message(),error(),program(),lineno()
CLOSE DATABASES
RELEASE WINDOW
RETURN
*
PROCEDURE errores
PARAMETER m.messg, m.code,  ;
          m.progr, m.lin
SET DEVICE TO SCREEN
DEFINE WINDOW msgerr FROM 10, 02  ;
       TO 18, 78 SHADOW TITLE  ;
       'MENSAJE DE ERROR ' COLOR  ;
       SCHEME 7
DO CASE
     CASE m.code = 125
          ACTIVATE WINDOW msgerr
          @ 00, 01 SAY  ;
            'Programa: ' +  ;
            ALLTRIM(m.progr +  ;
            '.PRG')
          @ 01, 01 SAY  ;
            'Linea del Error : ' +  ;
            LTRIM(STR(m.lin))
          @ 02, 01 SAY  ;
            'Mensaje: ' +  ;
            m.messg
          @ 03, 01 SAY 'Error : ' +  ;
            LTRIM(STR(m.code))
          ms = 'Active la Impresora y Pulse una Tecla ¢'
          @ 03, 01 SAY  ;
            SPACE((WCOLS() -  ;
            LEN(ms) - 6) / 2) +  ;
            SPACE(3)
          @ 03, COL() SAY ms
          ms = 'Presione Esc para Cancelar la Impresi¢n'
          @ 04, 01 SAY  ;
            SPACE((WCOLS() -  ;
            LEN(ms) - 6) / 2) +  ;
            SPACE(3)
          @ 04, COL() SAY ms
          SET ESCAPE OFF
          WAIT ''
          IF LASTKEY() = 27
               CLOSE DATABASES
               RELEASE WINDOW  ;
                       msgerr,  ;
                       record1,  ;
                       a1, a2, a3,  ;
                       _r
               RELEASE WINDOW all
               RELEASE WINDOW
               RELEASE WINDOW
               RELEASE WINDOW
               RELEASE WINDOW
               RELEASE WINDOW
               RELEASE WINDOW
               RETURN TO MASTER
          ENDIF
          RELEASE WINDOW msgerr
          SET DEVICE TO PRINTER
          RETRY
     CASE m.code = 108
          RETRY
     CASE m.code = 109 .OR.  ;
          m.code = 1707
          RETRY
     CASE m.code = 15
          ACTIVATE WINDOW msgerr
          @ 00, 01 SAY  ;
            'Programa: ' +  ;
            ALLTRIM(m.progr +  ;
            '.PRG')
          @ 01, 01 SAY  ;
            'Linea del Error : ' +  ;
            LTRIM(STR(m.lin))
          @ 02, 01 SAY  ;
            'Mensaje: ' +  ;
            'EL archivo:' +  ;
            yarchivo + '  ' +  ;
            m.messg
          @ 03, 01 SAY 'Error : ' +  ;
            LTRIM(STR(m.code))
          WAIT ''
          corecto = .F.
          RETURN
     CASE m.code = 114
          pos = AT('.', yarchivo)
          mcdx = SUBSTR(yarchivo,  ;
                 1, pos) + 'CDX'
          dele file &mcdx
          RETRY
     OTHERWISE
          ACTIVATE WINDOW msgerr
          @ 00, 01 SAY  ;
            'Programa: ' +  ;
            ALLTRIM(m.progr +  ;
            '.PRG')
          @ 01, 01 SAY  ;
            'Linea del Error : ' +  ;
            LTRIM(STR(m.lin))
          @ 02, 01 SAY  ;
            'Mensaje: ' +  ;
            m.messg
          @ 03, 01 SAY 'Error : ' +  ;
            LTRIM(STR(m.code))
          ms = 'Anote la Operaci¢n y Consulte con (SEGUNDO GONZALEZ)'
          @ 04, 01 SAY  ;
            SPACE((WCOLS() -  ;
            LEN(ms) - 6) / 2) +  ;
            SPACE(3)
          @ 04, COL() SAY ms
          @ 05, 01 SAY  ;
            'Telef.: (086)-626602 '
          SET ESCAPE OFF
          WAIT ''
ENDCASE
RELEASE WINDOW msgerr, record1,  ;
        a1, a2, a3, _r
RELEASE WINDOW
RELEASE WINDOW
RELEASE WINDOW
RELEASE WINDOW
RELEASE WINDOW
RELEASE WINDOW
to = 0
RETURN TO MASTER
*

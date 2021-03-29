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
  'Reconstrucci¢n de Indices'  ;
  COLOR W+/R 
@ 04, 04 SAY 'ARCHIVO : '
@ 06, 04 SAY 'INDICE  : '
@ 07, 15 SAY '...' COLOR W+/R* 
@ 07, COL() + 2 SAY 'Procesando'  ;
  COLOR W+/R 
@ 07, COL() + 2 SAY '...' COLOR W+/ ;
  R* 
SELECT 1
USE EXCLUSIVE concepto
@ 4, 15 SAY 'CONCEPTO.DBF    '
@ 6, 15 SAY 'CODIGO.CDX    '
DO WHILE .T.
     IF FLOCK()
          DELETE TAG all OF  ;
                 concepto
          INDEX ON codigo TAG  ;
                codigo
          UNLOCK
          EXIT
     ENDIF
ENDDO
SELECT 1
USE EXCLUSIVE bancos
@ 4, 15 SAY 'BANCOS.DBF    '
@ 6, 15 SAY 'CODIGO.CDX    '
DO WHILE .T.
     IF FLOCK()
          DELETE TAG all OF  ;
                 bancos
          INDEX ON codigo TAG  ;
                codigo
          UNLOCK
          EXIT
     ENDIF
ENDDO
SELECT 1
USE EXCLUSIVE cobrador
@ 4, 15 SAY 'COBRADOR.DBF    '
@ 6, 15 SAY 'CODIGO.CDX    '
DO WHILE .T.
     IF FLOCK()
          DELETE TAG all OF  ;
                 cobrador
          INDEX ON codigo TAG  ;
                codigo
          UNLOCK
          EXIT
     ENDIF
ENDDO
SELECT 1
USE EXCLUSIVE plancobr
@ 4, 15 SAY 'PLANCOBR.DBF    '
@ 6, 15 SAY 'CODIGO.CDX    '
DO WHILE .T.
     IF FLOCK()
          DELETE TAG all OF  ;
                 plancobr
          INDEX ON codigo TAG  ;
                codigo
          INDEX ON lapso + codigo  ;
                TAG lapcod
          UNLOCK
          EXIT
     ENDIF
ENDDO
SELECT 1
USE EXCLUSIVE detacobro
@ 4, 15 SAY 'DETACOBRO.DBF    '
@ 6, 15 SAY 'CODIGO.CDX    '
DO WHILE .T.
     IF FLOCK()
          DELETE TAG all OF  ;
                 detacobro
          INDEX ON codigo TAG  ;
                codigo
          @ 6, 15 SAY  ;
            'CODCUOTA.CDX    '
          INDEX ON lapso + codigo +  ;
                STR(cuota) TAG  ;
                codcuota
          INDEX ON lapso + codigo  ;
                TAG lapcod
          UNLOCK
          EXIT
     ENDIF
ENDDO
SELECT 1
USE EXCLUSIVE regpagos
@ 4, 15 SAY 'REGPAGOS.DBF    '
@ 6, 15 SAY 'bandepo.CDX    '
DO WHILE .T.
     IF FLOCK()
          DELETE TAG all OF  ;
                 regpagos
          INDEX ON (cod_banco +  ;
                nro_depo) TAG  ;
                bandepo
          @ 6, 15 SAY  ;
            'CED.CDX        '
          INDEX ON cedula TAG ced
          @ 6, 15 SAY  ;
            'CEDULA.CDX        '
          INDEX ON (cedula) TAG  ;
                cedula
          @ 6, 15 SAY  ;
            'CEDCUOTA.CDX        '
          INDEX ON (cedula +  ;
                STR(nrocuota))  ;
                TAG cedcuota
          @ 6, 15 SAY  ;
            'CEDRECIBO.CDX        '
          INDEX ON (cedula +  ;
                STR(recibo)) TAG  ;
                cedrecibo
          @ 6, 15 SAY  ;
            'FECHAPAGO.CDX        '
          INDEX ON  ;
                (DTOS(fecha_pago))  ;
                TAG fechapago
          @ 6, 15 SAY  ;
            'FECRECIB.CDX    '
          INDEX ON  ;
                (DTOS(fecha_pago) +  ;
                STR(recibo)) TAG  ;
                fecrecib
          @ 6, 15 SAY  ;
            'FECRECIB1.CDX    '
          INDEX ON  ;
                (DTOS(fecha_pago) +  ;
                STR(recibo)) TAG  ;
                fecreci1 FOR  ;
                entrega <> 'S'
          INDEX ON cedula +  ;
                STR(recibo) TAG  ;
                cedrecibo
          INDEX ON cedula +  ;
                STR(recibo) TAG  ;
                cedrecib
          INDEX ON recibo TAG  ;
                recibo
          INDEX ON codigo TAG  ;
                codigo
          UNLOCK
          EXIT
     ENDIF
ENDDO
SELECT 1
USE EXCLUSIVE atrasado
@ 4, 15 SAY 'ATRASADO.DBF    '
DO WHILE .T.
     IF FLOCK()
          DELETE TAG all OF  ;
                 atrasado
          @ 6, 15 SAY  ;
            'CED.CDX        '
          INDEX ON cedula TAG ced
          @ 6, 15 SAY  ;
            'RECIBO.CDX        '
          INDEX ON recibo TAG  ;
                recibo
          UNLOCK
          EXIT
     ENDIF
ENDDO
SELECT 1
USE EXCLUSIVE ingresos
@ 4, 15 SAY 'INGRESOS.DBF    '
@ 6, 15 SAY 'bandepo.CDX    '
DO WHILE .T.
     IF FLOCK()
          DELETE TAG all OF  ;
                 regpagos
          INDEX ON (cod_banco +  ;
                nro_depo) TAG  ;
                bandepo
          @ 6, 15 SAY  ;
            'FECHAPAGO.CDX        '
          INDEX ON  ;
                (DTOS(fecha_pago))  ;
                TAG fechapago
          @ 6, 15 SAY  ;
            'FECRECIB.CDX    '
          INDEX ON  ;
                (DTOS(fecha_pago) +  ;
                STR(recibo)) TAG  ;
                fecrecib
          @ 6, 15 SAY  ;
            'FECRECIB1.CDX    '
          INDEX ON  ;
                (DTOS(fecha_pago) +  ;
                STR(recibo)) TAG  ;
                fecreci1 FOR  ;
                entrega <> 'S'
          @ 6, 15 SAY  ;
            'FECHCON.CDX        '
          INDEX ON  ;
                DTOS(fecha_pago) +  ;
                concepto TAG  ;
                fechcon
          INDEX ON recibo TAG  ;
                recibo
          INDEX ON recibo TAG  ;
                recibos
          UNLOCK
          EXIT
     ENDIF
ENDDO
SELECT 1
USE EXCLUSIVE predatos
@ 4, 15 SAY 'PREDATOS.DBF    '
@ 6, 15 SAY 'cedula.CDX    '
DO WHILE .T.
     IF FLOCK()
          DELETE TAG all OF  ;
                 predatos
          @ 6, 15 SAY  ;
            'cedula.CDX    '
          INDEX ON cedula TAG  ;
                cedula
          @ 6, 15 SAY  ;
            'control.CDX    '
          INDEX ON control TAG  ;
                control
          @ 6, 15 SAY  ;
            'curced.CDX    '
          INDEX ON (cur +  ;
                STR(VAL(cedula),  ;
                3, 10)) TAG  ;
                curced
          UNLOCK
          EXIT
     ENDIF
ENDDO
SELECT 1
USE EXCLUSIVE contdeud
@ 4, 15 SAY 'CONTDEUD.DBF    '
@ 6, 15 SAY 'cedula.CDX    '
DO WHILE .T.
     IF FLOCK()
          DELETE TAG all OF  ;
                 contdeud
          INDEX ON  ;
                VAL(SUBSTR(cedula,  ;
                3, 13)) TAG ced
          @ 6, 15 SAY  ;
            'cedula.CDX    '
          INDEX ON cedula TAG  ;
                cedula
          UNLOCK
          EXIT
     ENDIF
ENDDO
SELECT 1
use mp&lapmat exclu
@ 4, 15 SAY 'MP' + lapmat +  ;
  '.DBF'
@ 6, 15 SAY 'ced.CDX'
DO WHILE .T.
     IF FLOCK()
          dele tag all of mp&lapmat
          INDEX ON ced TAG ced
          @ 6, 15 SAY  ;
            'cedcur.CDX        '
          INDEX ON ced + cur TAG  ;
                cedcur
          @ 6, 15 SAY  ;
            'mced.CDX           '
          INDEX ON VAL(SUBSTR(ced,  ;
                3, 13)) TAG mced
          @ 6, 15 SAY  ;
            'cd.CDX         '
          INDEX ON ced TAG cd
          @ 6, 15 SAY  ;
            'cedcur1.CDX      '
          INDEX ON VAL(SUBSTR(ced,  ;
                3, 13)) +  ;
                VAL(cur) TAG  ;
                cedcur1 FOR cur =  ;
                '32011'
          @ 6, 15 SAY  ;
            'cedcur2.CDX      '
          INDEX ON VAL(SUBSTR(ced,  ;
                3, 13)) +  ;
                VAL(cur) TAG  ;
                cedcur2 FOR cur =  ;
                '31018'
          @ 6, 15 SAY  ;
            'cedcur3.CDX      '
          INDEX ON VAL(SUBSTR(ced,  ;
                3, 13)) +  ;
                VAL(cur) TAG  ;
                cedcur3 FOR cur =  ;
                '31019'
          @ 6, 15 SAY  ;
            'cedcur4.CDX      '
          INDEX ON VAL(SUBSTR(ced,  ;
                3, 13)) +  ;
                VAL(cur) TAG  ;
                cedcur4 FOR cur <>  ;
                '32011'
          @ 6, 15 SAY  ;
            'cdcur.CDX      '
          INDEX ON VAL(cur) +  ;
                VAL(SUBSTR(ced, 3,  ;
                13)) TAG cdcur
          @ 6, 15 SAY  ;
            'sec.CDX      '
          INDEX ON cur + semestre +  ;
                STR(VAL(SUBSTR(ced,  ;
                3, 10))) TAG sec
          INDEX ON cur +  ;
                STR(VAL(SUBSTR(ced,  ;
                3, 10))) TAG  ;
                cursoced
          INDEX ON cur + semestre +  ;
                turno +  ;
                STR(VAL(SUBSTR(ced,  ;
                3, 10))) TAG  ;
                cusetuce
          INDEX ON cur + turno +  ;
                STR(VAL(SUBSTR(ced,  ;
                3, 10))) TAG  ;
                cutuce
          INDEX ON turno +  ;
                STR(VAL(SUBSTR(ced,  ;
                3, 10))) TAG  ;
                tuce
          INDEX ON plancobro +  ;
                cur + semestre +  ;
                STR(VAL(SUBSTR(ced,  ;
                3, 10))) TAG  ;
                placuse
          UNLOCK
          EXIT
     ENDIF
ENDDO
SELECT 1
use ad&lapmat exclu
@ 4, 15 SAY 'AD' + lapmat +  ;
  '.DBF'
@ 6, 15 SAY 'ced.CDX'
DO WHILE .T.
     IF FLOCK()
          dele tag all of ad&lapmat
          INDEX ON ced TAG ced
          @ 6, 15 SAY  ;
            'cedcur.CDX        '
          INDEX ON ced + cur TAG  ;
                cedcur
          @ 6, 15 SAY  ;
            'mced.CDX           '
          INDEX ON VAL(SUBSTR(ced,  ;
                3, 13)) TAG mced
          @ 6, 15 SAY  ;
            'cd.CDX         '
          INDEX ON ced TAG cd
          @ 6, 15 SAY  ;
            'cedcur1.CDX      '
          INDEX ON VAL(SUBSTR(ced,  ;
                3, 13)) +  ;
                VAL(cur) TAG  ;
                cedcur1 FOR cur =  ;
                '32011'
          @ 6, 15 SAY  ;
            'cedcur2.CDX      '
          INDEX ON VAL(SUBSTR(ced,  ;
                3, 13)) +  ;
                VAL(cur) TAG  ;
                cedcur2 FOR cur =  ;
                '31018'
          @ 6, 15 SAY  ;
            'cedcur3.CDX      '
          INDEX ON VAL(SUBSTR(ced,  ;
                3, 13)) +  ;
                VAL(cur) TAG  ;
                cedcur3 FOR cur =  ;
                '31019'
          @ 6, 15 SAY  ;
            'cedcur4.CDX      '
          INDEX ON VAL(SUBSTR(ced,  ;
                3, 13)) +  ;
                VAL(cur) TAG  ;
                cedcur4 FOR cur <>  ;
                '32011'
          @ 6, 15 SAY  ;
            'cdcur.CDX      '
          INDEX ON VAL(cur) +  ;
                VAL(SUBSTR(ced, 3,  ;
                13)) TAG cdcur
          @ 6, 15 SAY  ;
            'sec.CDX      '
          INDEX ON cur + semestre +  ;
                STR(VAL(SUBSTR(ced,  ;
                3, 10))) TAG sec
          INDEX ON cur +  ;
                STR(VAL(SUBSTR(ced,  ;
                3, 10))) TAG  ;
                cursoced
          INDEX ON cur + semestre +  ;
                turno +  ;
                STR(VAL(SUBSTR(ced,  ;
                3, 10))) TAG  ;
                cusetuce
          INDEX ON cur + turno +  ;
                STR(VAL(SUBSTR(ced,  ;
                3, 10))) TAG  ;
                cutuce
          INDEX ON turno +  ;
                STR(VAL(SUBSTR(ced,  ;
                3, 10))) TAG  ;
                tuce
          INDEX ON plancobro +  ;
                cur + semestre +  ;
                STR(VAL(SUBSTR(ced,  ;
                3, 10))) TAG  ;
                placuse
          UNLOCK
          EXIT
     ENDIF
ENDDO
SELECT 1
use ma&lapmat exclu
@ 4, 15 SAY 'MA' + lapmat +  ;
  '.DBF'
@ 6, 15 SAY 'ced.CDX'
DO WHILE .T.
     IF FLOCK()
          DELETE TAG all
          index on ced tag ced of ma&lapmat
          @ 6, 15 SAY  ;
            'cedcur.CDX        '
          INDEX ON ced + cur TAG  ;
                cedcur
          @ 6, 15 SAY  ;
            'mced.CDX           '
          INDEX ON VAL(SUBSTR(ced,  ;
                3, 13)) TAG mced
          INDEX ON VAL(SUBSTR(ced,  ;
                3, 13)) TAG ced1
          @ 6, 15 SAY  ;
            'cd.CDX         '
          INDEX ON ced TAG cd
          @ 6, 15 SAY  ;
            'cedcur1.CDX      '
          INDEX ON VAL(SUBSTR(ced,  ;
                3, 13)) +  ;
                VAL(cur) TAG  ;
                cedcur1 FOR cur =  ;
                '32012'
          @ 6, 15 SAY  ;
            'cedcur2.CDX      '
          INDEX ON VAL(SUBSTR(ced,  ;
                3, 13)) +  ;
                VAL(cur) TAG  ;
                cedcur2 FOR cur =  ;
                '31022'
          @ 6, 15 SAY  ;
            'cedcur3.CDX      '
          INDEX ON VAL(SUBSTR(ced,  ;
                3, 13)) +  ;
                VAL(cur) TAG  ;
                cedcur3 FOR cur =  ;
                '31023'
          @ 6, 15 SAY  ;
            'cedcur4.CDX      '
          INDEX ON VAL(SUBSTR(ced,  ;
                3, 13)) +  ;
                VAL(cur) TAG  ;
                cedcur4 FOR cur <>  ;
                '32012'
          @ 6, 15 SAY  ;
            'cdcur.CDX      '
          INDEX ON VAL(cur) +  ;
                VAL(SUBSTR(ced, 3,  ;
                13)) TAG cdcur
          UNLOCK
          EXIT
     ENDIF
ENDDO
SELECT 1
USE EXCLUSIVE datos
@ 4, 15 SAY 'DATOS.DBF    '
@ 6, 15 SAY 'ced.CDX    '
DO WHILE .T.
     IF FLOCK()
          DELETE TAG all OF datos
          INDEX ON  ;
                VAL(SUBSTR(cedula,  ;
                3, 13)) TAG ced
          @ 6, 15 SAY  ;
            '_r5p13u5ge.CDX    '
          INDEX ON cedula TAG  ;
                _r5p13u5ge
          @ 6, 15 SAY  ;
            'mced.CDX    '
          INDEX ON  ;
                VAL(SUBSTR(cedula,  ;
                3, 13)) TAG mced
          @ 6, 15 SAY  ;
            'nombres.CDX    '
          INDEX ON nombres TAG  ;
                nombres
          @ 6, 15 SAY  ;
            'cedula.CDX    '
          INDEX ON cedula TAG  ;
                cedula
          UNLOCK
          EXIT
     ENDIF
ENDDO
SELECT 1
USE EXCLUSIVE record
@ 4, 15 SAY 'RECORD.DBF    '
@ 6, 15 SAY 'cedcurso.CDX    '
DO WHILE .T.
     IF FLOCK()
          DELETE TAG all OF  ;
                 record
          INDEX ON cedula + curso  ;
                TAG cedcurso
          @ 6, 15 SAY  ;
            'cedula.CDX    '
          INDEX ON cedula TAG  ;
                cedula
          @ 6, 15 SAY  ;
            'cdcr.CDX    '
          INDEX ON  ;
                VAL(SUBSTR(cedula,  ;
                3, 13)) +  ;
                VAL(curso) TAG  ;
                cdcr
          @ 6, 15 SAY  ;
            'cedcur1.CDX    '
          INDEX ON  ;
                VAL(SUBSTR(cedula,  ;
                3, 13)) +  ;
                VAL(curso) TAG  ;
                cedcur1 FOR curso =  ;
                '32012'
          @ 6, 15 SAY  ;
            'cedcur2.CDX    '
          INDEX ON  ;
                VAL(SUBSTR(cedula,  ;
                3, 13)) +  ;
                VAL(curso) TAG  ;
                cedcur2 FOR curso =  ;
                '31022'
          @ 6, 15 SAY  ;
            'cedcur3.CDX    '
          INDEX ON  ;
                VAL(SUBSTR(cedula,  ;
                3, 13)) +  ;
                VAL(curso) TAG  ;
                cedcur3 FOR curso =  ;
                '31023'
          UNLOCK
          EXIT
     ENDIF
ENDDO
SELECT 1
USE EXCLUSIVE solicitu
@ 4, 15 SAY 'SOLICITU.DBF    '
@ 6, 15 SAY 'cedula.CDX    '
DO WHILE .T.
     IF FLOCK()
          DELETE TAG all OF datos
          INDEX ON  ;
                VAL(SUBSTR(cedula,  ;
                3, 13)) TAG  ;
                cedula
          INDEX ON recibo TAG  ;
                recibo
          UNLOCK
          EXIT
     ENDIF
ENDDO
CLOSE DATABASES
RELEASE WINDOW
RETURN
*

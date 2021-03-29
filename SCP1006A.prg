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
          INDEX ON VAL(SUBSTR(ced,  ;
                3, 13)) TAG ced1
          UNLOCK
          EXIT
     ENDIF
ENDDO
SET CENTURY ON
RELEASE WINDOW
RELEASE WINDOW
m.cod1 = SUBSTR(lapmat, 1, 2)
m.cod2 = SUBSTR(lapmat, 3, 4)
l_actual = m.cod1 + '-' + m.cod2 +  ;
           '/' + RIGHT(lapso, 4)
@ 22, 01 SAY 'Lapso Actual : ' +  ;
  l_actual COLOR W+/B,W+/B 
CLOSE DATABASES
RETURN
*

CLOSE DATABASES
SET DELETED ON
SET TALK OFF
SET SAFETY OFF
SET BELL OFF
SET MESSAGE TO 23 CENTER
SET BELL TO 1700, 4
SET CENTURY ON
SELECT 7
USE lapso1
SCATTER MEMO MEMVAR
SELECT 1
USE datos
SET ORDER TO cedula
SELECT 2
use mp&lapmat
SET ORDER TO sec
SELECT 3
CREATE CURSOR alumnos (cedula C  ;
       (12), total_d N (13, 2),  ;
       monto_p  N(13,2),;
       deuda_a  n(13,2),;
       concepto1 C (50),  ;
       concepto2 C (50),  ;
       concepto3 C (50),  ;
       concepto4 C (50),  ;
       concepto5 C (50), plancob  ;
       C (4), plancobad C (4),  ;
       lapsomat C (6))
INDEX ON cedula TAG cedula
SELECT 4
USE contdeud
SET ORDER TO cedula
SELECT 8
use ad&lapmat
SET ORDER TO sec
DEFINE WINDOW record1 FROM 12, 10  ;
       TO 18, 70 GROW FLOAT CLOSE  ;
       ZOOM SHADOW TITLE  ;
       'Cierre de Matricula de Control del Pagos'  ;
       MINIMIZE COLOR SCHEME 5
ACTIVATE WINDOW record1
SET CENTURY ON
DO WHILE .T.
     SELECT 7
     m.cod1 = SUBSTR(m.lapso1, 1, 2)
     m.cod2 = SUBSTR(m.lapso1, 3, 4)
     m.cod3 = SUBSTR(m.lapso, 3,  4)
     hh = '01/01/' + m.cod2
     hh = CTOD(hh)
     hh = ALLTRIM(STR(YEAR(hh)))
     @ 01, 01 SAY 'Lapso Actual      : ' COLOR SCHEME 5 GET m.cod1  PICTURE '99'
     @ 01, 24 SAY '-'
     @ 01, 25 GET hh PICTURE  '9999' COLOR SCHEME 5
     CLEAR GETS
     @ 03, 00 TO 03, WCOLS() DOUBLE
     @ 04, 26 GET m.choice  ;
       DEFAULT 1 SIZE 1, 10, 10  ;
       PICTURE '@*HT \<Ok' COLOR  ;
       SCHEME 5
     READ
     IF LASTKEY() = 27 .OR. m.choice = 2
          RELEASE WINDOW record1
          CLOSE DATABASES
          RETURN
     ENDIF
     lapmat = lapso1
     SELECT 2
     COUNT FOR  .NOT. DELETED() TO ptaje
     IF ptaje <= 0
          p = msgerro('No Existen alumnos para este Lapso, verifique')
          LOOP
     ENDIF
     GOTO TOP
     IF cierre = 'S'
          p = msgerro('Usted, ya realiz¢ el cierre para este lapso, verifique')
          LOOP
     ENDIF
     alum = 0
     pp = 1
     = relock1('Actualizando.....')
     DO WHILE  .NOT. EOF()
          = relock2(((pp * 100) /ptaje))
          m.cedula = ced
          IF status = 'X'
               pp = pp + 1
               SELECT 2
               SKIP
               LOOP
          ENDIF
          SELECT 1
          SEEK m.cedula
          IF  .NOT. FOUND()
               pp = pp + 1
               SELECT 2
               WAIT WINDOW 'La Cedula: ' + m.cedula + ' No existe en Datos'
               SKIP
               LOOP
          ENDIF
          m.nombres = nombres
          SELECT 2
          m.curso = cur
          m.plan = plancobro
          m.lapsomat = lapmat
          m.total_deud = total_deud
          m.monto_paga = monto_paga
          m.saldo = total_deud - monto_paga
          m.ultimacuot = ultimacuot
          m.cuota2 = cuota2
          cuantopaga = SPACE(15)
          cuantopaga = ALLTRIM(STR(m.cuota2))
          alum = 1
          IF m.saldo <= 0
               pp = pp + 1
               SELECT 2
               SKIP
               LOOP
          ENDIF
          concep1 = 'Deuda Correspondiente al Semestre '+m.cod1 + '-' +  ;
                    m.cod2 + '/' + m.cod3
          m.ultimacuot = 7 - m.ultimacuot
          cuotadeuda = SPACE(30)
          cuotadeuda = ALLTRIM(STR(m.ultimacuot))
          concep2 = 'la cual contempla las ' +  cuotadeuda + ' £ltimas cuotas,'
          IF m.cod1 = '01'
               concep3 = 'entre Septiembre de ' +  ;
                         m.cod2 +  ;
                         ' y Febrero de ' +  ;
                         m.cod3
          ELSE
               concep3 = 'entre Marzo de ' +  ;
                         m.cod3 +  ;
                         ' y Agosto de ' +  ;
                         m.cod3
          ENDIF
          concep4 = 'pagaba una mensualidad de: Bs.' +  ;
                    cuantopaga
          SELECT 3
          DO WHILE .T.
               IF FLOCK()
                    APPEND BLANK
                    REPLACE cedula WITH m.cedula, total_d WITH m.saldo
                    replace deuda_a with m.saldo
                    REPLACE concepto1 WITH concep1,concepto2 WITH concep2
                    REPLACE concepto3 WITH concep3,concepto4 WITH concep4
                    REPLACE lapsomat WITH lapmat, plancob WITH m.plan
                    UNLOCK
                    EXIT
               ENDIF
          ENDDO
          SELECT 4
          IF SEEK(m.cedula)
             mtotal_d=total_d
             mmonto_p=monto_p
             SELECT 3
             REPLA total_d with total_d+mtotal_d, monto_p with mmonto_p
          endif   
          SELECT 2
          pp = pp + 1
          SKIP
     ENDDO
     SELECT 8
     COUNT FOR  .NOT. DELETED()TO ptaje
     IF ptaje <= 0
          p = msgerro('No Existen alumnos con matricula adicional para este Lapso, verifique')
          *LOOP
     ENDIF
     GOTO TOP
     IF cierre = 'S'
          p = msgerro('Usted, ya realiz¢ el cierre para este lapso, verifique')
          LOOP
     ENDIF
     alum = 1
     pp = 1
     = relock1('Actualizando.....')
     DO WHILE  .NOT. EOF()
          = relock2(((pp * 100) /  ;
            ptaje))
          m.cedula = ced
          IF status = 'X'
               pp = pp + 1
               SELECT 8
               SKIP
               LOOP
          ENDIF
          SELECT 1
          SEEK m.cedula
          IF  .NOT. FOUND()
               pp = pp + 1
               SELECT 8
               WAIT WINDOW 'La Cedula: '+m.cedula +' No existe en Datos'
               SKIP
               LOOP
          ENDIF
          m.nombres = nombres
          SELECT 8
          m.curso = cur
          m.plan = plancobro
          m.lapsomat = lapmat
          m.total_deud = total_deud
          m.monto_paga = monto_paga
          m.saldo = total_deud - monto_paga
          m.ultimacuot = ultimacuot
          m.cuota2 = cuota2
          cuantopaga = SPACE(15)
          cuantopaga = ALLTRIM(STR(m.cuota2))
          alum = 1
          IF m.saldo <= 0
               pp = pp + 1
               SELECT 8
               SKIP
               LOOP
          ENDIF
          concep1 = 'Deuda Correspondiente al Semestre ' +  ;
                    m.cod1 + '-' +  ;
                    m.cod2 + '/' +  ;
                    m.cod3
          m.ultimacuot = 7 -  ;
                         m.ultimacuot
          cuotadeuda = SPACE(30)
          cuotadeuda = ALLTRIM(STR(m.ultimacuot))
          concep2 = 'la cual contempla las ' +  ;
                    cuotadeuda +  ;
                    ' £ltimas cuotas,'
          IF m.cod1 = '01'
               concep3 = 'entre Septiembre de ' +  ;
                         m.cod2 +  ;
                         ' y Febrero de ' +  ;
                         m.cod3
          ELSE
               concep3 = 'entre Marzo de ' +  ;
                         m.cod3 +  ;
                         ' y Agosto de ' +  ;
                         m.cod3
          ENDIF
          concep4 = 'pagaba una mensualidad de: Bs.' +  ;
                    cuantopaga
          concep5 = 'Mat.Adicional(es) ' +  ;
                    cuotadeuda +  ;
                    ' cuotas de Bs.' +  ;
                    cuantopaga
          concep6 = 'por Materia(s) Adicional(es).'
          SELECT 3
          SET ORDER TO CEDULA
          DO WHILE .T.
               IF FLOCK()
                    IF SEEK(m.cedula)
                         REPLACE total_d WITH total_d + m.saldo
                         REPLACE concepto5 WITH concep5, plancobad WITH m.plan
                    ELSE
                         APPEND BLANK
                         REPLACE cedula WITH m.cedula, ;
                                 total_d WITH m.saldo
                         REPLACE concepto1 WITH concep1, concepto2 WITH concep2
                         REPLACE concepto3 WITH concep3, concepto4 WITH concep4
                         REPLACE concepto5 WITH concep6
                         REPLACE lapsomat  WITH lapmat, plancobad WITH m.plan
                    ENDIF
                    UNLOCK
                    EXIT
               ENDIF
          ENDDO
          SELECT 8
          pp = pp + 1
          SKIP
     ENDDO
     = relock2(((pp * 100) / ptaje))
     = relock3()
     IF alum = 1
          kl = acepta('Realizo un Cierre de Pagos')
          IF kl = 2
               SET DEVICE TO SCREEN
               RELEASE WINDOW a1, record1, record1
               EXIT
          ENDIF
     ELSE
          = msgerro('No existen alumnos con notas para realizar el cierre, verifique')
          SET DEVICE TO SCREEN
          RELEASE WINDOW a1,record1,record1
          EXIT
     ENDIF
     SELECT 3
     GOTO TOP
     DO WHILE  .NOT. EOF()
          SCATTER MEMO MEMVAR
          SELECT 4
          SET ORDER TO CEDULA
          DO WHILE .T.
               IF RLOCK()
                    SELECT 4
                    IF SEEK(m.cedula)
                         GATHER MEMVAR MEMO
                         donde=1
                         for i=1 to 5
                            oo=ltrim(str(i))
                            if monto&oo=0
                              donde=i
                              exit
                            endif  
                         endfor
                         replace monto&oo with m.deuda_a
                    ELSE
                         APPEND BLANK
                         GATHER MEMVAR MEMO
                         replace monto1 with total_d
                    ENDIF
                    UNLOCK
                    EXIT
               ENDIF
          ENDDO
          SELECT 3
          SKIP
     ENDDO
     SELECT 7
     USE lapso1
     GOTO TOP
     SCATTER MEMO MEMVAR
     SET CENTURY ON
     DEFINE WINDOW record1 FROM  ;
            05, 10 TO 13, 70 GROW  ;
            FLOAT CLOSE ZOOM  ;
            SHADOW TITLE  ;
            'Lapso Historico Actual'  ;
            MINIMIZE COLOR SCHEME  ;
            5
     ACTIVATE WINDOW record1
     x1 = 'Actualizaci¢n del Nuevo Lapso Historico'
     xfechaf = SUBSTR(DTOC(m.fechaf),  ;
               4, 7)
     xfechar = SUBSTR(DTOC(m.fechar),  ;
               4, 7)
     xfechaexta = SUBSTR(DTOC(m.fechaexta),  ;
                  4, 7)
     m.cod1 = SUBSTR(m.lapso1, 1,  ;
              2)
     m.cod2 = SUBSTR(m.lapso1, 3,  ;
              4)
     m.cod3 = SUBSTR(m.lapso, 3,  ;
              4)
     hh = '01/01/' + m.cod2
     hh = CTOD(hh)
     hh = ALLTRIM(STR(YEAR(hh)))
     @ 01, 01 SAY  ;
       'Lapso Actual         :  '
     @ 01, 25 GET m.cod1 PICTURE  ;
       '99'
     @ 01, 27 SAY '-'
     @ 01, 28 GET hh PICTURE  ;
       '9999'
     DO WHILE .T.
          @ 01, 01 SAY  ;
            'Lapso Actual         :  '
          kcol = COL()
          @ 01, 27 SAY '-'
          @ 01, 28 GET hh PICTURE  ;
            '9999'
          @ 02, 01 SAY  ;
            'Fecha Final          : '  ;
            GET xfechaf PICTURE  ;
            '99/9999'
          @ 03, 01 SAY  ;
            'Fecha Revisi¢n       : '  ;
            GET xfechar PICTURE  ;
            '99/9999'
          CLEAR GETS
          @ 01, 25 GET m.cod1  ;
            PICTURE '99'
          @ 05, 00 TO 05, WCOLS()  ;
            DOUBLE
          @ 06, 14 GET m.choice  ;
            DEFAULT 1 SIZE 1, 10,  ;
            10 PICTURE  ;
            '@*HT \<Aceptar;\<Cancelar'  ;
            COLOR SCHEME 5
          READ CYCLE
          IF m.choice = 1
               IF m.cod1 = '01'
                    m.cod1 = '0' +  ;
                             LTRIM(STR(VAL(m.cod1) +  ;
                             1))
                    xfechaf = '07/' +  ;
                              m.cod3
                    xfechar = '09/' +  ;
                              m.cod3
                    hh = '01/01/' +  ;
                         m.cod2
                    hh = CTOD(hh)
                    hh = ALLTRIM(STR(YEAR(hh)))
                    @ 01, 01 SAY  ;
                      'Lapso Actual         :  '
                    @ 01, 25 GET  ;
                      m.cod1  ;
                      PICTURE  ;
                      '99'
                    @ 01, 27 SAY  ;
                      '-'
                    @ 01, 28 GET  ;
                      hh PICTURE  ;
                      '9999'
                    @ 02, 01 SAY  ;
                      'Fecha Final          : '  ;
                      GET xfechaf  ;
                      PICTURE  ;
                      '99/9999'
                    @ 03, 01 SAY  ;
                      'Fecha Revisi¢n       : '  ;
                      GET xfechar  ;
                      PICTURE  ;
                      '99/9999'
                    CLEAR GETS
               ELSE
                    m.cod1 = '0' +  ;
                             LTRIM(STR(VAL(m.cod1) -  ;
                             1))
                    xfechaf = '02/' +  ;
                              m.cod3
                    xfechar = '03/' +  ;
                              m.cod3
                    hh = '01/01/' +  ;
                         m.cod3
                    hh = CTOD(hh)
                    hh = ALLTRIM(STR(YEAR(hh)))
                    @ 01, 01 SAY  ;
                      'Lapso Actual         :  '
                    @ 01, 25 GET  ;
                      m.cod1  ;
                      PICTURE  ;
                      '99'
                    @ 01, 27 SAY  ;
                      '-'
                    @ 01, 28 GET  ;
                      hh PICTURE  ;
                      '9999'
                    @ 02, 01 SAY  ;
                      'Fecha Final          : '  ;
                      GET xfechaf  ;
                      PICTURE  ;
                      '99/9999'
                    @ 03, 01 SAY  ;
                      'Fecha Revisi¢n       : '  ;
                      GET xfechar  ;
                      PICTURE  ;
                      '99/9999'
                    CLEAR GETS
               ENDIF
               m.lapso1 = m.cod1 +  ;
                          hh
               m.fechaf = CTOD( ;
                          '01/' +  ;
                          xfechaf)
               m.fechar = CTOD( ;
                          '01/' +  ;
                          xfechar)
               gg = IIF(xfechaexta =  ;
                    'SEPTIEMBRE',  ;
                    '10', '06')
               m.fechafmp = CTOD( ;
                            '01/' +  ;
                            gg +  ;
                            '/' +  ;
                            m.cod1)
               DO WHILE .T.
                    IF RLOCK()
                         GATHER MEMVAR  ;
                                MEMO
                         UNLOCK
                         EXIT
                    ENDIF
               ENDDO
               m.lapzo = m.lapso1
               lapmat = m.cod1 +  ;
                        hh
               esta = FILE('MA' +  ;
                      lapmat +  ;
                      '.DBF')
               IF  .NOT. esta
                    USE matrcont
                    copy struc to ma&lapmat
               ENDIF
               esta = FILE('MP' +  ;
                      lapmat +  ;
                      '.DBF')
               IF  .NOT. esta
                    USE mapocont
                    copy struc to mp&lapmat
               ENDIF
               esta = FILE('AD' +  ;
                      lapmat +  ;
                      '.DBF')
               IF  .NOT. esta
                    USE adcontr
                    copy struc to ad&lapmat
               ENDIF
          ENDIF
          EXIT
     ENDDO
     lapso2 = SUBSTR(lapmat, 3,  ;
              4)
     lapso3 = ALLTRIM(STR(VAL(SUBSTR(lapmat,  ;
              3, 4)) + 1))
     m.ano = m.cod1 + '-' +  ;
             lapso2 + '/' +  ;
             lapso3
     SELECT 2
     GOTO TOP
     DO WHILE .T.
          IF FLOCK()
               REPLACE cierre  ;
                       WITH 'S'  ;
                       ALL
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     SELECT 8
     GOTO TOP
     DO WHILE .T.
          IF FLOCK()
               REPLACE cierre  ;
                       WITH 'S'  ;
                       ALL
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     CLOSE DATABASES
     RELEASE WINDOW record1
     @ 22, 01 SAY  ;
       'Lapso Actual : ' COLOR W+/ ;
       B,W+/B 
     @ 22, COL() SAY m.ano COLOR  ;
       W+/B,W+/B 
     DEFINE WINDOW indices FROM  ;
            11, 10 TO 19, 60 GROW  ;
            FLOAT SHADOW NONE  ;
            COLOR SCHEME 7
     ACTIVATE WINDOW indices
     MOVE WINDOW indices CENTER
     @ 00, 00 TO 08, 51
     @ 02, 00 SAY 'Ã' +  ;
       REPLICATE('Ä', 49) + '´'
     @ 01, 12 SAY  ;
       'Reconstrucci¢n de Indices'  ;
       COLOR W+/R 
     @ 04, 04 SAY 'ARCHIVO : '
     @ 06, 04 SAY 'INDICE  : '
     @ 07, 15 SAY '...' COLOR W+/ ;
       R* 
     @ 07, COL() + 2 SAY  ;
       'Procesando' COLOR W+/R 
     @ 07, COL() + 2 SAY '...'  ;
       COLOR W+/R* 
     SELECT 1
     use mp&lapmat exclu
     @ 4, 15 SAY 'MP' + lapmat +  ;
       '.DBF'
     @ 6, 15 SAY 'ced.CDX'
     DO WHILE .T.
          IF FLOCK()
               dele tag all of mp&lapmat
               INDEX ON ced TAG  ;
                     ced
               @ 6, 15 SAY  ;
                 'cedcur.CDX        '
               INDEX ON ced + cur  ;
                     TAG cedcur
               @ 6, 15 SAY  ;
                 'mced.CDX           '
               INDEX ON  ;
                     VAL(SUBSTR(ced,  ;
                     3, 13)) TAG  ;
                     mced
               @ 6, 15 SAY  ;
                 'cd.CDX         '
               INDEX ON ced TAG  ;
                     cd
               @ 6, 15 SAY  ;
                 'cedcur1.CDX      '
               INDEX ON  ;
                     VAL(SUBSTR(ced,  ;
                     3, 13)) +  ;
                     VAL(cur) TAG  ;
                     cedcur1 FOR  ;
                     cur =  ;
                     '32011'
               @ 6, 15 SAY  ;
                 'cedcur2.CDX      '
               INDEX ON  ;
                     VAL(SUBSTR(ced,  ;
                     3, 13)) +  ;
                     VAL(cur) TAG  ;
                     cedcur2 FOR  ;
                     cur =  ;
                     '31018'
               @ 6, 15 SAY  ;
                 'cedcur3.CDX      '
               INDEX ON  ;
                     VAL(SUBSTR(ced,  ;
                     3, 13)) +  ;
                     VAL(cur) TAG  ;
                     cedcur3 FOR  ;
                     cur =  ;
                     '31019'
               @ 6, 15 SAY  ;
                 'cedcur4.CDX      '
               INDEX ON  ;
                     VAL(SUBSTR(ced,  ;
                     3, 13)) +  ;
                     VAL(cur) TAG  ;
                     cedcur4 FOR  ;
                     cur <>  ;
                     '32011'
               @ 6, 15 SAY  ;
                 'cdcur.CDX      '
               INDEX ON VAL(cur) +  ;
                     VAL(SUBSTR(ced,  ;
                     3, 13)) TAG  ;
                     cdcur
               @ 6, 15 SAY  ;
                 'sec.CDX      '
               INDEX ON cur +  ;
                     semestre +  ;
                     STR(VAL(SUBSTR(ced,  ;
                     3, 10))) TAG  ;
                     sec
               INDEX ON cur +  ;
                     STR(VAL(SUBSTR(ced,  ;
                     3, 10))) TAG  ;
                     cursoced
               INDEX ON cur +  ;
                     semestre +  ;
                     turno +  ;
                     STR(VAL(SUBSTR(ced,  ;
                     3, 10))) TAG  ;
                     cusetuce
               INDEX ON cur +  ;
                     turno +  ;
                     STR(VAL(SUBSTR(ced,  ;
                     3, 10))) TAG  ;
                     cutuce
               INDEX ON turno +  ;
                     STR(VAL(SUBSTR(ced,  ;
                     3, 10))) TAG  ;
                     tuce
               INDEX ON plancobro +  ;
                     cur +  ;
                     semestre +  ;
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
               INDEX ON ced TAG  ;
                     ced
               @ 6, 15 SAY  ;
                 'cedcur.CDX        '
               INDEX ON ced + cur  ;
                     TAG cedcur
               @ 6, 15 SAY  ;
                 'mced.CDX           '
               INDEX ON  ;
                     VAL(SUBSTR(ced,  ;
                     3, 13)) TAG  ;
                     mced
               @ 6, 15 SAY  ;
                 'cd.CDX         '
               INDEX ON ced TAG  ;
                     cd
               @ 6, 15 SAY  ;
                 'cedcur1.CDX      '
               INDEX ON  ;
                     VAL(SUBSTR(ced,  ;
                     3, 13)) +  ;
                     VAL(cur) TAG  ;
                     cedcur1 FOR  ;
                     cur =  ;
                     '32011'
               @ 6, 15 SAY  ;
                 'cedcur2.CDX      '
               INDEX ON  ;
                     VAL(SUBSTR(ced,  ;
                     3, 13)) +  ;
                     VAL(cur) TAG  ;
                     cedcur2 FOR  ;
                     cur =  ;
                     '31018'
               @ 6, 15 SAY  ;
                 'cedcur3.CDX      '
               INDEX ON  ;
                     VAL(SUBSTR(ced,  ;
                     3, 13)) +  ;
                     VAL(cur) TAG  ;
                     cedcur3 FOR  ;
                     cur =  ;
                     '31019'
               @ 6, 15 SAY  ;
                 'cedcur4.CDX      '
               INDEX ON  ;
                     VAL(SUBSTR(ced,  ;
                     3, 13)) +  ;
                     VAL(cur) TAG  ;
                     cedcur4 FOR  ;
                     cur <>  ;
                     '32011'
               @ 6, 15 SAY  ;
                 'cdcur.CDX      '
               INDEX ON VAL(cur) +  ;
                     VAL(SUBSTR(ced,  ;
                     3, 13)) TAG  ;
                     cdcur
               @ 6, 15 SAY  ;
                 'sec.CDX      '
               INDEX ON cur +  ;
                     semestre +  ;
                     STR(VAL(SUBSTR(ced,  ;
                     3, 10))) TAG  ;
                     sec
               INDEX ON cur +  ;
                     STR(VAL(SUBSTR(ced,  ;
                     3, 10))) TAG  ;
                     cursoced
               INDEX ON cur +  ;
                     semestre +  ;
                     turno +  ;
                     STR(VAL(SUBSTR(ced,  ;
                     3, 10))) TAG  ;
                     cusetuce
               INDEX ON cur +  ;
                     turno +  ;
                     STR(VAL(SUBSTR(ced,  ;
                     3, 10))) TAG  ;
                     cutuce
               INDEX ON turno +  ;
                     STR(VAL(SUBSTR(ced,  ;
                     3, 10))) TAG  ;
                     tuce
               INDEX ON plancobro +  ;
                     cur +  ;
                     semestre +  ;
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
               index on ced tag ced of;
ma&lapmat
               @ 6, 15 SAY  ;
                 'cedcur.CDX        '
               INDEX ON ced + cur  ;
                     TAG cedcur
               @ 6, 15 SAY  ;
                 'mced.CDX           '
               INDEX ON  ;
                     VAL(SUBSTR(ced,  ;
                     3, 13)) TAG  ;
                     mced
               INDEX ON  ;
                     VAL(SUBSTR(ced,  ;
                     3, 13)) TAG  ;
                     ced1
               @ 6, 15 SAY  ;
                 'cd.CDX         '
               INDEX ON ced TAG  ;
                     cd
               @ 6, 15 SAY  ;
                 'cedcur1.CDX      '
               INDEX ON  ;
                     VAL(SUBSTR(ced,  ;
                     3, 13)) +  ;
                     VAL(cur) TAG  ;
                     cedcur1 FOR  ;
                     cur =  ;
                     '32012'
               @ 6, 15 SAY  ;
                 'cedcur2.CDX      '
               INDEX ON  ;
                     VAL(SUBSTR(ced,  ;
                     3, 13)) +  ;
                     VAL(cur) TAG  ;
                     cedcur2 FOR  ;
                     cur =  ;
                     '31022'
               @ 6, 15 SAY  ;
                 'cedcur3.CDX      '
               INDEX ON  ;
                     VAL(SUBSTR(ced,  ;
                     3, 13)) +  ;
                     VAL(cur) TAG  ;
                     cedcur3 FOR  ;
                     cur =  ;
                     '31023'
               @ 6, 15 SAY  ;
                 'cedcur4.CDX      '
               INDEX ON  ;
                     VAL(SUBSTR(ced,  ;
                     3, 13)) +  ;
                     VAL(cur) TAG  ;
                     cedcur4 FOR  ;
                     cur <>  ;
                     '32012'
               @ 6, 15 SAY  ;
                 'cdcur.CDX      '
               INDEX ON VAL(cur) +  ;
                     VAL(SUBSTR(ced,  ;
                     3, 13)) TAG  ;
                     cdcur
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     RELEASE WINDOW indices
     RELEASE WINDOW record1, a5
     SET DEVICE TO SCREEN
     RELEASE WINDOW a1
     EXIT
ENDDO
CLOSE DATABASES
RELEASE WINDOW record1
RETURN
*

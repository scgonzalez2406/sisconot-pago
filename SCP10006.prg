CLOSE DATABASES
SELECT 1
USE lapso1
GOTO TOP
SET SAFETY ON
SCATTER MEMO MEMVAR
SET CENTURY ON
DEFINE WINDOW record1 FROM 10, 10  ;
       TO 20, 70 FLOAT NOCLOSE  ;
       SHADOW NOMINIMIZE COLOR  ;
       SCHEME 5
ACTIVATE WINDOW record1
x1 = 'Actualizaci¢n del Lapso Historico'
@ 00, 00 SAY SPACE(((WCOLS() -  ;
  LEN(x1) - 6) / 2) + 3) + x1
@ 01, 00 TO 01, WCOLS() - 2
xfechaf = SUBSTR(DTOC(m.fechaf),  ;
          4, 7)
xfechar = SUBSTR(DTOC(m.fechar),  ;
          4, 7)
xfechaexta = SUBSTR(DTOC(m.fechaexta),  ;
             4, 7)
m.cod1 = SUBSTR(m.lapso1, 1, 2)
m.cod2 = SUBSTR(m.lapso1, 3, 4)
m.cod3 = SUBSTR(m.lapso, 3, 4)
lapmat = m.cod1 + m.cod2
DO WHILE .T.
     @ 03, 01 SAY  ;
       'Fecha Final          : '  ;
       GET xfechaf PICTURE  ;
       '99/9999'
     @ 04, 01 SAY  ;
       'Fecha Revisi¢n       : '  ;
       GET xfechar PICTURE  ;
       '99/9999'
     @ 05, 01 SAY  ;
       'Fecha Extraordinario : '  ;
       GET xfechaexta PICTURE  ;
       '99/9999'
     CLEAR GETS
     @ 02, 01 SAY  ;
       'Lapso Actual         : '  ;
       GET m.cod1 PICTURE  ;
       '@M 01,02'
     @ 02, COL() SAY '-' GET  ;
       m.cod2 PICTURE '9999'
     @ 06, 01 TO 06, WCOLS()  ;
       DOUBLE
     @ 07, 14 GET m.choice  ;
       DEFAULT 1 SIZE 1, 10, 10  ;
       PICTURE  ;
       '@*HT \<Aceptar;\<Cancelar'  ;
       COLOR SCHEME 5
     READ CYCLE
     m.cod3 = LTRIM(STR(VAL(m.cod2) +  ;
              1))
     IF LASTKEY() = 27
          RELEASE WINDOW
          CLOSE DATABASES
          RETURN
     ENDIF
     IF m.choice = 1
          m.lapso1 = m.cod1 +  ;
                     m.cod2
          IF m.cod1 = '01'
               xfechaf = '02/' +  ;
                         m.cod3
               xfechar = '03/' +  ;
                         m.cod3
               xfechaextr = '03/' +  ;
                            m.cod3
          ELSE
               xfechaf = '07/' +  ;
                         m.cod3
               xfechar = '07/' +  ;
                         m.cod3
               xfechaextr = '09/' +  ;
                            m.cod3
          ENDIF
          @ 03, 01 SAY  ;
            'Fecha Final          : '  ;
            GET xfechaf PICTURE  ;
            '99/9999'
          @ 04, 01 SAY  ;
            'Fecha Revisi¢n       : '  ;
            GET xfechar PICTURE  ;
            '99/9999'
          @ 05, 01 SAY  ;
            'Fecha Extraordinario : '  ;
            GET xfechaextr  ;
            PICTURE '99/9999'
          CLEAR GETS
          m.fechaf = CTOD('01/' +  ;
                     xfechaf)
          m.fechar = CTOD('01/' +  ;
                     xfechar)
          m.fechaexta = CTOD( ;
                        '01/' +  ;
                        xfechaextr)
          m.fechaexts = m.fechaexta
          m.lapso = IIF(SUBSTR(m.lapso1,  ;
                    1, 2) = '01',  ;
                    '11' + m.cod3,  ;
                    '04' +  ;
                    m.cod3)
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
                   m.cod2
          esta = FILE('MA' +  ;
                 lapmat +  ;
                 '.DBF')
          IF  .NOT. esta
               USE matrcontr
               copy struc to ma&lapmat
          ENDIF
          esta = FILE('MP' +  ;
                 lapmat +  ;
                 '.DBF')
          IF  .NOT. esta
               USE mapocontr
               copy struc to mp&lapmat
          ENDIF
          esta = FILE('AD' +  ;
                 lapmat +  ;
                 '.DBF')
          IF  .NOT. esta
               USE adcontr
               copy struc to ad&lapmat
          ENDIF
          SET SAFETY OFF
          DO scp1006a
     ENDIF
     CLEAR GETS
     EXIT
ENDDO
SET CENTURY ON
RELEASE WINDOW
m.cod1 = SUBSTR(lapmat, 1, 2)
m.cod2 = SUBSTR(lapmat, 3, 4)
m.cod3 = STR(VAL(m.cod2) + 1)
l_actual = m.cod1 + '-' + m.cod2 +  ;
           '/' + RIGHT(lapso, 4)
@ 22, 01 SAY 'Lapso Actual : ' +  ;
  l_actual COLOR W+/B,W+/B 
CLOSE DATABASES
RETURN
*

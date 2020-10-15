SET DEVICE TO SCREEN
SET TALK OFF
SET SAFETY OFF
SET CENTURY ON
CLOSE DATABASES
SELECT 1
USE lapso1
m.lapso = lapso1
CLEAR GETS
SELECT 1
USE datos
SET ORDER TO cedula
SELECT 2
use mp&lapmat
SET ORDER TO ced
DEFINE WINDOW rep FROM 09, 02 TO  ;
       15, 78 SHADOW COLOR SCHEME  ;
       5
ACTIVATE WINDOW rep
titulo3 = 'Reporte de Cumpleaคeros del Mes '
@ 00, 00 SAY SPACE(((WCOLS() -  ;
  LEN(titulo3) - 6) / 2) + 3) +  ;
  titulo3
fich = SPACE(2)
mced = SPACE(12)
fich1 = YEAR(DATE())
SELECT 3
matralu1 = SYS(3)
CREATE CURSOR matralu1 (cedula C  ;
       (12), nombres C (40),  ;
       fechanac D (8), semestre C  ;
       (2), turno C (3))
DO WHILE .T.
     mielecc = 1
     @ 01, 25 SAY  ;
       'Mes de Cumpleaคos: ' GET  ;
       fich PICTURE '99'
     @ 02, 00 TO 2, WCOLS()  ;
       DOUBLE
     READ
     IF READKEY() = 12 .OR. fich =  ;
        SPACE(2)
          CLOSE DATABASES
          RELEASE WINDOW rep
          EXIT
     ENDIF
     IF LASTKEY() = 27
          CLOSE DATABASES
          RELEASE WINDOW
          EXIT
     ENDIF
     @ 3, 20 GET mielecc FUNCTION  ;
       '*NH \<Vista Previa;\<Imprimir;\<Cancelar'  ;
       VALID botones(mielecc)
     READ CYCLE
     EXIT
ENDDO
CLEAR READ
CLOSE DATABASES
RELEASE WINDOW
RETURN
*
FUNCTION botones
PARAMETER btn
IF btn = 3
     CLOSE DATABASES
     RELEASE WINDOW rep
     CLEAR READ
     RETURN .T.
ENDIF
cuenta = 0
SELECT 2
SET ORDER TO ced
GOTO TOP
DO WHILE  .NOT. EOF()
     mced = ced
     SELECT 1
     SEEK mced
     SCATTER MEMO MEMVAR
     IF SUBSTR(DTOC(m.fechanac),  ;
        4, 2) = fich
          SELECT 2
          m.semestre = semestre
          m.turno = turno
          SELECT 3
          INSERT INTO (ALIAS())  ;
                 FROM MEMVAR
          cuenta = cuenta + 1
     ENDIF
     SELECT 2
     SKIP
ENDDO
IF cuenta = 0
     x = msgerro( ;
         'No existe alumnos nacidos para este mes' ;
         )
     RELEASE WINDOW
     CLOSE DATABASES
     CLEAR READ
     _CUROBJ = OBJNUM(fich)
     RETURN
ENDIF
fr = fich
IF fr = '01'
     STORE 'E N E R O' TO mes
ENDIF
IF fr = '02'
     STORE 'F E B R E R O' TO mes
ENDIF
IF fr = '03'
     STORE 'M A R Z O' TO mes
ENDIF
IF fr = '04'
     STORE 'A B R I L' TO mes
ENDIF
IF fr = '05'
     STORE 'M A Y O' TO mes
ENDIF
IF fr = '06'
     STORE 'J U N I O' TO mes
ENDIF
IF fr = '07'
     STORE 'J U L I O' TO mes
ENDIF
IF fr = '08'
     STORE 'A G O S T O' TO mes
ENDIF
IF fr = '09'
     STORE 'S E P T I E M B R E'  ;
           TO mes
ENDIF
IF fr = '10'
     STORE 'O C T U B R E' TO mes
ENDIF
IF fr = '11'
     STORE 'N O V I E M B R E' TO  ;
           mes
ENDIF
IF fr = '12'
     STORE 'D I C I E M B R E' TO  ;
           mes
ENDIF
mes = mes + '  D E  ' +  ;
      LTRIM(STR(fich1))
SELECT 3
INDEX ON DTOC(fechanac) TAG  ;
      matralu1
SET ORDER TO matralu1
IF btn = 1
     archivo = SYS(3) + '.TXT'
     set print to &archivo
ELSE
     p = printer()
     IF p = 2
          RETURN .T.
     ENDIF
ENDIF
pag = 0
li = 89
SET DEVICE TO PRINTER
GOTO TOP
DO WHILE  .NOT. EOF()
     IF li > 58
          m.cuantos = 0
          pag = pag + 1
          @ 00, 00 SAY l_nombre
          @ 00, 58 SAY 'PAGINA: '
          @ 00, PCOL() SAY pag  ;
            PICTURE '9999'
          @ 01, 58 SAY 'FECHA : ' +  ;
            DTOC(DATE())
          @ 01, 00 SAY sistema
          @ 02, 00 SAY titulo3
          titulo4 = 'C U M P L E A ฅ O S  D E L  M E S  D E  ' +  ;
                    mes
          @ 03, 00 SAY SPACE(((80 -  ;
            LEN(titulo4) - 6) /  ;
            2) + 3) + titulo4
          @ 06, 00 SAY  ;
            'ีอออออออออออัออออออออออออออออออออออออออออออออออออออออัออออออออออออัอออออออธ'
          @ 07, 00 SAY  ;
            'ณ   FECHA   ณ APELLIDOS Y NOMBRES                    ณ  SEMESTRE  ณ TURNO ณ'
          @ 08, 00 SAY  ;
            'ฦอออออออออออุออออออออออออออออออออออออออออออออออออออออุออออออออออออุอออออออต'
          li = 09
     ENDIF
     SELECT 3
     @ li, 00 SAY 'ณ'
     @ li, 04 SAY  ;
       SUBSTR(DTOC(fechanac), 1,  ;
       5)
     @ li, 12 SAY 'ณ'
     @ li, 14 SAY nombres PICTURE  ;
       '@!s38'
     @ li, 53 SAY 'ณ'
     @ li, 59 SAY semestre  ;
       PICTURE '@!s2'
     @ li, 66 SAY 'ณ'
     @ li, 69 SAY turno PICTURE  ;
       '@!s3'
     @ li, 74 SAY 'ณ'
     li = li + 1
     SKIP
ENDDO
ha = li + 1
FOR i = li TO ha
     @ li, 00 SAY 'ณ'
     @ li, 12 SAY 'ณ'
     @ li, 53 SAY 'ณ'
     @ li, 66 SAY 'ณ'
     @ li, 74 SAY 'ณ'
     li = li + 1
ENDFOR
@ li, 00 SAY  ;
  'ณ           ณ                                        ณ            ณ       ณ'
li = li + 1
@ li, 00 SAY  ;
  'ิอออออออออออฯออออออออออออออออออออออออออออออออออออออออฯออออออออออออฯอออออออพ'
li = li + 1
@ li, 01 SAY  ;
  'Total Alumnos de Cumpleaคos: '
@ li, PCOL() SAY cuenta PICTURE  ;
  '99999'
li = li + 1
EJECT
SET DEVICE TO SCREEN
SELECT 3
USE
IF btn = 1
     SET PRINTER TO
     SET SYSMENU ON
     KEYBOARD '"{CTRL+F10}'
     modi comm &archivo.txt nomodi
     SET SYSMENU OFF
     dele file &archivo
ELSE
     fich = SPACE(2)
     mced = SPACE(12)
     fich1 = YEAR(DATE())
ENDIF
CLEAR READ
RETURN .T.
*

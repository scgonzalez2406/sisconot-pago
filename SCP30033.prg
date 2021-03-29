CLOSE DATABASES
SET DEVICE TO SCREEN
SELECT 3
USE datos AGAIN ALIAS datos ORDER  ;
    cedula
SELECT 4
use mp&lapmat again alias mp order ced
DEFINE WINDOW sclr FROM 10, 10 TO  ;
       18, 70 FLOAT NOCLOSE  ;
       SHADOW TITLE  ;
       'Listados de Alumnos Cursantes (SCP30003)'  ;
       NOMINIMIZE COLOR SCHEME 5
ACTIVATE WINDOW sclr
m.fecha = DATE()
valida1 = 'ABCDEFGHIJKLMNOPQRSTUVXYZ*'
titulo3 = 'LISTADO GENERAL DE ALUMNOS CURSANTES'
auxilio = LTRIM(STR(VAL(SUBSTR(lapmat,  ;
          3, 4)) + 1))
titulo4 = 'AฅO ESCOLAR: ' +  ;
          SUBSTR(lapmat, 3, 4) +  ;
          '-' + auxilio
ordena = 'Cdula '
SAVE SCREEN TO jol
DO WHILE .T.
     mielecc = 1
     RESTORE SCREEN FROM jol
     @ 1, 1 SAY  ;
       'Fecha Reporte : ' GET  ;
       m.fecha PICTURE '@!'
     @ 2, 1 SAY  ;
       'Ordenado por  : ' GET  ;
       ordena PICTURE  ;
       '@!M Cdula,Nombres'
     @ 4, 1 TO 4, WCOLS() DOUBLE
     READ
     IF LASTKEY() = 27
          RELEASE WINDOW sclr
          CLOSE DATABASES
          RETURN
     ENDIF
     @ 6, 10 GET mielecc FUNCTION  ;
       '*NH \<Vista Previa;\<Imprimir;\<Cancelar'  ;
       VALID botones(mielecc)
     READ CYCLE
ENDDO
CLEAR READ
CLOSE DATABASES
RELEASE WINDOW sclr
RETURN
*
FUNCTION botones
PARAMETER btn
IF btn = 3
     CLEAR READ
     RETURN .T.
ENDIF
SELECT 4
IF ordena = 'Nombres'
     INDEX ON nombres TAG nombres
     SET ORDER TO nombres
ELSE
     SET ORDER TO mced
ENDIF
GOTO TOP
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
m.cuantos = 0
m.num = 0
xtotalv = 0
DO WHILE  .NOT. EOF()
     SELECT 4
     xced = ced
     IF status = 'X'
          SELECT 4
          SKIP
          LOOP
     ENDIF
     SELECT 3
     SEEK xced
     IF  .NOT. FOUND()
          SELECT 4
          SKIP
          LOOP
     ENDIF
     IF li > 58
          m.num = m.num +  ;
                  m.cuantos
          m.cuantos = 0
          pag = pag + 1
          @ 00, 00 SAY l_nombre
          @ 00, 58 SAY 'PAGINA: '
          @ 00, PCOL() SAY pag  ;
            PICTURE '9999'
          @ 01, 58 SAY 'FECHA : ' +  ;
            DTOC(m.fecha)
          @ 01, 00 SAY sistema
          @ 02, 00 SAY SPACE(((43 -  ;
            LEN(titulo3) - 6) /  ;
            2) + 3) + titulo3
          @ 03, 00 SAY SPACE(((43 -  ;
            LEN(titulo4) - 6) /  ;
            2) + 3) + titulo4
          @ 06, 00 SAY  ;
            'ีอออออออออออออัออออออออออออออออออออออออออออออออัอออออออออออออออออออออออออออออออธ'
          @ 07, 00 SAY  ;
            'ณ   CEDULA    ณ APELLIDOS                      ณ NOMBRES                       ณ'
          @ 08, 00 SAY  ;
            'ฦอออออออออออออุออออออออออออออออออออออออออออออออุอออออออออออออออออออออออออออออออต'
          li = 09
     ENDIF
     SELECT 3
     m.apel = SUBSTR(nombres, 1,  ;
              AT(',', nombres, 1) -  ;
              1)
     m.nomb = SUBSTR(nombres,  ;
              AT(',', nombres, 1) +  ;
              1, LEN(nombres))
     m.cuantos = m.cuantos + 1
     @ li, 00 SAY 'ณ'
     @ li, 02 SAY xced PICTURE  ;
       '!-9999999999'
     @ li, 14 SAY 'ณ'
     @ li, 16 SAY m.apel PICTURE  ;
       '@!s28'
     @ li, 47 SAY 'ณ'
     @ li, 49 SAY m.nomb PICTURE  ;
       '@!s27'
     @ li, 79 SAY 'ณ'
     li = li + 1
     IF li > 58
          @ li, 00 SAY  ;
            'ิอออออออออออออฯออออออออออออออออออออออออออออออออฯอออออออออออออออออออออออออออออออพ'
     ENDIF
     SELECT 4
     SKIP
ENDDO
ha = li + 1
FOR i = li TO ha
     @ li, 00 SAY 'ณ'
     @ li, 14 SAY 'ณ'
     @ li, 47 SAY 'ณ'
     @ li, 79 SAY 'ณ'
ENDFOR
li = li + 1
@ li, 00 SAY  ;
  'ิอออออออออออออฯออออออออออออออออออออออออออออออออฯอออออออออออออออออออออออออออออออพ'
li = li + 1
m.num = m.num + m.cuantos
@ li, 01 SAY  ;
  'Total Alumnos cursantes: '
@ li, PCOL() SAY m.num PICTURE  ;
  '99999'
@ li, 00 SAY CHR(18) + CHR(20) +  ;
  '  '
EJECT
SET DEVICE TO SCREEN
IF btn = 1
     SET PRINTER TO
     SET SYSMENU ON
     KEYBOARD '"{CTRL+F10}'
     modi comm &archivo.txt nomodi
     SET SYSMENU OFF
     dele file &archivo
ELSE
     m.fecha = DATE()
ENDIF
CLEAR READ
RETURN .T.
*

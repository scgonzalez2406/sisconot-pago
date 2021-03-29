CLOSE DATABASES
SET DEVICE TO SCREEN
SELECT 1
USE datos AGAIN ALIAS datos ORDER  ;
    cedula
SELECT 2
use mp&lapmat again alias mp order cusetuce
DEFINE WINDOW sclr FROM 10, 10 TO  ;
       19, 70 FLOAT NOCLOSE  ;
       SHADOW TITLE  ;
       'Listados de Alumnos por Semestre (SCP30011)'  ;
       NOMINIMIZE COLOR SCHEME 5
ACTIVATE WINDOW sclr
m.cedula = SPACE(12)
m.tur = SPACE(3)
m.curso1 = '32012'
m.semestre = SPACE(2)
m.fecha = DATE()
m.codigo = SPACE(2)
m.seccion = SPACE(1)
m.turno = SPACE(3)
m.curso = '32012'
valida1 = 'ABCDEFGHIJKLMNOPQRSTUVXYZ'
titulo3 = 'LISTADOS DE ALUMNOS POR SEMESTRE'
SAVE SCREEN TO jol
raya = CHR(27) + CHR(45) + '1'
noraya = CHR(27) + CHR(45) + '0'
ordena = 'Cdula '
DO WHILE .T.
     mielecc = 1
     RESTORE SCREEN FROM jol
     @ 1, 1 SAY  ;
       'Fecha Reporte : ' GET  ;
       m.fecha PICTURE '@!'
     @ 2, 1 SAY  ;
       'Menciขn       : ' GET  ;
       m.curso DEFAULT '32012'  ;
       PICTURE  ;
       '@!M 32012,31022,31023,31000,30000'
     @ 3, 1 SAY  ;
       'Semestre      : '
     @ 4, 1 SAY  ;
       'Turno         : '
     @ 5, 1 SAY  ;
       'Ordenado por  : '
     @ 6, 1 TO 6, WCOLS() DOUBLE
     READ
     IF LASTKEY() = 27
          RELEASE WINDOW sclr
          CLOSE DATABASES
          RETURN
     ENDIF
     DO CASE
          CASE m.curso = '32012'
               m.mencion = 'BASICA'
          CASE m.curso = '31022'
               m.mencion = 'CIENCIAS'
          CASE m.curso = '31023'
               m.mencion = 'HUMANIDADES'
     ENDCASE
     @ 2, 28 SAY m.mencion
     IF m.curso = '32012'
          @ 3, 1 SAY  ;
            'Semestre      : '  ;
            GET m.codigo DEFAULT  ;
            '07' PICTURE  ;
            '@!M 07,08,09,10,11,12'
     ELSE
          @ 3, 1 SAY  ;
            'Semestre      : '  ;
            GET m.codigo DEFAULT  ;
            '01' PICTURE  ;
            '@!M 01,02,03,04'
     ENDIF
     @ 4, 1 SAY  ;
       'Turno         : ' GET  ;
       m.turno DEFAULT 'MAT'  ;
       PICTURE  ;
       '@!M MAT,VES,NOC,SAB'
     @ 5, 1 SAY  ;
       'Ordenado por  : ' GET  ;
       ordena PICTURE  ;
       '@!M Cdula,Nombres'
     @ 7, 10 GET mielecc FUNCTION  ;
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
     CLEAR READ
     RETURN .T.
ENDIF
IF LASTKEY() = 27
     RELEASE WINDOW sclr
     CLOSE DATABASES
     RETURN
ENDIF
SELECT 2
SET ORDER TO cusetuce
SEEK m.curso + m.codigo + m.turno
IF  .NOT. FOUND()
     = msgerro( ;
       'No existen alumnos asignados a este semestre, Verifique' ;
       )
     RELEASE WINDOW sclr
     CLOSE DATABASES
     RETURN
ENDIF
SELECT 3
matralu1 = SYS(3)
CREATE CURSOR matralu1 (cedula C  ;
       (12), nombres C (45),  ;
       curso1 C (5), semestre C  ;
       (2), tur C (3))
SELECT 2
DO WHILE  .NOT. EOF()
     m.cedula = ced
     m.tur = turno
     m.curso1 = cur
     m.semestre = semestre
     IF cur <> m.curso
          SELECT 2
          SKIP
          LOOP
     ENDIF
     IF m.semestre <> m.codigo
          SELECT 2
          SKIP
          LOOP
     ENDIF
     IF turno <> m.turno
          SELECT 2
          SKIP
          LOOP
     ENDIF
     SELECT 1
     SET ORDER TO cedula
     IF  .NOT. SEEK(m.cedula)
          SELECT 2
          SKIP
     ENDIF
     SELECT 1
     m.nombres = nombres
     SELECT 3
     INSERT INTO (ALIAS()) FROM  ;
            MEMVAR
     SELECT 2
     SKIP
ENDDO
DO WHILE .T.
     SELECT 3
     IF FLOCK()
          IF ordena = 'Nombres'
               INDEX ON nombres  ;
                     TAG  ;
                     matralu1
          ELSE
               INDEX ON  ;
                     VAL(SUBSTR(cedula,  ;
                     3, 10)) TAG  ;
                     matralu1
          ENDIF
          UNLOCK
          EXIT
     ENDIF
ENDDO
SELECT 3
SET ORDER TO matralu1
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
SELECT 3
m.cuantos = 0
DO WHILE  .NOT. EOF()
     IF li > 58
          m.cuantos = 0
          pag = pag + 1
          @ 00, 00 SAY l_nombre
          @ 00, 58 SAY 'PAGINA: '
          @ 00, PCOL() SAY pag  ;
            PICTURE '9999'
          @ 01, 58 SAY 'FECHA : ' +  ;
            DTOC(m.fecha)
          @ 01, 00 SAY sistema
          @ 02, 00 SAY titulo3
          @ 03, 00 SAY  ;
            'MENCION : ' +  ;
            m.mencion
          @ 04, 00 SAY  ;
            'SEMESTRE : ' +  ;
            m.codigo
          @ 05, 00 SAY 'TURNO : ' +  ;
            m.turno
          @ 06, 00 SAY  ;
            'ีออออัออออออออออัอออออออออออออออออออออออออออออออัออออออออออออออออออออออออออออออธ'
          @ 07, 00 SAY  ;
            'ณNro.ณ CEDULA   ณ APELLIDOS                     ณ NOMBRES                      ณ'
          @ 08, 00 SAY  ;
            'ฦออออุออออออออออุอออออออออออออออออออออออออออออออุออออออออออออออออออออออออออออออต'
          li = 09
     ENDIF
     SELECT 3
     m.ced = cedula
     SELECT 1
     SEEK m.ced
     m.apel = SUBSTR(nombres, 1,  ;
              AT(',', nombres, 1) -  ;
              1)
     m.nomb = SUBSTR(nombres,  ;
              AT(',', nombres, 1) +  ;
              1, LEN(nombres))
     m.cuantos = m.cuantos + 1
     m.num = IIF(m.cuantos <= 9,  ;
             '0' +  ;
             LTRIM(STR(m.cuantos)),  ;
             LTRIM(STR(m.cuantos)))
     @ li, 00 SAY 'ณ'
     @ li, 02 SAY m.num PICTURE  ;
       '99'
     @ li, 05 SAY 'ณ'
     @ li, 06 SAY m.ced PICTURE  ;
       '!-99999999'
     @ li, 16 SAY 'ณ'
     @ li, 18 SAY m.apel PICTURE  ;
       '@!s28'
     @ li, 48 SAY 'ณ'
     @ li, 50 SAY m.nomb PICTURE  ;
       '@!s27'
     @ li, 79 SAY 'ณ'
     li = li + 1
     SELECT 3
     SKIP
     IF li > 58
          @ li, 00 SAY  ;
            'ิออออฯออออออออออฯอออออออออออออออออออออออออออออออฯออออออออออออออออออออออออออออออพ'
          li = li + 1
     ENDIF
ENDDO
ha = li + 1
FOR i = li TO ha
     @ li, 00 SAY 'ณ'
     @ li, 05 SAY 'ณ'
     @ li, 16 SAY 'ณ'
     @ li, 48 SAY 'ณ'
     @ li, 79 SAY 'ณ'
     li = li + 1
ENDFOR
@ li, 00 SAY  ;
  'ณ    ณ          ณ                               ณ                              ณ'
li = li + 1
@ li, 00 SAY  ;
  'ิออออฯออออออออออฯอออออออออออออออออออออออออออออออฯออออออออออออออออออออออออออออออพ'
li = li + 1
@ li, 00 SAY CHR(18) + CHR(20) +  ;
  '  '
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
     m.fecha = DATE()
     m.codigo = SPACE(2)
     m.seccion = SPACE(1)
     m.turno = SPACE(3)
     m.curso = '32012'
ENDIF
CLEAR READ
RETURN .T.
*

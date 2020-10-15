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
DEFINE WINDOW rep FROM 08, 10 TO  ;
       16, 70 FLOAT NOCLOSE  ;
       SHADOW TITLE  ;
       'Aviso General de Cobro (SCP20007)'  ;
       NOMINIMIZE COLOR SCHEME 5
MOVE WINDOW rep CENTER
ACTIVATE WINDOW rep
fich = SPACE(2)
mced = SPACE(12)
fich1 = YEAR(DATE())
m.cedula = SPACE(12)
m.tur = SPACE(3)
m.curso1 = '32012'
m.semestre = SPACE(2)
m.fecha = DATE()
m.codigo = SPACE(2)
m.seccion = SPACE(1)
m.turno = SPACE(3)
m.curso = '32012'
m.equivale = SPACE(12)
m.semetre = '                '
m.mencion = '                '
ng = CHR(27) + CHR(69)
ngn = CHR(27) + CHR(70)
@ 01, 10 SAY  ;
  'Mes y A¤o de Aviso de Cobro: '  ;
  GET fich PICTURE '99'
@ 01, 42 SAY '-'
@ 01, 43 GET fich1 PICTURE '9999'
@ 02, 10 SAY 'Fecha Reporte : '  ;
  GET m.fecha PICTURE '@!'
@ 03, 10 SAY 'Menci¢n       : '  ;
  GET m.curso DEFAULT '32012'  ;
  PICTURE  ;
  '@!M 32012,31022,31023,31000,30000'
@ 04, 10 SAY 'Semestre      : '
@ 05, 10 SAY 'Turno         : '
READ
DO CASE
     CASE m.curso = '32012'
          m.mencion = 'BASICA'
     CASE m.curso = '31022'
          m.mencion = 'CIENCIAS'
     CASE m.curso = '31023'
          m.mencion = 'HUMANIDADES'
ENDCASE
@ 03, 34 SAY m.mencion
IF m.curso = '32012'
     @ 04, 10 SAY  ;
       'Semestre      : ' GET  ;
       m.codigo DEFAULT '07'  ;
       PICTURE  ;
       '@!M 07,08,09,10,11,12'
ELSE
     @ 04, 10 SAY  ;
       'Semestre      : ' GET  ;
       m.codigo DEFAULT '01'  ;
       PICTURE '@!M 01,02,03,04'
ENDIF
@ 05, 10 SAY 'Turno         : '  ;
  GET m.turno DEFAULT 'MAT'  ;
  PICTURE '@!M MAT,VES,NOC'
READ
IF READKEY() = 12 .OR. fich =  ;
   SPACE(2)
     CLOSE DATABASES
     RELEASE WINDOW
     RETURN
ENDIF
IF LASTKEY() = 27
     CLOSE DATABASES
     RELEASE WINDOW
     RETURN
ENDIF
fr = fich
IF fr = '01'
     STORE 'ENERO' TO mes
ENDIF
IF fr = '02'
     STORE 'FEBRERO' TO mes
ENDIF
IF fr = '03'
     STORE 'MARZO' TO mes
ENDIF
IF fr = '04'
     STORE 'ABRIL' TO mes
ENDIF
IF fr = '05'
     STORE 'MAYO' TO mes
ENDIF
IF fr = '06'
     STORE 'JUNIO' TO mes
ENDIF
IF fr = '07'
     STORE 'JULIO' TO mes
ENDIF
IF fr = '08'
     STORE 'AGOSTO' TO mes
ENDIF
IF fr = '09'
     STORE 'SEPTIEMBRE' TO mes
ENDIF
IF fr = '10'
     STORE 'OCTUBRE' TO mes
ENDIF
IF fr = '11'
     STORE 'NOVIEMBRE' TO mes
ENDIF
IF fr = '12'
     STORE 'DICIEMBRE' TO mes
ENDIF
mes = mes + ' DE ' +  ;
      LTRIM(STR(fich1))
DO CASE
     CASE m.curso = '32012'
          IF VAL(m.codigo) = 07
               m.semetre = 'SEPTIMO SEMESTRE'
               m.equivale = '7mo. GRADO'
          ENDIF
          IF VAL(m.codigo) = 08
               m.semetre = 'OCTAVO SEMESTRE'
               m.equivale = '7mo. GRADO'
          ENDIF
          IF VAL(m.codigo) = 09
               m.semetre = 'NOVENO SEMESTRE'
               m.equivale = '8vo. GRADO'
          ENDIF
          IF VAL(m.codigo) = 10
               m.semetre = 'DECIMO SEMESTRE'
               m.equivale = '8vo. GRADO'
          ENDIF
          IF VAL(m.codigo) = 11
               m.semetre = 'DECIMO PRIMER SEMESTRE'
               m.equivale = '9no. GRADO'
          ENDIF
          IF VAL(m.codigo) = 12
               m.semetre = 'DECIMO SEGUNDO SEMESTRE'
               m.equivale = '9no. GRADO'
          ENDIF
     CASE m.curso = '31022' .OR.  ;
          m.curso = '31023'
          IF VAL(m.codigo) = 01
               m.semetre = 'PRIMER SEMESTRE'
               m.equivale = '1er. A¥O'
          ENDIF
          IF VAL(m.codigo) = 02
               m.semetre = 'SEGUNDO SEMESTRE'
               m.equivale = '1er. A¥O'
          ENDIF
          IF VAL(m.codigo) = 03
               m.semetre = 'TERCER SEMESTRE'
               m.equivale = '2do. A¥O'
          ENDIF
          IF VAL(m.codigo) = 04
               m.semetre = 'CUARTO SEMESTRE'
               m.equivale = '2do. A¥O'
          ENDIF
ENDCASE
cuenta = 0
p = printer()
IF p = 2
     CLOSE DATABASES
     RELEASE WINDOW
     RETURN
ENDIF
pag = 0
li = 89
SET DEVICE TO PRINTER
y3 = ALLTRIM(l_nombre)
y4 = ALLTRIM(l_localida)
y5 = ALLTRIM(l_direccio)
y6 = ALLTRIM(l_telefono)
@ 00, 00 SAY CHR(18) + CHR(20) +  ;
  ' ' + ng
@ 01, 00 SAY SPACE(((80 - LEN(y3) -  ;
  6) / 2) + 3) + y3
@ 02, 00 SAY SPACE(((80 - LEN(y4) -  ;
  6) / 2) + 3) + y4
@ 03, 00 SAY SPACE(((80 - LEN(y5) -  ;
  6) / 2) + 3) + y5
@ 04, 00 SAY SPACE(((80 - LEN(y6) -  ;
  6) / 2) + 3) + y6
centro1 = 'AVISO GENERAL DE COBRO'
@ 07, 00 SAY ng + SPACE(((80 -  ;
  LEN(centro1) - 6) / 2) + 3) +  ;
  centro1 + ngn
@ 10, 05 SAY  ;
  '   Por  medio  de  la  presente  se  le  participa a los representantes'
DO CASE
     CASE m.curso = '32012'
          @ 12, 05 SAY  ;
            'de   los    Alumnos  (as)   que   cursan   el  ________________________'
          @ 12, 53 SAY m.semetre
          @ 14, 05 SAY  ;
            '(_____________ de Educaci¢n B sica),  que  deben cancelar a la brevedad'
          @ 14, 07 SAY m.equivale
          @ 16, 05 SAY  ;
            'posible la Mensualidad correspondiente al mes de _____________________.'
          @ 16, 56 SAY mes
     CASE m.curso = '31022' .OR.  ;
          m.curso = '31023'
          @ 12, 05 SAY  ;
            'de   los    Alumnos  (as)   que   cursan   el  ________________________'
          @ 12, 53 SAY m.semetre
          @ 14, 05 SAY  ;
            '(_____________ Educ. Media Diversificada),que deben cancelar  lo  antes'
          @ 14, 07 SAY m.equivale
          @ 16, 05 SAY  ;
            'posible la Mensualidad correspondiente al mes de _____________________.'
          @ 16, 56 SAY mes
ENDCASE
EJECT
SET DEVICE TO SCREEN
CLEAR READ
CLOSE DATABASES
RELEASE WINDOW
RETURN
*

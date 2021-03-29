CLOSE DATABASES
SET DEVICE TO SCREEN
SELECT 1
USE datos AGAIN ALIAS datos ORDER  ;
    cedula
SELECT 2
use mp&lapmat again alias mp order cusetuce
SELECT 3
USE plancobr AGAIN ALIAS plancobr  ;
    ORDER lapcod
SELECT 4
USE detacobr AGAIN ALIAS detacobr  ;
    ORDER lapcod
DEFINE WINDOW sclr FROM 09, 10 TO  ;
       20, 70 FLOAT NOCLOSE  ;
       SHADOW TITLE  ;
       'Relaciขn de Morosidad  (SCP30555)'  ;
       NOMINIMIZE COLOR SCHEME 5
ACTIVATE WINDOW sclr
m.fecha = DATE()
m.codigo = SPACE(2)
m.seccion = SPACE(1)
m.turno = SPACE(3)
m.curso = '32012'
valida1 = 'ABCDEFGHIJKLMNOPQRSTUVXYZ'
titulo3 = 'LISTADOS DE ALUMNOS MOROSOS POR SEMESTRE'
SAVE SCREEN TO jol
raya = CHR(27) + CHR(45) + '1'
noraya = CHR(27) + CHR(45) + '0'
ordena = 'Cdula '
STORE DATE() TO m.fecha1,  ;
      m.fecha2
DO WHILE .T.
     mielecc = 1
     RESTORE SCREEN FROM jol
     @ 1, 1 SAY  ;
       'Fecha Desde   : ' GET  ;
       m.fecha1 PICTURE '@E'
     @ 2, 1 SAY  ;
       'Fecha Hasta   : ' GET  ;
       m.fecha2 PICTURE '@E'
     @ 3, 1 SAY  ;
       'Menciขn       : ' GET  ;
       m.curso DEFAULT '32012'  ;
       PICTURE  ;
       '@!M 32012,31022,31023,31000,30000'
     @ 4, 1 SAY  ;
       'Semestre      : '
     @ 5, 1 SAY  ;
       'Turno         : '
     @ 6, 1 SAY  ;
       'Ordenado por  : '
     @ 7, 1 TO 7, WCOLS() DOUBLE
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          RELEASE WINDOW
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
     @ 3, 28 SAY m.mencion
     IF m.curso = '32012'
          @ 4, 1 SAY  ;
            'Semestre      : '  ;
            GET m.codigo DEFAULT  ;
            '07' PICTURE  ;
            '@!M 07,08,09,10,11,12'
     ELSE
          @ 4, 1 SAY  ;
            'Semestre      : '  ;
            GET m.codigo DEFAULT  ;
            '01' PICTURE  ;
            '@!M 01,02,03,04'
     ENDIF
     @ 5, 1 SAY  ;
       'Turno         : ' GET  ;
       m.turno DEFAULT 'MAT'  ;
       PICTURE  ;
       '@!M MAT,VES,NOC,SAB'
     @ 6, 1 SAY  ;
       'Ordenado por  : ' GET  ;
       ordena PICTURE  ;
       '@!M Cdula,Nombres'
     @ 8, 10 GET mielecc FUNCTION  ;
       '*NH \<Vista Previa;\<Imprimir;\<Cancelar'  ;
       VALID botones(mielecc)
     READ CYCLE
     m.fecha = DATE()
     m.codigo = SPACE(2)
     m.seccion = SPACE(1)
     m.turno = SPACE(3)
     m.curso = '32012'
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
IF m.fecha1 > m.fecha2
     = msgerro( ;
       'El Lapso para el analisis es invalido, verifique' ;
       )
     RETURN .F.
ENDIF
IF m.fecha2 < m.fecha1
     = msgerro( ;
       'El Lapso para el analisis es invalido, verifique' ;
       )
     RETURN .F.
ENDIF
SELECT 2
SET ORDER TO cusetuce
SEEK m.curso + m.codigo + m.turno
IF  .NOT. FOUND()
     = msgerro( ;
       'No existen alumnos asignados a este semestre, Verifique' ;
       )
     RETURN .F.
ENDIF
SELECT 5
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
     SELECT 5
     INSERT INTO (ALIAS()) FROM  ;
            MEMVAR
     SELECT 2
     SKIP
ENDDO
SELECT 5
IF ordena = 'Nombres'
     INDEX ON nombres TAG  ;
           matralu1
ELSE
     INDEX ON VAL(SUBSTR(cedula,  ;
           3, 10)) TAG matralu1
ENDIF
SELECT 5
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
m.cuantos = 0
m.sec = m.codigo
xtotalv = 0
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
          titulo4 = 'Desde : ' +  ;
                    DTOC(m.fecha1) +  ;
                    ' Hasta : ' +  ;
                    DTOC(m.fecha2)
          @ 03, 00 SAY SPACE(((80 -  ;
            LEN(titulo4) - 6) /  ;
            2) + 3) + titulo4
          @ 04, 00 SAY  ;
            'SEMESTRE : ' +  ;
            m.codigo
          @ 05, 00 SAY 'TURNO : ' +  ;
            m.turno
          @ 06, 00 SAY  ;
            'ีออออัออออออออออัออออออออออออออออออออออออออออออออออออออออออออัอออออออออออออออออธ'
          @ 07, 00 SAY  ;
            'ณNro.ณ CEDULA   ณ APELLIDOS Y NOMBRES                        ณ DEUDA PENDIENTE ณ'
          @ 08, 00 SAY  ;
            'ฦออออุออออออออออุออออออออออออออออออออออออออออออออออออออออออออุอออออออออออออออออต'
          li = 09
     ENDIF
     m.ced = cedula
     SELECT 2
     SET ORDER TO ced
     SEEK m.ced
     m.plancobro = plancobro
     SELECT 3
     SEEK lapmat + m.plancobro
     xnro = nrocuotas
     SAVE SCREEN TO hol
     SELECT 2
     ii = 1
     m.montov = 0.00 
     FOR ii = 1 TO xnro
          lk = LTRIM(STR(ii))
          if cuota&lk=pagado&lk
               LOOP
          ENDIF
          SELECT 4
          SET ORDER TO codcuota
          LOCATE FOR lapso =  ;
                 lapmat .AND.  ;
                 codigo =  ;
                 m.plancobro  ;
                 .AND. cuota =  ;
                 ii
          IF fecha_ven >=  ;
             m.fecha1 .AND.  ;
             fecha_ven <=  ;
             m.fecha2
               SELECT 2
               m.montov=m.montov+(cuota&lk-pagado&lk)
          ENDIF
          SELECT 2
     ENDFOR
     IF m.montov = 0
          SELECT 5
          SKIP
          LOOP
     ENDIF
     xtotalv = xtotalv + m.montov
     SELECT 5
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
     @ li, 18 SAY nombres PICTURE  ;
       '@!s42'
     @ li, 61 SAY 'ณ'
     @ li, 64 SAY m.montov  ;
       PICTURE '99,999,999.99'
     @ li, 79 SAY 'ณ'
     li = li + 1
     SELECT 5
     SKIP
ENDDO
ha = li + 1
FOR i = li TO ha
     @ li, 00 SAY 'ณ'
     @ li, 05 SAY 'ณ'
     @ li, 16 SAY 'ณ'
     @ li, 61 SAY 'ณ'
     @ li, 79 SAY 'ณ'
     li = li + 1
ENDFOR
@ li, 00 SAY  ;
  'ณ    ณ          ณ                                            ณ                 ณ'
li = li + 1
@ li, 00 SAY  ;
  'ิออออฯออออออออออฯออออออออออออออออออออออออออออออออออออออออออออฯอออออออออออออออออพ'
li = li + 1
@ li, 01 SAY  ;
  'Total Alumnos Morosos: '
@ li, PCOL() SAY m.cuantos  ;
  PICTURE '99999'
li = li + 1
@ li, 01 SAY  ;
  'Total Deuda Morosa ->: '
@ li, PCOL() SAY xtotalv PICTURE  ;
  '99,999,999.99'
@ li, 00 SAY CHR(18) + CHR(20) +  ;
  '  '
EJECT
SET DEVICE TO SCREEN
SELECT 5
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

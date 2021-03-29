CLOSE DATABASES
SET DEVICE TO SCREEN
SELECT 2
USE asignatu AGAIN ALIAS asignatu  ;
    ORDER codigo
SELECT 3
USE datos AGAIN ALIAS datos ORDER  ;
    cedula
SELECT 4
USE predatos AGAIN ALIAS predatos  ;
    ORDER cedula
DEFINE WINDOW sclr FROM 10, 10 TO  ;
       17, 70 FLOAT NOCLOSE  ;
       SHADOW TITLE  ;
       'Listados de Alumnos Pre-Inscrito'  ;
       NOMINIMIZE COLOR SCHEME 5
ACTIVATE WINDOW sclr
m.fecha = DATE()
m.codigo = SPACE(1)
m.seccion = SPACE(1)
m.curso = '32011'
valida1 = 'ABCDEFGHIJKLMNOPQRSTUVXYZ*'
titulo3 = 'LISTADO GENERAL DE PRE-INSCRITOS POR ESPECIALIDAD'
titulo4 = 'AฅO ESCOLAR: ' +  ;
          SUBSTR(lapmat, 1, 2) +  ;
          '-' + SUBSTR(lapmat, 3,  ;
          2)
SAVE SCREEN TO jol
DO WHILE .T.
     RESTORE SCREEN FROM jol
     @ 1, 1 SAY  ;
       'Fecha Reporte : ' GET  ;
       m.fecha PICTURE '@!'
     @ 2, 1 SAY  ;
       'Menciขn       : ' GET  ;
       m.curso DEFAULT '31022'  ;
       PICTURE  ;
       '@!M 32012,31022,31023'
     @ 4, 1 TO 4, WCOLS() DOUBLE
     @ 5, 1 SAY  ;
       'Presione Esc para cancelar la operaciขn'
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
     p = printer()
     IF p = 2
          RELEASE WINDOW sclr
          CLOSE DATABASES
          RETURN
     ENDIF
     SELECT 4
     SET ORDER TO curced
     GOTO TOP
     pag = 0
     li = 89
     SET DEVICE TO PRINTER
     m.cuantos = 0
     DO WHILE  .NOT. EOF()
          SELECT 4
          IF cur <> m.curso
               SELECT 4
               SKIP
               LOOP
          ENDIF
          xced = cedula
          SELECT 3
          SEEK xced
          IF  .NOT. FOUND()
               SELECT 4
               SKIP
               LOOP
          ENDIF
          IF li > 55
               pag = pag + 1
               @ 00, 00 SAY  ;
                 CHR(18) +  ;
                 CHR(20) + '  '
               @ 00, 00 SAY  ;
                 CHR(14) +  ;
                 CHR(15) +  ;
                 l_nombre
               @ 00, 00 SAY  ;
                 CHR(18) +  ;
                 CHR(20) + '  '
               @ 00, 62 SAY  ;
                 'PAGINA: '
               @ 00, PCOL() SAY  ;
                 pag PICTURE  ;
                 '9999'
               @ 01, 60 SAY  ;
                 'FECHA : ' +  ;
                 DTOC(m.fecha)
               @ 01, 00 SAY  ;
                 CHR(15) +  ;
                 sistema +  ;
                 CHR(18) +  ;
                 CHR(20) + ' '
               @ 02, 00 SAY  ;
                 CHR(14) +  ;
                 CHR(15)
               @ 02, 18 SAY  ;
                 titulo3
               @ 03, 00 SAY  ;
                 CHR(14) +  ;
                 CHR(15)
               @ 03, 00 SAY  ;
                 SPACE(((43 -  ;
                 LEN(titulo4) -  ;
                 6) / 2) + 3) +  ;
                 titulo4
               @ 04, 00 SAY  ;
                 CHR(18) +  ;
                 CHR(20) + ' '
               @ 05, 01 SAY  ;
                 'MENCION: ' +  ;
                 m.curso + '  ' +  ;
                 m.mencion
               @ 06, 00 SAY  ;
                 'ีออออออออออออัออออออออออออออออออออออออออออออออัอออออออออออออออออออออออออออออออธ'
               @ 07, 00 SAY  ;
                 'ณ CEDULA     ณ APELLIDOS                      ณ NOMBRES                       ณ'
               @ 08, 00 SAY  ;
                 'ิออออออออออออฯออออออออออออออออออออออออออออออออฯอออออออออออออออออออออออออออออออพ'
               li = 09
          ENDIF
          SELECT 3
          m.apel = SUBSTR(nombres,  ;
                   1, AT(',',  ;
                   nombres, 1) -  ;
                   1)
          m.nomb = SUBSTR(nombres,  ;
                   AT(',',  ;
                   nombres, 1) +  ;
                   1,  ;
                   LEN(nombres))
          m.cuantos = m.cuantos +  ;
                      1
          @ li, 02 SAY xced  ;
            PICTURE  ;
            '!-9999999999'
          @ li, 15 SAY m.apel  ;
            PICTURE '@!s28'
          @ li, 48 SAY m.nomb  ;
            PICTURE '@!s27'
          li = li + 1
          IF li > 55
               @ li, 01 SAY  ;
                 REPLICATE('-',  ;
                 79)
          ENDIF
          SELECT 4
          SKIP
     ENDDO
     @ li, 01 SAY REPLICATE('-',  ;
       79)
     li = li + 1
     @ li, 01 SAY  ;
       'Total Alumnos Pre-Inscritos: '
     @ li, PCOL() SAY m.cuantos  ;
       PICTURE '99999'
     li = li + 1
     @ li, 00 SAY CHR(18) +  ;
       CHR(20) + '  '
     SET DEVICE TO SCREEN
     m.fecha = DATE()
ENDDO
CLOSE DATABASES
RELEASE WINDOW sclr
RETURN
*

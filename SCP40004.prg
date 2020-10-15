CLOSE DATABASES
SET DEVICE TO SCREEN
SELECT 3
USE datos AGAIN ALIAS datos ORDER  ;
    cedula
SELECT 4
use mapo&lapmat again alias mapo order;
sec
DEFINE WINDOW sclr FROM 10, 10 TO  ;
       17, 70 FLOAT NOCLOSE  ;
       SHADOW TITLE  ;
       'Listados de Alumnos Vs. Familias (SCP40004)'  ;
       NOMINIMIZE COLOR SCHEME 5
ACTIVATE WINDOW sclr
m.fecha = DATE()
m.codigo = SPACE(1)
m.seccion = SPACE(1)
m.curso = '32011'
valida1 = 'ABCDEFGHIJKLMNOPQRSTUVXYZ'
titulo3 = 'LISTADOS DE ALUMNOS VS. FAMILIAS'
SAVE SCREEN TO jol
raya = CHR(27) + CHR(45) + '1'
noraya = CHR(27) + CHR(45) + '0'
DO WHILE .T.
     RESTORE SCREEN FROM jol
     @ 1, 1 SAY  ;
       'Fecha Reporte : ' GET  ;
       m.fecha PICTURE '@!'
     @ 2, 1 SAY  ;
       'Menci¢n       : ' GET  ;
       m.curso DEFAULT '31022'  ;
       PICTURE  ;
       '@!M 32011,31018,31019,31000,30000'
     @ 4, 1 TO 4, WCOLS() DOUBLE
     @ 5, 1 SAY  ;
       'Presione Esc para cancelar la operaci¢n'
     READ
     IF LASTKEY() = 27
          RELEASE WINDOW sclr
          CLOSE DATABASES
          RETURN
     ENDIF
     DO CASE
          CASE m.curso = '32011'
               m.mencion = 'BASICA'
          CASE m.curso = '31018'
               m.mencion = 'CIENCIAS'
          CASE m.curso = '31019'
               m.mencion = 'HUMANIDADES'
          CASE m.curso = '31000'
               m.mencion = 'PRIMARIA'
          CASE m.curso = '30000'
               m.mencion = 'PRE-ESCOLAR'
     ENDCASE
     @ 2, 28 SAY m.mencion
     p = printer()
     IF p = 2
          RELEASE WINDOW sclr
          CLOSE DATABASES
          RETURN
     ENDIF
     SELECT 5
     SELECT DISTINCT mapo.*,  ;
            datos.* FROM mapo,  ;
            datos WHERE mapo.ced =  ;
            datos.cedula AND  ;
            mapo.cur = m.curso  ;
            AND mapo.status <>  ;
            'X' INTO DBF  ;
            matralu1
     USE
     USE matralu1
     DO WHILE .T.
          IF FLOCK()
               INDEX ON  ;
                     SUBSTR(rep_legal,  ;
                     3, 10) +  ;
                     cedula TO  ;
                     matralu1
               UNLOCK
               EXIT
          ENDIF
     ENDDO
     USE
     SELECT 5
     USE matralu1 INDEX matralu1
     pag = 0
     li = 89
     SET DEVICE TO PRINTER
     m.cuantos = 0
     m.sec = m.codigo + m.seccion
     swp = 0
     DO WHILE  .NOT. EOF()
          SELECT 5
          IF cur <> m.curso
               SELECT 5
               SKIP
               LOOP
          ENDIF
          IF status = 'X'
               SELECT 5
               SKIP
               LOOP
          ENDIF
          IF li > 52
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
               @ 02, 00 SAY  ;
                 titulo3
               @ 02, 00 SAY  ;
                 CHR(18) +  ;
                 CHR(20) + ' '
               @ 03, 00 SAY  ;
                 'MENCION : ' +  ;
                 m.mencion
               @ 05, 01 SAY  ;
                 '==============================================================================='
               @ 06, 01 SAY  ;
                 ' REPRESENTANTE           CEDULA      ALUMNOS                            GR./SE.'
               @ 07, 01 SAY  ;
                 '-------------------------------------------------------------------------------'
               li = 08
               IF pag > 1
                    @ li, 01 SAY  ;
                      xrep  ;
                      PICTURE  ;
                      '@!'
                    @ li, PCOL() +  ;
                      2 SAY  ;
                      xrep1
                    li = li + 1
               ENDIF
          ENDIF
          IF swp = 0
               xrep = rep_legal
               xrep1 = representa
               @ li, 01 SAY xrep  ;
                 PICTURE '@!'
               @ li, PCOL() + 2  ;
                 SAY xrep1
               li = li + 1
               swp = 1
          ENDIF
          IF xrep <> rep_legal
               @ li, 01 SAY  ;
                 REPLICATE('-',  ;
                 77)
               li = li + 1
               xrep = rep_legal
               xrep1 = representa
               @ li, 01 SAY xrep  ;
                 PICTURE '@!'
               @ li, PCOL() + 2  ;
                 SAY xrep1
               li = li + 1
          ENDIF
          @ li, 24 SAY cedula  ;
            PICTURE '@!'
          @ li, 37 SAY nombres  ;
            PICTURE '@!S30'
          @ li, 74 SAY seccionr  ;
            PICTURE '@!'
          li = li + 1
          IF li > 55
               @ li, 01 SAY  ;
                 REPLICATE('-',  ;
                 77)
          ENDIF
          SKIP
     ENDDO
     @ li, 01 SAY REPLICATE('-',  ;
       77)
     li = li + 1
     SET DEVICE TO SCREEN
     m.fecha = DATE()
ENDDO
CLOSE DATABASES
RELEASE WINDOW
RETURN
*

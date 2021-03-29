CLOSE DATABASES
SET DEVICE TO SCREEN
SELECT 3
USE datos AGAIN ALIAS datos ORDER  ;
    cedula
SELECT 4
use mapo&lapmat again alias mapo order;
sec
DEFINE WINDOW sclr FROM 10, 10 TO  ;
       16, 70 FLOAT NOCLOSE  ;
       SHADOW TITLE  ;
       'Listados de Alumnos Vs. Familias (SCP4004a)'  ;
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
     @ 3, 1 TO 3, WCOLS() DOUBLE
     @ 4, 1 SAY  ;
       'Presione Esc para cancelar la operaciขn'
     READ
     IF LASTKEY() = 27
          RELEASE WINDOW sclr
          CLOSE DATABASES
          RETURN
     ENDIF
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
            mapo.status <> 'X'  ;
            INTO CURSOR query
     INDEX ON rep_legal + cedula  ;
           TAG rep
     SET ORDER TO rep
     GOTO TOP
     pag = 0
     li = 89
     SET DEVICE TO PRINTER
     m.cuantos = 0
     m.sec = m.codigo + m.seccion
     swp = 0
     DO WHILE  .NOT. EOF()
          IF status = 'X'
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
               @ 04, 00 SAY  ;
                 'ีอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ'
               @ 05, 01 SAY  ;
                 'ณ REPRESENTANTE          CEDULA     ALUMNOS                                   ณ'
               @ 07, 01 SAY  ;
                 'ิอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ'
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
                 78)
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
                 78)
          ENDIF
          SKIP
     ENDDO
     @ li, 01 SAY REPLICATE('-',  ;
       78)
     li = li + 1
     SET DEVICE TO SCREEN
     m.fecha = DATE()
     USE
ENDDO
CLOSE DATABASES
RELEASE WINDOW
RETURN
*

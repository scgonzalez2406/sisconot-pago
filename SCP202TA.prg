CLOSE DATABASES
SET DEVICE TO SCREEN
SELECT 1
USE datos AGAIN ALIAS datos ORDER  ;
    cedula
SELECT 2
USE contdeud AGAIN ALIAS contdeud  ;
    ORDER cedula
DEFINE WINDOW sclr FROM 10, 10 TO  ;
       16, 70 FLOAT NOCLOSE  ;
       SHADOW TITLE  ;
       'Estado de Cuenta por Concepto Deudas  (SCP202TA)'  ;
       NOMINIMIZE COLOR SCHEME 5
ACTIVATE WINDOW sclr
m.fecha = DATE()
titulo3 = 'ESTADO DE CUENTA DE REGISTRO DE DEUDAS'
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
       'Presione Esc para cancelar la operaci¢n'
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
     SELECT 2
     GOTO TOP
     STORE 0 TO xtotal1, xtotal2,  ;
           xtotal3, pag,  ;
           m.cuantos
     li = 89
     swp = 0
     SET DEVICE TO PRINTER
     DO WHILE  .NOT. EOF()
          SELECT 2
          IF total_d = 0
               SELECT 2
               SKIP
               LOOP
          ENDIF
          IF total_d = 0
               SELECT 2
               SKIP
               LOOP
          ENDIF
          m.saldo = total_d -  ;
                    monto_p
          IF m.saldo = 0
               SELECT 2
               SKIP
               LOOP
          ENDIF
          xcedula = cedula
          SELECT 1
          IF  .NOT. SEEK(xcedula)
               SELECT 2
               SKIP
               LOOP
          ENDIF
          IF li > 58
               m.cuantos = 0
               pag = pag + 1
               @ 00, 00 SAY  ;
                 CHR(18) +  ;
                 CHR(20) + '  ' +  ;
                 noraya
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
               @ 02, 00 SAY  ;
                 CHR(18) +  ;
                 CHR(20) + ' '
               @ 05, 00 SAY  ;
                 '==============================================+==========+==========+=========+'
               @ 06, 00 SAY  ;
                 '  CEDULA  | APELLIDOS/NOMBRES                 | T.DEUDA  | T.PAGADO |  SALDO  |'
               @ 07, 00 SAY  ;
                 '----------------------------------------------+----------+----------+---------+'
               li = 08
          ENDIF
          swp = 1
          @ li, 00 SAY cedula  ;
            PICTURE '!-99999999'
          @ li, 11 SAY nombres  ;
            PICTURE '@!s33'
          SELECT 2
          @ li, 47 SAY total_d  ;
            PICTURE '999,999.99'
          @ li, 58 SAY monto_p  ;
            PICTURE '999,999.99'
          @ li, 69 SAY m.saldo  ;
            PICTURE '999,999.99'
          li = li + 1
          IF li > 58
               @ li, 00 SAY  ;
                 REPLICATE('-',  ;
                 77)
          ENDIF
          xtotal1 = xtotal1 +  ;
                    total_d
          xtotal2 = xtotal2 +  ;
                    monto_p
          xtotal3 = xtotal3 +  ;
                    m.saldo
          m.cuantos = m.cuantos +  ;
                      1
          SELECT 2
          SKIP
     ENDDO
     IF swp = 1
          @ li, 00 SAY  ;
            REPLICATE('-', 77)
          li = li + 1
          @ li, 01 SAY  ;
            'TOTALES ->'
          @ li, PCOL() SAY  ;
            m.cuantos PICTURE  ;
            '999'
          @ li, 47 SAY xtotal1  ;
            PICTURE '999,999.99'
          @ li, 58 SAY xtotal2  ;
            PICTURE '999,999.99'
          @ li, 69 SAY xtotal3  ;
            PICTURE '999,999.99'
          li = li + 1
          @ li, 00 SAY CHR(18) +  ;
            CHR(20) + '  '
     ELSE
          SET DEVICE TO SCREEN
          = msgerro( ;
            'No Existen Alumnos con registro de deudas, verifique' ;
            )
     ENDIF
     SET DEVICE TO SCREEN
     m.fecha = DATE()
ENDDO
CLOSE DATABASES
RELEASE WINDOW sclr
RETURN
*

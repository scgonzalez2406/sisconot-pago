CLOSE DATABASES
SET DEVICE TO SCREEN
SELECT 1
USE lapso1
m.lapso = lapso1
USE datos
SET ORDER TO cedula
SELECT 2
USE regpagos
SET ORDER TO fechapago
SELECT 3
USE bancos AGAIN ALIAS bancos  ;
    ORDER codigo
DEFINE WINDOW sclr FROM 10, 10 TO  ;
       17, 70 FLOAT NOCLOSE  ;
       SHADOW TITLE  ;
       'Relaciขn de Depositos (SCP30006)'  ;
       NOMINIMIZE COLOR SCHEME 5
ACTIVATE WINDOW sclr
m.fecha1 = DATE()
m.fecha2 = DATE()
titulo3 = 'RELACION GENERAL DE DEPOSITOS'
xbanco = '  '
SAVE SCREEN TO jol
DO WHILE .T.
     RESTORE SCREEN FROM jol
     @ 1, 1 SAY 'Banco: ' GET  ;
       xbanco PICTURE '@!'
     @ 2, 1 SAY 'Fecha Desde : '  ;
       GET m.fecha1 PICTURE '@E'
     @ 3, 1 SAY 'Fecha Hasta : '  ;
       GET m.fecha2 PICTURE '@E'
     @ 4, 1 TO 4, WCOLS() DOUBLE
     @ 5, 1 SAY  ;
       'Presione Esc para cancelar la operaciขn'
     READ
     IF LASTKEY() = 27
          RELEASE WINDOW sclr
          CLOSE DATABASES
          RETURN
     ENDIF
     IF xbanco <> SPACE(2)
          SELECT 3
          IF  .NOT. SEEK(xbanco)
               xbanco = boxfield('bancos', ;
                        'codigo')
          ENDIF
          xdesban = ALLTRIM(descripcio)
          @ 1, 1 SAY 'Banco: '  ;
            GET xbanco PICTURE  ;
            '@!'
          @ 1, COL() + 2 GET  ;
            xdesban PICTURE  ;
            '@!s25'
          CLEAR GETS
     ENDIF
     IF m.fecha1 > m.fecha2
          = msgerro( ;
            'El Lapso para el analisis es invalido, verifique' ;
            )
          LOOP
     ENDIF
     archivo = SYS(3) + '.TXT'
     set print to &archivo
     titulo4 = 'Desde : ' +  ;
               DTOC(m.fecha1) +  ;
               ' Hasta : ' +  ;
               DTOC(m.fecha2)
     SELECT 2
     GOTO TOP
     pag = 0
     li = 89
     SET DEVICE TO PRINTER
     m.cuantos = 0
     xtotald = 0
     DO WHILE  .NOT. EOF()
          IF status = 'D'
               SELECT 2
               SKIP
               LOOP
          ENDIF
          IF fecha_pago <  ;
             m.fecha1
               SELECT 2
               SKIP
               LOOP
          ENDIF
          IF fecha_pago >  ;
             m.fecha2
               SELECT 2
               SKIP
               LOOP
          ENDIF
          IF tipo_pago <> 'D'
               SELECT 2
               SKIP
               LOOP
          ENDIF
          IF xbanco <> SPACE(2)
               IF cod_banco <>  ;
                  xbanco
                    SELECT 2
                    SKIP
                    LOOP
               ENDIF
          ENDIF
          IF li > 55
               pag = pag + 1
               @ 00, 00 SAY  ;
                 l_nombre
               @ 00, 62 SAY  ;
                 'PAGINA: '
               @ 00, PCOL() SAY  ;
                 pag PICTURE  ;
                 '9999'
               @ 01, 60 SAY  ;
                 'FECHA : ' +  ;
                 DTOC(DATE())
               @ 01, 00 SAY  ;
                 sistema
               @ 02, 00 SAY  ;
                 SPACE(((80 -  ;
                 LEN(titulo3) -  ;
                 6) / 2) + 3) +  ;
                 titulo3
               @ 03, 00 SAY  ;
                 SPACE(((80 -  ;
                 LEN(titulo4) -  ;
                 6) / 2) + 3) +  ;
                 titulo4
               IF xbanco <>  ;
                  SPACE(2)
                    @ 04, 00 SAY  ;
                      'BANCO : ' +  ;
                      xbanco +  ;
                      '  ' +  ;
                      xdesban
               ENDIF
               @ 05, 00 SAY  ;
                 'ีออออออออออออัอออออออออออออออออออออออออออัออออออออออัอออออออออออัอออออออออออัออธ'
               @ 06, 00 SAY  ;
                 'ณ Nง.DEPOST. ณ ALUMNO                    ณFECHA PAGOณ Nง RECIBO ณ   MONTO   ณSTณ'
               @ 07, 00 SAY  ;
                 'ิออออออออออออฯอออออออออออออออออออออออออออฯออออออออออฯอออออออออออฯอออออออออออฯออพ'
               li = 08
          ENDIF
          xcedula = cedula
          SELECT 1
          IF  .NOT. SEEK(xcedula)
               xnombre = 'SIN NOMBRE'
          ELSE
               xnombre = ALLTRIM(nombres)
          ENDIF
          SELECT 2
          @ li, 01 SAY  ;
            ALLTRIM(nro_depo)  ;
            PICTURE '@!s13'
          @ li, 14 SAY xnombre  ;
            PICTURE '@!s22'
          @ li, 39 SAY seccion  ;
            PICTURE '@!'
          @ li, 42 SAY fecha_pago  ;
            PICTURE '@E'
          @ li, 53 SAY  ;
            ALLTRIM(STR(recibo))
          IF status = 'D'
               @ li, 77 SAY 'NU'
          ELSE
               @ li, 66 SAY monto  ;
                 PICTURE  ;
                 '999,999.99'
               xtotald = xtotald +  ;
                         monto
          ENDIF
          m.cuantos = m.cuantos +  ;
                      1
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
     @ li, 01 SAY  ;
       'Total Depositos: '
     @ li, PCOL() SAY m.cuantos  ;
       PICTURE '999'
     @ li, 62 SAY xtotald PICTURE  ;
       '99,999,999.99'
     @ li, 01 SAY REPLICATE('=',  ;
       78)
     SET DEVICE TO SCREEN
     m.fecha = DATE()
     SET PRINTER TO
     SET SYSMENU ON
     KEYBOARD '"{CTRL+F10}'
     modi comm &archivo.txt noedit
     SET SYSMENU OFF
     dele file &archivo
ENDDO
CLOSE DATABASES
RELEASE WINDOW sclr
RETURN
*

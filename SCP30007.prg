CLOSE DATABASES
SET DEVICE TO SCREEN
SELECT 1
USE lapso1
m.lapso = lapso1
SELECT 2
USE ingresos
SET ORDER TO fechapago
SELECT 3
USE concepto AGAIN ALIAS concepto  ;
    ORDER codigo
SELECT 5
USE bancos AGAIN ALIAS bancos  ;
    ORDER codigo
SELECT 4
USE cobrador AGAIN ALIAS cobrador  ;
    ORDER codigo
DEFINE WINDOW sclr FROM 10, 10 TO  ;
       17, 70 FLOAT NOCLOSE  ;
       SHADOW TITLE  ;
       'Relaciขn de Cobros por Otros Ingresos (SCP40007)'  ;
       NOMINIMIZE COLOR SCHEME 5
ACTIVATE WINDOW sclr
m.fecha1 = DATE()
m.fecha2 = DATE()
titulo3 = 'RELACION GENERAL DE COBROS POR OTROS INGRESOS'
xbanco = '  '
SAVE SCREEN TO jol
DO WHILE .T.
     RESTORE SCREEN FROM jol
     @ 1, 1 SAY 'Concepto : ' GET  ;
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
     SELECT 3
     IF  .NOT. SEEK(xbanco)
          xbanco = boxfield('concepto', ;
                   'codigo')
     ENDIF
     xdesban = ALLTRIM(descripcio)
     @ 1, 1 SAY 'Concepto : ' GET  ;
       xbanco PICTURE '@!'
     @ 1, COL() + 2 GET xdesban  ;
       PICTURE '@!s25'
     CLEAR GETS
     IF m.fecha1 > m.fecha2
          = msgerro( ;
            'El Lapso para el analisis es invalido, verifique' ;
            )
          LOOP
     ENDIF
     archivo = SYS(3) + '.txt'
     set print to &archivo
     titulo4 = 'DESDE : ' +  ;
               DTOC(m.fecha1) +  ;
               ' HASTA : ' +  ;
               DTOC(m.fecha2)
     SELECT 2
     pag = 0
     li = 89
     SET DEVICE TO PRINTER
     SELECT 2
     STORE 0 TO diario, contado,  ;
           credito, efectivo,  ;
           cheques, pag, swp,  ;
           xtotal
     STORE 0 TO devcred, devcont,  ;
           paso
     DO WHILE  .NOT. EOF()
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
          IF concepto <> xbanco
               SELECT 2
               SKIP
               LOOP
          ENDIF
          IF li > 55
               pag = pag + 1
               @ 00, 00 SAY  ;
                 l_nombre
               @ 00, 119 SAY  ;
                 'PAGINA: '
               @ 00, PCOL() SAY  ;
                 pag PICTURE  ;
                 '9999'
               @ 01, 119 SAY  ;
                 'FECHA : ' +  ;
                 DTOC(DATE())
               @ 01, 00 SAY  ;
                 sistema
               xd = 'RELACION DE COBROS POR CONCEPTOS'
               @ 02, 00 SAY  ;
                 SPACE(((140 -  ;
                 LEN(xd) - 6) /  ;
                 2) + 3) + xd
               @ 03, 00 SAY  ;
                 SPACE(((140 -  ;
                 LEN(titulo4) -  ;
                 6) / 2) + 3) +  ;
                 titulo4
               @ 05, 00 SAY  ;
                 'CONCEPTO : ' +  ;
                 xbanco + '  ' +  ;
                 xdesban
               @ 07, 00 SAY  ;
                 'ีอออออออออออัออออออออออออัออออออออออออัอออออออออออออออออัออออออออออออออออออออออออออัออออออออออออออัอออออออออออออออออออออออออออออออออออออัออธ'
               @ 08, 00 SAY  ;
                 'ณNro. RECIBOณ FECHA/PAGO ณ  TIPO/PAGO ณ Nro./Dep./Cheq. ณ BANCOS                   ณ    MONTO     ณ COBRADOR                            ณSTณ'
               @ 09, 00 SAY  ;
                 'ิอออออออออออฯออออออออออออฯออออออออออออฯอออออออออออออออออฯออออออออออออออออออออออออออฯออออออออออออออฯอออออออออออออออออออออออออออออออออออออฯออพ'
               li = 10
          ENDIF
          @ li, 01 SAY  ;
            ALLTRIM(STR(recibo))
          @ li, 13 SAY fecha_pago  ;
            PICTURE '@E'
          xdesba1 = SPACE(10)
          IF tipo_pago = 'E'
               @ li, 27 SAY  ;
                 'Efectivo'
          ELSE
               IF tipo_pago = 'D'
                    @ li, 27 SAY  ;
                      'Depositos'
               ENDIF
               IF tipo_pago = 'C'
                    @ li, 27 SAY  ;
                      'Cheques'
               ENDIF
               xcod = cod_banco
               SELECT 5
               IF  .NOT.  ;
                   SEEK(xcod)
                    xdesba1 = 'SIN NOMBRE'
               ELSE
                    xdesba1 = ALLTRIM(descripcio)
               ENDIF
          ENDIF
          xcob = cobrador
          SELECT 4
          IF  .NOT. SEEK(xcob)
               xcob = 'SIN NOMBRE'
          ELSE
               xcob = ALLTRIM(nombres)
          ENDIF
          SELECT 2
          @ li, 40 SAY  ;
            ALLTRIM(nro_depo)
          @ li, 51 SAY xdesba1  ;
            PICTURE '@!S24'
          @ li, 84 SAY monto  ;
            PICTURE  ;
            '99,999,999.99'
          @ li, 100 SAY xcob  ;
            PICTURE '@!S25'
          contado = contado +  ;
                    monto
          li = li + 1
          IF li > 55
               @ li, 01 SAY  ;
                 REPLICATE('-',  ;
                 139)
          ENDIF
          SKIP
     ENDDO
     @ li, 01 SAY REPLICATE('-',  ;
       139)
     li = li + 1
     @ li, 01 SAY  ;
       'Total Relaciขn de Cobranza: '
     @ li, PCOL() SAY contado  ;
       PICTURE '99,999,999.99'
     li = li + 1
     @ li, 01 SAY REPLICATE('=',  ;
       139)
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

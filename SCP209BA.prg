SELECT 1
USE bancos AGAIN ALIAS bancos  ;
    ORDER codigo
SELECT 2
USE datos AGAIN ALIAS datos ORDER  ;
    cedula
SELECT 3
USE cobrador AGAIN ALIAS cobrador  ;
    ORDER codigo
SELECT 5
USE ingresos AGAIN ALIAS ingresos  ;
    ORDER fechapago
SELECT 4
USE regpagos AGAIN ALIAS regpagos  ;
    ORDER fecrecib
archivo = SYS(3) + '.TXT'
set print to &archivo
SET DEVICE TO PRINTER
li = 90
cuenta = 0
DO WHILE  .NOT. EOF()
     IF entrega = 'S'
          SELECT 4
          SKIP
          LOOP
     ENDIF
     IF li > 55
          pag = pag + 1
          @ 0, 00 SAY l_nombre
          @ 0, 120 SAY 'PAGINA: '
          @ 0, PCOL() SAY pag  ;
            PICTURE '999'
          @ 1, 00 SAY  ;
            'CONTROL DE PAGO'
          @ 1, 120 SAY 'FECHA : '
          @ 1, PCOL() SAY DATE()  ;
            PICTURE '@E'
          @ 2, 120 SAY 'HORA  : '
          @ 2, PCOL() SAY TIME()
          xd = 'RELACION DE ENTREGA DE CAJA'
          @ 03, 00 SAY SPACE(((80 -  ;
            LEN(xd) - 6) / 2) +  ;
            3) + xd
          xd = 'FECHA DE PROCESO: ' +  ;
               DTOC(m.fecha1)
          @ 04, 00 SAY  ;
            SPACE(((140 - LEN(xd) -  ;
            6) / 2) + 3) + xd
          @ 07, 00 SAY  ;
            'ีออออออออัออออออออออออัออออออออออออัอออออออออออออัอออออออออออออออัออออออออออออออออออออออัอออออออออออออออออออออออออัอออออออออออออออัออออธ'
          @ 08, 00 SAY  ;
            'ณ    Nง  ณ     Nง     ณ    Fecha   ณ    Tipo     ณ       Nง      ณ                      ณ                         ณ     Monto     ณ    ณ'
          @ 09, 00 SAY  ;
            'ณ Recibo ณ   Cedula   ณ   de Pago  ณ   de Pago   ณ    Dep/Cheq   ณ Banco                ณ Cobrador                ณ    Cobrado    ณ TT ณ'
          @ 10, 00 SAY  ;
            'ิออออออออฯออออออออออออฯออออออออออออฯอออออออออออออฯอออออออออออออออฯออออออออออออออออออออออฯอออออออออออออออออออออออออฯอออออออออออออออฯออออพ'
          li = 11
     ENDIF
     cuenta = cuenta + 1
     @ li, 01 SAY  ;
       LTRIM(STR(recibo))
     @ li, 11 SAY ALLTRIM(cedula)
     @ li, 24 SAY fecha_pago  ;
       PICTURE '@E'
     IF status = 'D'
          @ li, 38 SAY  ;
            'R E C I B O   A N U L A D O'
     ELSE
          IF tipo_pago = 'E'
               @ li, 38 SAY  ;
                 'EFECTIVO '
               contado = contado +  ;
                         monto
          ELSE
               IF tipo_pago = 'D'
                    @ li, 38 SAY  ;
                      'DEPOSITO'
                    deposito = deposito +  ;
                               monto
               ENDIF
               IF tipo_pago = 'C'
                    @ li, 38 SAY  ;
                      'CHEQUE'
                    cheques = cheques +  ;
                              monto
               ENDIF
               @ li, 51 SAY  ;
                 ALLTRIM(nro_depo)
               xbanco = cod_banco
               SELECT 1
               IF  .NOT.  ;
                   SEEK(xbanco)
                    xdesban = 'SIN NOMBRE'
               ELSE
                    xdesban = ALLTRIM(descripcio)
               ENDIF
               SELECT 4
               @ li, 67 SAY  ;
                 xdesban
          ENDIF
          xcobrador = cobrador
          SELECT 3
          IF  .NOT.  ;
              SEEK(xcobrador)
               xdescobr = 'SIN NOMBRE'
          ELSE
               xdescobr = ALLTRIM(nombres)
          ENDIF
          SELECT 4
          @ li, 90 SAY xdescobr
          @ li, 116 SAY monto  ;
            PICTURE  ;
            '99,999,999.99'
          @ li, 132 SAY transa
          diario = diario + monto
     ENDIF
     li = li + 1
     IF li > 55
          @ li, 01 SAY  ;
            REPLICATE('-', 138)
          @ li, 00 SAY SPACE(150)
          @ li, 00 SAY SPACE(150)
          @ li, 00 SAY SPACE(150)
          @ li, 00 SAY SPACE(150)
          @ li, 00 SAY SPACE(150)
     ENDIF
     SELECT 4
     SKIP
ENDDO
SELECT 5
SET ORDER TO fechapago
GOTO TOP
IF SEEK(DTOS(m.fecha1))
     DO WHILE  .NOT. EOF()
          IF entrega = 'S'
               SELECT 5
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
               @ 00, 58 SAY  ;
                 CHR(18) +  ;
                 CHR(20) + ' '
               @ 00, 58 SAY  ;
                 'PAGINA: '
               @ 0, PCOL() SAY  ;
                 pag PICTURE  ;
                 '999'
               @ 1, 00 SAY  ;
                 'CONTROL DE PAGO'
               @ 1, 58 SAY  ;
                 'FECHA : '
               @ 1, PCOL() SAY  ;
                 DATE() PICTURE  ;
                 '@E'
               @ 2, 58 SAY  ;
                 'HORA  : '
               @ 2, PCOL() SAY  ;
                 TIME()
               xd = 'RELACION DE ENTREGA DE CAJA'
               @ 03, 00 SAY  ;
                 SPACE(((80 -  ;
                 LEN(xd) - 6) /  ;
                 2) + 3) + ng +  ;
                 xd + ngn
               xd = 'FECHA DE PROCESO: ' +  ;
                    DTOC(m.fecha1)
               @ 04, 00 SAY  ;
                 SPACE(((80 -  ;
                 LEN(xd) - 6) /  ;
                 2) + 3) + ng +  ;
                 xd + ngn
               @ 5, 0 SAY CHR(15) +  ;
                 ' '
               @ 07, 00 SAY  ;
                 'ีออออออออัออออออออออออัออออออออออออัอออออออออออออัอออออออออออออออัออออออออออออออออออออออัอออออออออออออออออออออออออัอออออออออออออออัออออธ'
               @ 08, 00 SAY  ;
                 'ณ    Nง  ณ     Nง     ณ    Fecha   ณ    Tipo     ณ       Nง      ณ                      ณ                         ณ     Monto     ณ    ณ'
               @ 09, 00 SAY  ;
                 'ณ Recibo ณ   Cedula   ณ   de Pago  ณ   de Pago   ณ    Dep/Cheq   ณ Banco                ณ Cobrador                ณ    Cobrado    ณ TT ณ'
               @ 10, 00 SAY  ;
                 'ิออออออออฯออออออออออออฯออออออออออออฯอออออออออออออฯอออออออออออออออฯออออออออออออออออออออออฯอออออออออออออออออออออออออฯอออออออออออออออฯออออพ'
               li = 11
          ENDIF
          paso = 1
          cuenta = cuenta + 1
          @ li, 01 SAY  ;
            LTRIM(STR(recibo))
          @ li, 24 SAY fecha_pago  ;
            PICTURE '@E'
          IF status = 'D'
               @ li, 38 SAY  ;
                 'R E C I B O   A N U L A D O'
          ELSE
               IF tipo_pago = 'E'
                    @ li, 38 SAY  ;
                      'EFECTIVO '
                    contado = contado +  ;
                              monto
               ELSE
                    IF tipo_pago =  ;
                       'D'
                         @ li, 38  ;
                           SAY  ;
                           'DEPOSITO'
                         deposito =  ;
                          deposito +  ;
                          monto
                    ENDIF
                    IF tipo_pago =  ;
                       'C'
                         @ li, 38  ;
                           SAY  ;
                           'CHEQUE'
                         cheques =  ;
                          cheques +  ;
                          monto
                    ENDIF
                    xbanco = cod_banco
                    SELECT 1
                    IF  .NOT.  ;
                        SEEK(xbanco)
                         xdesban =  ;
                          'SIN NOMBRE'
                    ELSE
                         xdesban =  ;
                          ALLTRIM(descripcio)
                    ENDIF
                    SELECT 5
                    @ li, 67 SAY  ;
                      xdesban
               ENDIF
               xcobrador = cobrador
               SELECT 3
               IF  .NOT.  ;
                   SEEK(xcobrador)
                    xdescobr = 'SIN NOMBRE'
               ELSE
                    xdescobr = ALLTRIM(nombres)
               ENDIF
               SELECT 5
               @ li, 90 SAY  ;
                 xdescobr
               @ li, 116 SAY  ;
                 monto PICTURE  ;
                 '99,999,999.99'
               @ li, 132 SAY  ;
                 transa
               diario = diario +  ;
                        monto
          ENDIF
          SELECT 5
          SKIP
     ENDDO
ENDIF
@ li, 01 SAY REPLICATE('-', 138)
li = li + 1
@ li, 01 SAY 'EFECTIVO      :'
@ li, 18 SAY contado PICTURE  ;
  '99,999,999.99'
li = li + 1
@ li, 01 SAY 'DEPOSITOS     :'
@ li, 18 SAY deposito PICTURE  ;
  '99,999,999.99'
li = li + 1
@ li, 01 SAY 'CHEQUES       :'
@ li, 18 SAY cheques PICTURE  ;
  '99,999,999.99'
li = li + 1
@ li, 01 SAY 'TOTAL COBRANZA:'
@ li, 18 SAY diario PICTURE  ;
  '99,999,999.99'
li = li + 1
@ li, PCOL() SAY '  ' + ngn
SET DEVICE TO SCREEN
IF cuenta = 0
     = msgerro( ;
       'No existen Relaciขn de Cobranza para esta Fecha, verifique' ;
       )
ELSE
     SET PRINTER TO
     SET SYSMENU ON
     KEYBOARD '"{CTRL+F10}'
     modi comm &archivo.txt noedit
     SET SYSMENU OFF
     dele file &archivo
ENDIF
RETURN
*

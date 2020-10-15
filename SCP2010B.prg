CLOSE DATABASES
SET DEVICE TO SCREEN
CLOSE DATABASES
SET DELETED ON
SET TALK OFF
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
SELECT 8
USE plancobr AGAIN ALIAS plancobr  ;
    ORDER lapcod
SELECT 11
USE detacobr AGAIN ALIAS detacobr  ;
    ORDER lapcod
SELECT 12
USE concepto AGAIN ALIAS concepto  ;
    ORDER codigo
SELECT 13
USE liceo
SELECT 14
USE cobrador AGAIN ALIAS cobrador  ;
    ORDER codigo
SELECT 9
USE ingresos AGAIN ALIAS ingresos  ;
    ORDER recibo
SELECT 10
USE bancos AGAIN ALIAS bancos  ;
    ORDER codigo
DEFINE WINDOW sclr FROM 10, 10 TO  ;
       17, 70 FLOAT NOCLOSE  ;
       SHADOW TITLE  ;
       'Recibos de Cobros sobre Otros Ingresos (SCP2010A)'  ;
       NOMINIMIZE COLOR SCHEME 5
ACTIVATE WINDOW sclr
SAVE SCREEN TO jol
xrecibo = 0
DO WHILE .T.
     RESTORE SCREEN FROM jol
     @ 1, 1 SAY  ;
       'Nฃmero de Recibo : ' GET  ;
       xrecibo PICTURE  ;
       '99999999'
     @ 3, 1 TO 3, WCOLS() DOUBLE
     @ 4, 1 SAY  ;
       'Presione Esc para cancelar la operaciขn'
     READ
     IF LASTKEY() = 27
          RELEASE WINDOW sclr
          CLOSE DATABASES
          RETURN
     ENDIF
     SELECT 9
     SET ORDER TO recibo
     IF  .NOT. SEEK(xrecibo)
          = msgerro( ;
            'No se ha Realizado ningun cobro Sobre Otros Ingresos con este recibo, verifuqe' ;
            )
          LOOP
     ENDIF
     IF status = 'D'
          = msgerro( ;
            'Este Recibo fue anulado, verifique' ;
            )
          LOOP
     ENDIF
     p = printer()
     IF p = 2
          RELEASE WINDOW
          CLOSE DATABASES
          RETURN
     ENDIF
     mrecibo = recibo
     mbeneficia = ALLTRIM(beneficiar)
     mfecha_pag = fecha_pago
     mmontop = 0.00 
     mnrode = nro_depo
     IF cod_banco <> SPACE(2)
          xcod = cod_banco
          SELECT 10
          IF  .NOT. SEEK(xcod)
               mdesban = 'SIN NOMBRES'
          ELSE
               mdesban = ALLTRIM(descripcio)
          ENDIF
     ELSE
          mdesban = SPACE(10)
     ENDIF
     SELECT 9
     IF tipo_pago = 'D'
          mtipop = 'Depositos'
     ENDIF
     IF tipo_pago = 'C'
          mtipop = 'Cheques'
     ENDIF
     IF tipo_pago = 'E'
          mtipop = 'Efectivo'
     ENDIF
     DO WHILE  .NOT. EOF()
          IF recibo <> mrecibo
               EXIT
          ENDIF
          mmontop = mmontop +  ;
                    monto
          SKIP
     ENDDO
     SEEK xrecibo
     SET DEVICE TO PRINTER
     @ 0, 1 SAY '  ' + CHR(27) +  ;
       CHR(67) + CHR(33)
     @ 00, 00 SAY CHR(18) +  ;
       CHR(20)
     @ 00, 00 SAY CHR(14) +  ;
       CHR(15) + titulo + CHR(18) +  ;
       CHR(20) + ' '
     @ 01, 00 SAY  ;
       'RIF .                                                          Nro.:          ' +  ;
       ng
     @ 01, 04 SAY l_rif
     @ 01, 67 SAY  ;
       LTRIM(STR(mrecibo))
     @ 02, 00 SAY CHR(15) + ' '
     @ 02, 00 SAY l_direccio
     @ 02, PCOL() SAY CHR(18) +  ;
       CHR(20) + ' '
     @ 02, 00 SAY  ;
       '                                                        FECHA PAGO:          ' +  ;
       ng
     @ 02, 67 SAY mfecha_pag  ;
       PICTURE '@E'
     @ 02, PCOL() SAY ngn
     @ 03, 25 SAY CHR(14) +  ;
       CHR(15) + 'RECIBO DE PAGO' +  ;
       CHR(18) + CHR(20) + ' '
     @ 04, 00 SAY  ;
       '                                                           ษอออออออออออออออป '
     @ 05, 00 SAY  ;
       '                                                        Bs.บ               บ ' +  ;
       ng
     @ 05, 61 SAY mmontop PICTURE  ;
       '9,999,999.99'
     @ 05, PCOL() SAY ngn
     @ 06, 00 SAY  ;
       '                                                           ศอออออออออออออออผ ' +  ;
       ng
     xcantidad = letras(mmontop)
     xcantidad = ALLTRIM(xcantidad)
     @ 07, 00 SAY  ;
       'Hemos recibido de: ' +  ;
       ng
     @ 07, PCOL() + 2 SAY  ;
       mbeneficia PICTURE '@!'
     @ 07, PCOL() SAY ngn
     @ 08, 00 SAY  ;
       '-------------------------------------------------------------------------------'
     @ 09, 00 SAY  ;
       ' CONCEPTO                                                         MONTO        '
     @ 10, 00 SAY  ;
       '==============================================================================='
     li = 11
     DO WHILE  .NOT. EOF()
          SELECT 9
          IF recibo <> mrecibo
               EXIT
          ENDIF
          xcon = concepto
          SELECT 12
          IF  .NOT. SEEK(xcon)
               xcon = 'SIN DESCRIPCION'
          ELSE
               xcon = ALLTRIM(descripcio)
          ENDIF
          SELECT 9
          @ li, 01 SAY xcon
          @ li, 62 SAY monto  ;
            PICTURE  ;
            '99,999,999.99'
          li = li + 1
          IF li > 21
               @ 0, 1 SAY '  ' +  ;
                 CHR(27) +  ;
                 CHR(67) +  ;
                 CHR(33)
               @ 00, 00 SAY  ;
                 CHR(18) +  ;
                 CHR(20)
               @ 00, 00 SAY  ;
                 CHR(14) +  ;
                 CHR(15) + titulo +  ;
                 CHR(18) +  ;
                 CHR(20) + ' '
               @ 01, 00 SAY  ;
                 'RIF .                                                          Nro.:          ' +  ;
                 ng
               @ 01, 04 SAY l_rif
               @ 01, 67 SAY  ;
                 LTRIM(STR(mrecibo))
               @ 02, 00 SAY  ;
                 CHR(15) + ' '
               @ 02, 00 SAY  ;
                 l_direccio
               @ 02, PCOL() SAY  ;
                 CHR(18) +  ;
                 CHR(20) + ' '
               @ 02, 00 SAY  ;
                 '                                                        FECHA PAGO:          ' +  ;
                 ng
               @ 02, 67 SAY  ;
                 mfecha_pag  ;
                 PICTURE '@E'
               @ 02, PCOL() SAY  ;
                 ngn
               @ 03, 25 SAY  ;
                 CHR(14) +  ;
                 CHR(15) +  ;
                 'RECIBO DE PAGO' +  ;
                 CHR(18) +  ;
                 CHR(20) + ' '
               @ 04, 00 SAY  ;
                 '                                                           ษอออออออออออออออป '
               @ 05, 00 SAY  ;
                 '                                                        Bs.บ               บ ' +  ;
                 ng
               @ 05, 61 SAY  ;
                 mmontop PICTURE  ;
                 '9,999,999.99'
               @ 05, PCOL() SAY  ;
                 ngn
               @ 06, 00 SAY  ;
                 '                                                           ศอออออออออออออออผ ' +  ;
                 ng
               xcantidad = letras(mmontop)
               xcantidad = ALLTRIM(xcantidad)
               @ 07, 00 SAY  ;
                 'Hemos recibido de: ' +  ;
                 ng
               @ 07, PCOL() + 2  ;
                 SAY mbeneficia  ;
                 PICTURE '@!'
               @ 07, PCOL() SAY  ;
                 ngn
               @ 08, 00 SAY  ;
                 '-------------------------------------------------------------------------------'
               @ 09, 00 SAY  ;
                 ' CONCEPTO                                                         MONTO        '
               @ 10, 00 SAY  ;
                 '==============================================================================='
               li = 11
          ENDIF
          SKIP
     ENDDO
     @ 21, 00 SAY  ;
       'ษอออออออออออออออออออออออออออออออหอออออออออออออออออออออออออออออออออออออออออออออป'
     @ 22, 00 SAY  ;
       'บ       M E N S A J E           บ  EFECTIVO   [ ]   CHEQ/DEPOST [ ]           บ'
     IF mtipop <> 'Efectivo'
          @ 22, 65 SAY 'X'
     ELSE
          @ 22, 47 SAY 'X'
     ENDIF
     @ 23, 00 SAY  ;
       'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ  NRO. :                                     บ'
     @ 23, 41 SAY mnrode PICTURE  ;
       '@!'
     @ 24, 00 SAY  ;
       'บ                               บ  BANCO:                                     บ'
     @ 24, 41 SAY mdesban
     @ 25, 00 SAY  ;
       'บ                               บ                                             บ'
     @ 26, 00 SAY  ;
       'บ                               บ                        ___________________  บ'
     @ 27, 00 SAY  ;
       'บ                               บ                           RECIBIDO POR:     บ'
     @ 28, 00 SAY  ;
       'ศอออออออออออออออออออออออออออออออสอออออออออออออออออออออออออออออออออออออออออออออผ' +  ;
       ng
     @ 29, 00 SAY  ;
       ' NOTA: Los Pagos Realizado no estan sujetos a devoluciones' +  ;
       ngn
     SET DEVICE TO SCREEN
     xrecibo = 0
ENDDO
CLOSE DATABASES
RELEASE WINDOW
RETURN
*
